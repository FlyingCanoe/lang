use std::collections::HashMap;
use std::mem;

use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

use crate::parser::{Ast, Expresion, OperationType, Statement, VariableID};

struct CodegenContext {
    variable_list: HashMap<VariableID, Value>,
    module: Module<SimpleJITBackend>,
    int: Type,
}

impl CodegenContext {
    fn new() -> Self {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module: Module<SimpleJITBackend> = Module::new(builder);
        let mut cranelift_ctx: Context = module.make_context();
        let int: Type = Type::int(64).unwrap();

        cranelift_ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(int));

        CodegenContext {
            variable_list: HashMap::new(),
            module,
            int,
        }
    }

    fn get_value(&self, id: VariableID) -> Value {
        *self.variable_list.get(&id).unwrap()
    }

    fn set_variable_value(&mut self, id: VariableID, value: Value) {
        self.variable_list.insert(id, value);
    }

    fn int(&self) -> Type {
        self.int
    }

    fn get_finalized_function(mut self, cranelift_context: &mut Context) -> fn() -> isize {
        let id = self
            .module
            .declare_function("eval", Linkage::Export, &cranelift_context.func.signature)
            .unwrap();
        self.module
            .define_function(
                id,
                cranelift_context,
                &mut codegen::binemit::NullTrapSink {},
            )
            .unwrap();
        self.module.clear_context(cranelift_context);
        self.module.finalize_definitions();
        let code = self.module.get_finalized_function(id);
        unsafe { mem::transmute::<_, fn() -> isize>(code) }
    }
}

impl Statement {
    fn translate(self, ctx: &mut CodegenContext, builder: &mut FunctionBuilder) {
        match self {
            Statement::LetStatement(identifier, expresion) => {
                let value = expresion.translate(ctx, builder);
                ctx.set_variable_value(identifier, value);
            }
        }
    }
}

impl Ast {
    pub fn run(self) -> isize {
        let func = self.build();
        func()
    }
    pub fn build(self) -> fn() -> isize {
        let mut ctx = CodegenContext::new();
        let mut cranelift_context = Context::new();
        cranelift_context
            .func
            .signature
            .returns
            .push(AbiParam::new(ctx.int()));
        let mut function_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut cranelift_context.func, &mut function_context);
        let block = builder.create_block();
        builder.switch_to_block(block);

        self.clone()
            .get_statement_list()
            .into_iter()
            .for_each(|statement| statement.translate(&mut ctx, &mut builder));

        let value = self.get_expresion().translate(&mut ctx, &mut builder);
        builder.ins().return_(&[value]);
        ctx.get_finalized_function(&mut cranelift_context)
    }
}

impl Expresion {
    fn translate(self, ctx: &mut CodegenContext, builder: &mut FunctionBuilder) -> Value {
        let int = ctx.int;
        match self {
            Expresion::Variable(id) => ctx.get_value(id),
            Expresion::Integer(num) => builder.ins().iconst(int, num as i64),
            Expresion::Operation(optype, terms) => {
                let (left_term, right_term) = *terms;
                let left_value = left_term.translate(ctx, builder);
                let right_value = right_term.translate(ctx, builder);
                translate_operation(optype, left_value, right_value, builder)
            }
        }
    }
}

fn translate_operation(
    optype: OperationType,
    left_value: Value,
    right_value: Value,
    builder: &mut FunctionBuilder,
) -> Value {
    match optype {
        OperationType::Addition => builder.ins().iadd(left_value, right_value),
        OperationType::Subtraction => builder.ins().isub(left_value, right_value),
        OperationType::Multiplication => builder.ins().imul(left_value, right_value),
        OperationType::Division => builder.ins().udiv(left_value, right_value),
    }
}
