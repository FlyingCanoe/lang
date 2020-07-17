use std::collections::HashMap;
use std::mem;

use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

use crate::parser::{Ast, Statement};
use crate::{Expr, Operation};

#[derive(Clone)]
struct Context {
    variable_list: HashMap<String, Value>,
}

impl Context {
    fn new() -> Self {
        Context {
            variable_list: HashMap::new(),
        }
    }

    fn get_value(&self, identifier: &str) -> Value {
        *self.variable_list.get(identifier).unwrap()
    }

    fn set_varible(mut self, identifier: String, value: Value) -> Self {
        self.variable_list.insert(identifier, value);
        self
    }
}

impl Statement {
    fn translate(self, ctx: Context, builder: &mut FunctionBuilder, int: Type) -> Context {
        match self {
            Statement::LetStatement(identifier, expresion) => ctx
                .clone()
                .set_varible(identifier, expresion.translate(builder, int, &ctx)),
        }
    }
}

impl Ast {
    pub fn run(self) -> isize {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let mut module: Module<SimpleJITBackend> = Module::new(builder);
        let mut ctx = module.make_context();
        let int = module.target_config().pointer_type();

        let mut function_builder = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut function_builder);

        let entry_block = builder.create_block();

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let lang_ctx = self
            .clone()
            .get_statement_list()
            .into_iter()
            .fold(Context::new(), |ctx, statment| {
                statment.translate(ctx, &mut builder, int)
            });
        let value = self.get_expresion().translate(&mut builder, int, &lang_ctx);
        builder.ins().return_(&[value]);

        ctx.func.signature.returns.push(AbiParam::new(int));

        let id = module
            .declare_function("eval", Linkage::Export, &ctx.func.signature)
            .unwrap();
        module
            .define_function(id, &mut ctx, &mut codegen::binemit::NullTrapSink {})
            .unwrap();
        module.clear_context(&mut ctx);
        module.finalize_definitions();
        let code = module.get_finalized_function(id);
        let compile_fonction = unsafe { mem::transmute::<_, fn() -> isize>(code) };
        compile_fonction()
    }
}

impl Expr {
    fn translate(&self, builder: &mut FunctionBuilder, int: Type, ctx: &Context) -> Value {
        match self {
            Expr::Identifier(ident) => ctx.get_value(ident),
            Expr::Number(num) => builder.ins().iconst(int, *num as i64),
            Expr::Op(op) => match op.as_ref() {
                Operation::Add(term1, term2) => {
                    let value1 = term1.translate(builder, int, ctx);
                    let value2 = term2.translate(builder, int, ctx);
                    builder.ins().iadd(value1, value2)
                }
                Operation::Sub(term1, term2) => {
                    let value1 = term1.translate(builder, int, ctx);
                    let value2 = term2.translate(builder, int, ctx);
                    builder.ins().isub(value1, value2)
                }
                Operation::Mul(term1, term2) => {
                    let value1 = term1.translate(builder, int, ctx);
                    let value2 = term2.translate(builder, int, ctx);
                    builder.ins().imul(value1, value2)
                }
                Operation::Div(term1, term2) => {
                    let value1 = term1.translate(builder, int, ctx);
                    let value2 = term2.translate(builder, int, ctx);
                    builder.ins().sdiv(value1, value2)
                }
            },
        }
    }
}
