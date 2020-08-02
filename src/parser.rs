use pest::error::Error;
use pest::Parser;

use pest::iterators::Pair;
use std::str::FromStr;

#[derive(Debug, Clone)]
/// A representation of a simple mathematical expresion. It is produce by the Expresion grammatical
/// rule witch can be find at the parser documentation [`parser`](fn.parser.html).
pub enum Expresion {
    ///Semantic: input {} -> Value; Return the value that as been bind to that variable. See [`Statement::Let`](enum.Statement.html#variant.Let)
    Variable(VariableID),
    ///Semantic: input {} -> Value; Return it value.
    Integer(u64),
    /// Semantic
    /// ---
    /// - Addition: input {Left, Right} -> Value; perform a wrapping addition.
    /// - Subtraction: input {Left, Right} -> Value; perform a wrapping subtraction.
    /// - Multiplication: input {Left, Right} -> Value; perform a wrapping multiplication.
    /// - Division:  input {Left, Right} -> Value; perform a unsigned integer division. divide Left by Right. Trap if Right = 0.
    Operation(OperationType, Box<(Expresion, Expresion)>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    /// Semantic: Input {Expr}; Bind the value of Expr to the variable.
    /// If more than one variable is declare with the same identifier (see grammar rule
    /// [Statement](fn.parser.html)), the variable witch is declare latter foreshadow the former.
    /// Any expresion witch use reference.
    LetStatement(VariableID, Expresion),
}

#[derive(Debug, Copy, Clone)]
pub enum OperationType {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct LangParser {}

#[derive(Debug, Clone)]
pub struct Ast {
    variable_list: VariableStore,
    statement_list: Vec<Statement>,
    expresion: Expresion,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
/// A opaque reference to a variable.
pub struct VariableID(usize);
#[derive(Debug, Clone)]
pub struct VariableStore(Vec<String>);

impl VariableStore {
    fn new() -> Self {
        VariableStore(Vec::new())
    }
    fn declare_variable(&mut self, identifier: String) -> VariableID {
        self.0.push(identifier);
        VariableID(self.0.len() - 1)
    }
    fn get_variable_id(&mut self, identifier: &str) -> VariableID {
        match self
            .0
            .iter()
            .enumerate()
            .find(|(_, ident)| identifier == ident.as_str())
        {
            Some((id, _)) => VariableID(id),
            None => {
                self.0.push(identifier.to_string());
                VariableID(self.0.len() - 1)
            }
        }
    }
    pub fn get_variable_identifier(&self, variable_id: VariableID) -> String {
        self.0[variable_id.0].clone()
    }
}

impl Ast {
    fn new(
        expresion: Expresion,
        statement_list: Vec<Statement>,
        variable_list: VariableStore,
    ) -> Self {
        Self {
            expresion,
            statement_list,
            variable_list,
        }
    }

    pub fn get_statement_list(self) -> Vec<Statement> {
        self.statement_list
    }

    pub fn get_expresion(self) -> Expresion {
        self.expresion
    }
}

/// Parse the given string slice with the following grammar producing a [`Ast`](struct.Ast.html).
/// This function utilize the grammar define in `grammar.pest`
pub fn parser(input: &str) -> Result<Ast, Error<Rule>> {
    try {
        let pair = LangParser::parse(Rule::Ast, input)?.next().unwrap();
        parse_ast(pair)
    }
}

fn parse_ast(input: Pair<Rule>) -> Ast {
    assert_eq!(input.as_rule(), Rule::Ast);

    let mut iter = input.into_inner();
    let mut variable_list = VariableStore::new();

    let statement_list: Vec<Statement> = iter
        .clone()
        .take_while(|pair| pair.as_rule() == Rule::Statement)
        .map(|pair| parse_statement(pair, &mut variable_list))
        .collect();

    let expression: Pair<Rule> = iter.find(|pair| pair.as_rule() == Rule::Expresion).expect("Internal parser error");

    assert_eq!(iter.next().unwrap().as_rule(), Rule::EOI);

    let expresion = parse_expresion(expression, &mut variable_list);
    Ast::new(expresion, statement_list, variable_list)
}

fn parse_statement(input: Pair<Rule>, variable_list: &mut VariableStore) -> Statement {
    assert_eq!(input.as_rule(), Rule::Statement);
    let mut pairs = input.into_inner();

    let identifier = pairs.next().unwrap().as_str().to_string();
    let expresion = parse_expresion(pairs.next().unwrap(), variable_list);

    pairs.next().unwrap_none();
    let id = variable_list.declare_variable(identifier);
    Statement::LetStatement(id, expresion)
}

fn parse_expresion(input: Pair<Rule>, variable_list: &mut VariableStore) -> Expresion {
    assert_eq!(input.as_rule(), Rule::Expresion);

    let pair = input.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::Operation => parse_operation(pair, variable_list),
        Rule::Term => parse_term(pair, variable_list),
        _ => unreachable!(),
    }
}

fn parse_operation(input: Pair<Rule>, variable_list: &mut VariableStore) -> Expresion {
    assert_eq!(input.as_rule(), Rule::Operation);
    let mut pairs = input.into_inner();

    let left_term = parse_term(pairs.next().unwrap(), variable_list);
    let operator = parse_operator(pairs.next().unwrap());
    let right_term = parse_expresion(pairs.next().unwrap(), variable_list);

    pairs.next().unwrap_none();
    Expresion::Operation(operator, Box::new((left_term, right_term)))
}

fn parse_operator(input: Pair<Rule>) -> OperationType {
    assert_eq!(input.as_rule(), Rule::Operator);

    match input.into_inner().next().unwrap().as_rule() {
        Rule::Addition => OperationType::Addition,
        Rule::Subtraction => OperationType::Subtraction,
        Rule::Multiplication => OperationType::Multiplication,
        Rule::Division => OperationType::Division,
        _ => unreachable!(),
    }
}

fn parse_term(input: Pair<Rule>, variable_list: &mut VariableStore) -> Expresion {
    assert_eq!(input.as_rule(), Rule::Term);

    let pair = input.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::Lateral => Expresion::Integer(u64::from_str(pair.as_str()).unwrap()),
        Rule::Identifier => Expresion::Variable(variable_list.get_variable_id(pair.as_str())),
        Rule::NestedExpresion => parse_expresion(pair.into_inner().next().unwrap(), variable_list),
        _ => unreachable!(),
    }
}

mod test {

    use super::{parser, Expresion, OperationType, Statement};

    #[test]
    fn statement_without_followup_expresion() {
        parser(
            "let x = 2 * 5\n\
                                        let y = x / 2",
        )
        .unwrap_err();
    }

    #[test]
    fn statement_without_newline() {
        parser("let x = 8 8*2").unwrap_err();
    }

    #[test]
    fn malformed_statement() {
        parser(
            "let x 5 * 2\n\
                                                x * 10",
        )
        .unwrap_err();
    }

    #[test]
    fn valid_operation() {
        let ast = parser("2 * 8").unwrap();
        if let Expresion::Operation(OperationType::Multiplication, terms) = ast.get_expresion() {
            if let (Expresion::Integer(2), Expresion::Integer(8)) = *terms {
                return;
            }
        }
        panic!()
    }

    #[test]
    fn expresion_with_undeclared_variable() {
        let ast = parser("x + 10").unwrap();
        if let Expresion::Operation(OperationType::Addition, terms) = ast.get_expresion() {
            if let (Expresion::Variable(_), Expresion::Integer(10)) = *terms {
                return;
            }
        }
        panic!()
    }

    #[test]
    fn valid_statement_follow_by_valid_expresion() {
        let ast = parser(
            "let xyz = 89\n\
                                    xyz / 5",
        )
        .unwrap();

        if let Statement::LetStatement(id, Expresion::Integer(89)) =
            ast.clone().get_statement_list().first().unwrap()
        {
            if let Expresion::Operation(OperationType::Division, terms) = ast.expresion {
                if let (Expresion::Variable(num), Expresion::Integer(5)) = *terms {
                    assert_eq!(*id, num);
                    return;
                }
            }
        }
        panic!()
    }

    #[test]
    fn nested_expresion() {
        let ast = parser("48 * (21 + 94)").unwrap();
        if let Expresion::Operation(OperationType::Multiplication, terms) = ast.get_expresion() {
            if let (Expresion::Integer(48), Expresion::Operation(OperationType::Addition, terms)) =
                *terms
            {
                if let (Expresion::Integer(21), Expresion::Integer(94)) = *terms {
                    return;
                }
            }
        }
        panic!()
    }

    #[test]
    fn variable_foreshadowing() {
        let ast = parser(
            "let x = 42 \n\
                                    let x = 32\n\
                                    x",
        )
        .unwrap();

        let mut iter = ast.clone().get_statement_list().into_iter();

        if let Some(Statement::LetStatement(true_id, _)) = iter.nth(1) {
            if let Expresion::Variable(id) = ast.get_expresion() {
                assert_eq!(true_id, id);
                return;
            }
        }
        panic!()
    }
}
