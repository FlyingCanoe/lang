use crate::lexer;
use crate::lexer::{lexer, Keyword, Operator, Token};

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
    Op(OperationType, Box<(Expresion, Expresion)>),
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

#[derive(Debug, Clone)]
pub struct Ast {
    variable_list: VariableStore,
    statement_list: Vec<Statement>,
    expresion: Expresion,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
/// A opaque reference to a variable.
pub struct  VariableID(usize);
#[derive(Debug, Clone)]
pub struct VariableStore(Vec<String>);

#[derive(Debug, Copy, Clone)]
pub enum Error {
    UnexpectedToken,
    UnexpectedEndOfFile,
    InvalidInteger,
    UnknownToken,
}

impl VariableStore {
    fn new() -> Self {
        VariableStore(Vec::new())
    }
    fn declare_variable(&mut self, identifier: String) -> VariableID {
        self.0.push(identifier);
        VariableID(self.0.len() - 1)
    }
    fn get_variable_id(&self, identifier: &str) -> Option<VariableID> {
        match self
            .0
            .iter()
            .enumerate()
            .find(|(_, ident)| identifier == ident.as_str())
        {
            Some((id, _)) => Some(VariableID(id)),
            None => None,
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
/// ***
/// Grammar
/// ---
/// - [`Ast`](struct.Ast.html) = (Statement)* ~ Expresion
///   - [`Statement`](enum.Statement.html) = let ~ identifier ~ = ~ Expresion ~ newline
///   - [`Expresion`](enum.Expresion.html) =  Operation | Term
///     - Operation = Term ~ Operator ~ Term
///     - Term = lateral | Identifier | NestedExpresion
///   - Operator = + | - | * | /
///   - NestedExpresion = ( ~ Expresion ~ )
/// ___
/// This grammar use the following [`Token`](../lexer/enum.Token.html). The documentation for the
/// tokenization of the code can be fond with the documentation of the [`lexer`](../lexer/fn.lexer.html).
/// - let = Token::Keyword(Keyword::Let)
/// - newline = Token::NewLine
/// - identifier = Token::Identifier
/// - lateral = Token::Integer
/// - \+ = Token::OpCode(Operator::Add)
/// - \- = Token::OpCode(Operator::Sub)
/// - \* = Token::OpCode(Operator::Mul)
/// - / = Token::OpCode(Operator::Div)
/// - ( = Token::LeftParenthesis
/// - ) = Token::RightParenthesis
pub fn parser(input: &str) -> Result<Ast, Error> {
    let token_list: &[Token] = &lexer(input)?;
    let mut variable_list = VariableStore::new();

    let (statement_list, reminder) = parse_statements(token_list, &mut variable_list)?;
    let (expresion, token_list) = parse_expresion(reminder, &mut variable_list)?;

    assert!(token_list.is_empty());
    Ok(Ast::new(expresion, statement_list, variable_list))
}

/// root = (Statement)*
fn parse_statements<'a>(
    token_list: &'a [Token],
    variable_list: &mut VariableStore,
) -> Result<(Vec<Statement>, &'a [Token]), Error> {
    match parse_statement(token_list, variable_list) {
        Ok((statement, reminder)) => {
            let (mut statement_list, reminder) = parse_statements(reminder, variable_list)?;
            statement_list.push(statement);
            Ok((statement_list, reminder))
        }
        Err(Error::UnexpectedToken) => Ok((vec![], token_list)),
        Err(other_error) => Err(other_error),
    }
}

/// root = let ~ Identifier ~ = ~ Expresion ~ \n
fn parse_statement<'a>(
    token_list: &'a [Token],
    variable_list: &mut VariableStore,
) -> Result<(Statement, &'a [Token]), Error> {
    let remainder = consume(Token::Keyword(Keyword::Let), token_list)?;
    match remainder.first().ok_or(Error::UnexpectedEndOfFile)? {
        Token::Identifier(identifier) => {
            let remainder = consume(Token::AssigmentSymbol, &remainder[1..])?;
            let (expr, remainder) = parse_expresion(remainder, variable_list)?;
            let remainder = consume(Token::NewLine, remainder)?;

            let id = variable_list.declare_variable(identifier.clone());
            Ok((Statement::LetStatement(id, expr), remainder))
        }
        _ => Err(Error::UnexpectedToken),
    }
}

/// root = Operation | Term
fn parse_expresion<'a>(
    token_list: &'a [Token],
    variable_list: &mut VariableStore,
) -> Result<(Expresion, &'a [Token]), Error> {
    match parse_operation(token_list, variable_list) {
        Ok(result) => Ok(result),
        Err(_) => parse_term(token_list, variable_list),
    }
}

/// - root = Term ~ Operator ~ Term
/// - Operator = "+" | "-" | "*" | "/"
fn parse_operation<'a>(
    token_list: &'a [Token],
    variable_list: &mut VariableStore,
) -> Result<(Expresion, &'a [Token]), Error> {
    let (left_term, remainder) = parse_term(token_list, variable_list)?;
    let operator: OperationType = match remainder.first().ok_or(Error::UnexpectedEndOfFile)? {
        Token::OpCode(operator) => *operator,
        _ => return Err(Error::UnexpectedToken),
    }
    .into();
    let (right_term, remainder) = parse_term(&remainder[1..], variable_list)?;
    Ok((
        Expresion::Op(operator, Box::new((left_term, right_term))),
        remainder,
    ))
}

/// - root = Lateral | Identifier | NestedExpresion
/// - NestedExpresion = "(" ~ Expresion ~ ")"
fn parse_term<'a>(
    token_list: &'a [Token],
    variable_list: &mut VariableStore,
) -> Result<(Expresion, &'a [Token]), Error> {
    match token_list.first().ok_or(Error::UnexpectedEndOfFile)? {
        Token::Integer(num) => Ok((Expresion::Integer(*num), &token_list[1..])),
        Token::LeftParenthesis => {
            let (expr, list) = parse_expresion(&token_list[1..], variable_list)?;
            Ok((expr, consume(Token::RightParenthesis, list)?))
        }
        Token::Identifier(ident) => Ok((
            Expresion::Variable(variable_list.get_variable_id(&ident).unwrap_or_else(||variable_list.declare_variable(ident.clone()))),
            &token_list[1..],
        )),
        _ => Err(Error::UnexpectedToken),
    }
}

fn consume(token_type: Token, input: &[Token]) -> Result<&[Token], Error> {
    if *input.first().ok_or(Error::UnexpectedEndOfFile)? == token_type {
        Ok(&input[1..])
    } else {
        Err(Error::UnexpectedToken)
    }
}

impl From<lexer::Error> for Error {
    fn from(err: lexer::Error) -> Self {
        match err {
            lexer::Error::UnknownToken => Error::UnknownToken,
            lexer::Error::InvalidInteger => Error::InvalidInteger,
        }
    }
}

impl From<Operator> for OperationType {
    fn from(op: Operator) -> Self {
        match op {
            Operator::Add => OperationType::Addition,
            Operator::Sub => OperationType::Subtraction,
            Operator::Mul => OperationType::Multiplication,
            Operator::Div => OperationType::Division,
        }
    }
}

mod test {

    use super::{
        parser, Expresion, OperationType, Statement, Error
    };

    #[test]
    fn statement_without_followup_expresion() {
        let error = parser("let x = 2 * 5\n\
                                        let y = x / 2").unwrap_err();
        match error {
            Error::UnexpectedEndOfFile => (),
            _ => panic!(),
        }
    }

    #[test]
    fn statement_without_newline() {
        let error = parser("let x = 8 8*2").unwrap_err();
        match error {
            Error::UnexpectedToken => (),
            _ => panic!(),
        }
    }

    #[test]
    fn malformed_statement() {
        let error = parser("let x 5 * 2\n\
                                                x * 10").unwrap_err();
        match error {
            Error::UnexpectedToken => (),
            _ => panic!(),
        }
    }

    #[test]
    fn valid_operation() {
        let ast = parser("2 * 8").unwrap();
        if let Expresion::Op(OperationType::Multiplication, terms) = ast.get_expresion() {
            if let (Expresion::Integer(2), Expresion::Integer(8)) = *terms {
                return;
            }
        }
        panic!()
    }

    #[test]
    fn expresion_with_undeclared_variable() {
        let ast = parser("x + 10").unwrap();
        if let Expresion::Op(OperationType::Addition, terms) = ast.get_expresion() {
            if let (Expresion::Variable(_), Expresion::Integer(10)) = *terms {
                return;
            }
        }
        panic!()
    }

    #[test]
    fn valid_statement_follow_by_valid_expresion() {
        let ast = parser("let xyz = 89\n\
                                    xyz / 5").unwrap();

        if let Statement::LetStatement(id, Expresion::Integer(89)) = ast.clone().get_statement_list().first().unwrap() {
           if let Expresion::Op(OperationType::Division, terms) = ast.expresion {
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
        if let Expresion::Op(OperationType::Multiplication, terms) = ast.get_expresion() {
            if let (Expresion::Integer(48), Expresion::Op(OperationType::Addition, terms)) = *terms {
                if let (Expresion::Integer(21), Expresion::Integer(94)) = *terms {
                    return;
                }
            }
        }
        panic!()
    }

    #[test]
    fn variable_foreshadowing() {
        let ast = parser("let x = 42 \n\
                                    let x = 32\n\
                                    x").unwrap();

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
