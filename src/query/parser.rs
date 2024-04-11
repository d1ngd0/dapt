use super::lexor::Lexer;

const SELECT: &str = "SELECT";
const FROM: &str = "FROM";
const WHERE: &str = "WHERE";
const EQUAL: &str = "=";
const NEGATE: &str = "!";

struct Parser<'a> {
    lex: Lexer<'a>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(s: &'a str) -> Parser<'a> {
        Parser {
            lex: Lexer::from(s),
        }
    }
}

impl<'a> Parser<'a> {
    // pub fn parse_query(&mut self) -> Option<Node> {
    //     let tok = self.lex.token()?;

    //     match tok {
    //         SELECT => Some(Node::Select),
    //     }
    // }

    // pub fn parse_select(&mut self) -> Option<()> {
    //     let tok = self.lex.token()?;

    //     match tok {
    //         SELECT => todo!(),
    //         _ => None,
    //     }
    // }
}
