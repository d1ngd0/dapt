const TOKEN_KEY: char = '"';
const TOKEN_STRING_LITERAL: char = '\'';
const TOKEN_ESCAPE: char = '\\';
const TOKEN_EQUAL: char = '=';
const TOKEN_NEGATE: char = '!';
const TOKEN_GREATER: char = '>';
const TOKEN_LESS: char = '<';
const TOKEN_PARENTHESIS_OPEN: char = '(';
const TOKEN_PARENTHESIS_CLOSE: char = ')';

pub struct Lexer<'a> {
    path: &'a str,
    head: usize,
    escape_token: Option<char>,
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(path: &'a str) -> Lexer<'a> {
        Lexer {
            path,
            head: 0,
            escape_token: None,
        }
    }
}

impl<'a> Lexer<'a> {
    // consumed returns the content which has already been tokenized
    // this is useful for creating error messages that point to where we
    // last were. This will not show the any peak tokens.
    pub fn consumed(&self) -> &'a str {
        &self.path[..self.head]
    }

    // token returns the next token in the path. When there are no more tokens
    // it returns None. All tokens returns are references to the original string
    // meaning all escape characters are still present.
    pub fn token(&mut self) -> Option<&'a str> {
        let (tok, next_index) = self.full_next()?;
        self.head = next_index;
        Some(tok)
    }

    // peak returns the next token without moving the head forward
    pub fn peak(&mut self) -> Option<&'a str> {
        let (tok, _) = self.full_next()?;
        Some(tok)
    }

    // full_nest returns the next token, and stiches together tokens like >=
    //
    fn full_next(&mut self) -> Option<(&'a str, usize)> {
        if self.head >= self.path.len() {
            return None;
        }

        self.consume_whitespace();

        let (tok, next_index) = self.next(self.head)?;
        match tok {
            // by doing a little work in special cases we can make sure
            // we return whole tokens.
            "=" | ">" | "<" | "!" => {
                if let Some((tok, next_index)) = self.next(next_index) {
                    if tok == "=" {
                        return Some((&self.path[self.head..next_index], next_index));
                    }
                }
            }
            // default case just let it fall through
            _ => {}
        }

        Some((tok, next_index))
    }

    // whitespace_offset will return the number of bytes from
    // the header that are whitespace.
    fn consume_whitespace(&mut self) {
        let c = self.path[self.head..].chars();

        for char in c {
            if !char.is_whitespace() {
                break;
            }

            self.head += char.len_utf8();
        }
    }

    // next returns the next token without moving the head forward. Tokens viewed
    // this way will not show up in consumed until token is called. This internal
    // function does not stitch tokens together like >=. Use peak instead. This function
    // also assumes there is no whitespace at the head specified
    fn next(&mut self, head: usize) -> Option<(&'a str, usize)> {
        let c = self.path[head..].char_indices();
        let mut tok: Option<&str> = None;
        let mut next_index = head;
        let mut escape_next = false;

        let escape_all = match self.escape_token {
            None => false,
            Some(_) => true,
        };

        // important, for every path out of this loop you MUST consider what to
        // do with next_index given your context.
        'charloop: for (i, char) in c {
            if !escape_all && char.is_whitespace() {
                println!("{:?}", tok);
                break 'charloop;
            }

            // Any special characters should be listed here. They can all be escaped
            match char {
                TOKEN_EQUAL
                | TOKEN_LESS
                | TOKEN_NEGATE
                | TOKEN_GREATER
                | TOKEN_PARENTHESIS_OPEN
                | TOKEN_PARENTHESIS_CLOSE => {
                    // if the previous token was an escape token, we just want to add this to the
                    // existing token
                    if escape_next || escape_all {
                        next_index += char.len_utf8();
                        tok = Some(&self.path[head..next_index]);
                        escape_next = false;
                        continue 'charloop;
                    }

                    // check the token, if there is a value in there we consumed
                    // an identifier first, so we need to return that. We keep
                    // keep track of the previous_index so that tokens greater
                    // than a single byte can be used.
                    if let Some(_) = tok {
                        break 'charloop;
                    }

                    // otherwise we are starting with a token, so lets return that
                    next_index += char.len_utf8();
                    tok = Some(&self.path[head..next_index]);
                    break 'charloop;
                }
                // Drives escaping logic, this will escape the next character
                TOKEN_ESCAPE => {
                    // collect the escapes so we can return something if the input is "\\\\\\\"
                    tok = Some(&self.path[head..next_index]);
                    escape_next = true;
                }
                // If something wraps something, and you want to escape everything inside
                // you should put it here. The wrapping token will be returned as it's
                // own token, and then the contents will be returned as a single token
                // followed by another wrapping token. Can use \ to escape. Escaped strings
                // are returned as is (with their escaping characters) so we can return a
                // reference
                TOKEN_KEY | TOKEN_STRING_LITERAL => {
                    // if the previous character was an escape we can ignore
                    // the double quote
                    if escape_next {
                        escape_next = false;
                        next_index += char.len_utf8();
                        continue;
                    }

                    // make sure the ending token matches the starting token
                    // we may add more string wrappers in the future, besides
                    // these two.
                    if let Some(t) = self.escape_token {
                        if t != char {
                            next_index += char.len_utf8();
                            continue;
                        }
                    }

                    // return the wrapping token as it's own token.
                    if let None = tok {
                        next_index += char.len_utf8();
                        tok = Some(&self.path[head..next_index]);

                        // toggle escaping when we encounter an escape token
                        match self.escape_token {
                            None => self.escape_token = Some(char),
                            Some(_) => self.escape_token = None,
                        }

                        break 'charloop;
                    }

                    if let Some(_) = self.escape_token {
                        // after we turn it off collect the token, without
                        // grabbing the ending "
                        tok = Some(&self.path[head..next_index]);
                        break 'charloop;
                    }

                    // if we are just starting we want to collect the previous
                    // token if it exists
                    if let Some(_) = tok {
                        break 'charloop;
                    }
                }
                // Anything not special should be wrapped up here
                _ => {
                    escape_next = false;
                    next_index += char.len_utf8();
                    tok = Some(&self.path[head..next_index]);
                }
            }
        }

        if let Some(tok) = tok {
            Some((tok, next_index))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_consumed {
        ($path:expr, $num:expr, $consumed:expr) => {{
            let mut p = Lexer::from($path);

            let mut x = 0;
            loop {
                if x == $num {
                    break;
                }
                _ = p.token();
                x += 1;
            }

            assert_eq!(p.consumed(), $consumed)
        }};
    }

    macro_rules! test_lexor {
        ($path:expr, $($args:tt),*) => {{
            let mut p = Lexer::from($path);
            let mut toks = vec![];
            let expected: Vec<&str> = vec![$($args),*];

            let mut tok = p.token();
            while let Some(t) = tok {
                toks.push(t);
                tok = p.token();
            }

            assert_eq!(toks, expected)
        }};
    }

    #[test]
    fn test_lexor() {
        test_lexor!(
            "SELECT * from something where 'name' = \"happy\"",
            "SELECT",
            "*",
            "from",
            "something",
            "where",
            "'",
            "name",
            "'",
            "=",
            "\"",
            "happy",
            "\""
        );

        test_lexor!(
            "SELECT '~.\"key name\"' FROM 'namespace' WHERE '*[0].name'==5 AND 'chiken'!='egg'",
            "SELECT",
            "'",
            "~.\"key name\"",
            "'",
            "FROM",
            "'",
            "namespace",
            "'",
            "WHERE",
            "'",
            "*[0].name",
            "'",
            "==",
            "5",
            "AND",
            "'",
            "chiken",
            "'",
            "!=",
            "'",
            "egg",
            "'"
        );

        test_lexor!(
            r#"a


                   b
 c"#,
            "a",
            "b",
            "c"
        );

        test_lexor!("()", "(", ")");
    }

    #[test]
    fn test_consumed() {
        test_consumed!("SELECT * FROM chicken", 2, "SELECT *");
    }
}