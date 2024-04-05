const TOKEN_DOT: char = '.';
const TOKEN_RECURSIVE: char = '~';
const TOKEN_ARRAY_OPEN: char = '[';
const TOKEN_ARRAY_CLOSE: char = ']';
const TOKEN_FIRST_OPEN: char = '{';
const TOKEN_FIRST_CLOSE: char = '}';
const TOKEN_FIRST_SEP: char = ',';
const TOKEN_MULTI_OPEN: char = '(';
const TOKEN_MULTI_CLOSE: char = ')';
const TOKEN_MULTI_SEP: char = '|';
const TOKEN_WILDCARD: char = '*';
const TOKEN_REGEX: char = '/';

const TOKEN_ESCAPE: char = '\\';
const TOKEN_STRING_WRAP: char = '"';

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
    pub fn token(&mut self) -> Option<&'a str> {
        if self.head >= self.path.len() {
            return None;
        }

        let c = self.path[self.head..].char_indices();
        let mut tok: Option<&str> = None;
        let mut next_index = self.head;
        let mut escape_next = false;

        'charloop: for (i, char) in c {
            let index = i + self.head;

            match char {
                TOKEN_DOT | TOKEN_WILDCARD | TOKEN_RECURSIVE | TOKEN_ARRAY_OPEN
                | TOKEN_ARRAY_CLOSE | TOKEN_FIRST_OPEN | TOKEN_FIRST_CLOSE | TOKEN_MULTI_OPEN
                | TOKEN_MULTI_CLOSE | TOKEN_FIRST_SEP | TOKEN_MULTI_SEP => {
                    // if the previous token was an escape token, we just want to add this to the
                    // existing token
                    let escape_all = match self.escape_token {
                        None => false,
                        Some(_) => true,
                    };

                    if escape_next || escape_all {
                        tok = Some(&self.path[self.head..index + char.len_utf8()]);
                        next_index = index + char.len_utf8();
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
                    tok = Some(&self.path[self.head..index + char.len_utf8()]);
                    next_index = index + char.len_utf8();
                    break 'charloop;
                }
                TOKEN_ESCAPE => {
                    // collect the escapes so we can return something if the input is "\\\\\\\"
                    tok = Some(&self.path[self.head..index + char.len_utf8()]);
                    escape_next = true;
                }
                TOKEN_STRING_WRAP | TOKEN_REGEX => {
                    // if the previous character was an escape we can ignore
                    // the double quote
                    if escape_next {
                        escape_next = false;
                        continue;
                    }

                    // make sure the ending token matches the starting token
                    // we may add more string wrappers in the future, besides
                    // these two.
                    if let Some(t) = self.escape_token {
                        if t != char {
                            continue;
                        }
                    }

                    // return the wrapping token as it's own token.
                    if let None = tok {
                        tok = Some(&self.path[self.head..index + char.len_utf8()]);
                        next_index = index + char.len_utf8();

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
                        tok = Some(&self.path[self.head..index]);
                        next_index = index;
                        break 'charloop;
                    }

                    // if we are just starting we want to collect the previous
                    // token if it exists
                    if let Some(_) = tok {
                        break 'charloop;
                    }
                }
                _ => {
                    escape_next = false;
                    tok = Some(&self.path[self.head..index + char.len_utf8()]);
                    next_index = index + char.len_utf8();
                }
            }
        }

        // move the head forward to the last seen index
        self.head = next_index;
        tok
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
        test_lexor!("Im.am a.fish", "Im", ".", "am a", ".", "fish");
        test_lexor!(" ", " ");
        test_lexor!(
            "labels.{hostname|host}",
            "labels",
            ".",
            "{",
            "hostname",
            "|",
            "host",
            "}"
        );
        test_lexor!("labels\\.hostname", "labels\\.hostname");
        test_lexor!(r#""labels.hostname""#, "\"", "labels.hostname", "\"");
        test_lexor!(r#""some\"thing""#, "\"", "some\\\"thing", "\"");
        test_lexor!(
            r#""one""two""three""four""#,
            "\"",
            "one",
            "\"",
            "\"",
            "two",
            "\"",
            "\"",
            "three",
            "\"",
            "\"",
            "four",
            "\""
        );
        test_lexor!(
            "€€€.€€€.€.asdf.asdf",
            "€€€",
            ".",
            "€€€",
            ".",
            "€",
            ".",
            "asdf",
            ".",
            "asdf"
        );
        test_lexor!("/.*/.something", "/", ".*", "/", ".", "something");
        test_lexor!("/asd\"asdf/", "/", "asd\"asdf", "/");
    }
}
