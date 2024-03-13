pub struct PathLexor<'a> {
    path: &'a str,
    head: usize,
}

pub fn new_path_lexor<'a>(path: &'a str) -> PathLexor<'a> {
    PathLexor { path, head: 0 }
}

impl<'a> PathLexor<'a> {
    pub fn token(&mut self) -> Option<&'a str> {
        // consume any whitespace, we don't care
        // about whitespace
        self.consume_whitespace();
        None
    }

    // consume_whitespace will consume any whitespace that exists in the key,
    // if you are a monster and create field values with whitespace in them
    // you will have to escape it or wrap it in double quotes. Also you
    // should see a shrink because god knows what other demented things you
    // are doing.
    fn consume_whitespace(&mut self) {
        let c = self.path.chars().skip(self.head);
        for char in c {
            match char {
                '\u{0009}' => self.head += 1, // character tabulation
                '\u{000A}' => self.head += 1, // line feed
                '\u{000B}' => self.head += 1, // line tabulation
                '\u{000C}' => self.head += 1, // form feed
                '\u{000D}' => self.head += 1, // carriage return
                '\u{0020}' => self.head += 1, // space
                '\u{0085}' => self.head += 1, // next line
                '\u{00A0}' => self.head += 1, // no-break space
                '\u{1680}' => self.head += 1, // ogham space mark
                '\u{180E}' => self.head += 1, // mongolian vowel separator
                '\u{2000}' => self.head += 1, // en quad
                '\u{2001}' => self.head += 1, // em quad
                '\u{2002}' => self.head += 1, // en space
                '\u{2003}' => self.head += 1, // em space
                '\u{2004}' => self.head += 1, // three-per-em space
                '\u{2005}' => self.head += 1, // four-per-em space
                '\u{2006}' => self.head += 1, // six-per-em space
                '\u{2007}' => self.head += 1, // figure space
                '\u{2008}' => self.head += 1, // punctuation space
                '\u{2009}' => self.head += 1, // thin space
                '\u{200A}' => self.head += 1, // hair space
                '\u{200B}' => self.head += 1, // zero width space
                '\u{200C}' => self.head += 1, // zero width non-joiner
                '\u{200D}' => self.head += 1, // zero width joiner
                '\u{2028}' => self.head += 1, // line separator
                '\u{2029}' => self.head += 1, // paragraph separator
                '\u{202F}' => self.head += 1, // narrow no-break space
                '\u{205F}' => self.head += 1, // medium mathematical space
                '\u{2060}' => self.head += 1, // word joiner
                '\u{3000}' => self.head += 1, // ideographic space
                '\u{FEFF}' => self.head += 1, // zero width non-breaking space
                _ => return,
            }
        }
    }
}

mod test {
    use super::*;

    macro_rules! test_lexor {
        ($path:expr, $($args:tt),*) => {{
            let mut p = new_path_lexor($path);
            let mut toks = vec![];
            let expected: Vec<&str> = vec![$($args),*];

            let tok = p.token();
            while let Some(tok) = tok {
                toks.push(tok);
            }

            assert_eq!(toks, expected)
        }};
    }

    #[test]
    fn test_lexor() {
        test_lexor!("    ",);
        test_lexor!("I am a fish", "I", "am", "a", "fish");
    }
}
