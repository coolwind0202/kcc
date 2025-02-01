#[derive(Debug)]
pub enum Token<'a> {
    Dec(&'a [char]),
    Hex(&'a [char]),
    Oct(&'a [char]),

    StrLiteral(&'a [char]),
    CharLiteral(&'a [char]),

    Plus,
    Minus,
    Astarisk,
    Slash,

    DoublePlus,
    DoubleMinus,

    Equal,
    PlusEqual,
    MinusEqual,
    AstariskEqual,
    SlashEqual,
    DoubleEqual,

    Reserved(&'a [char]),

    RoundOpen,
    CurlyOpen,
    SquareOpen,

    RoundClose,
    CurlyClose,
    SquareClose,

    Period,

    Semicolon,
    Comma,

    Identifier(&'a [char]),
    Comment(&'a [char]),
}

pub struct Lexer<'a> {
    rest: &'a [char],
}

fn extract_while<'a>(s: &'a [char], condition: fn(&'a [char]) -> bool) -> (&'a [char], &'a [char]) {
    let len = s.len();

    for i in 0..len {
        if condition(&s[i..]) {
            return (&s[..i], &s[i..]);
        }
    }

    return (&s[..len], &s[len..]);
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [char]) -> Self {
        Lexer { rest: source }
    }

    fn next_token(&mut self) -> Result<Option<Token<'a>>, ()> {
        let ret = match self.rest {
            [' ' | '\n' | '\t', rest @ ..] => Ok((None, rest)),
            ['=', '=', rest @ ..] => Ok((Some(Token::DoubleEqual), rest)),
            ['=', rest @ ..] => Ok((Some(Token::Equal), rest)),
            ['+', '+', rest @ ..] => Ok((Some(Token::DoublePlus), rest)),
            ['+', '=', rest @ ..] => Ok((Some(Token::PlusEqual), rest)),
            ['+', rest @ ..] => Ok((Some(Token::Plus), rest)),
            ['-', '-', rest @ ..] => Ok((Some(Token::DoubleMinus), rest)),
            ['-', '=', rest @ ..] => Ok((Some(Token::MinusEqual), rest)),
            ['-', rest @ ..] => Ok((Some(Token::Minus), rest)),
            ['*', '=', rest @ ..] => Ok((Some(Token::AstariskEqual), rest)),
            ['*', rest @ ..] => Ok((Some(Token::Astarisk), rest)),
            ['/', '*', rest @ ..] => {
                let (comment, after) = extract_while(rest, |s| matches!(s, ['*', '/', ..]));
                match after {
                    [] => Err(()),
                    _ => Ok((Some(Token::Comment(comment)), &after[2..])),
                }
            }
            ['/', '/', rest @ ..] => {
                let (comment, after) = extract_while(rest, |s| matches!(s, ['\n', ..]));
                Ok((Some(Token::Comment(comment)), after))
            }
            ['/', '=', rest @ ..] => Ok((Some(Token::SlashEqual), rest)),
            ['/', rest @ ..] => Ok((Some(Token::Slash), rest)),
            ['0', 'x', rest @ ..] => {
                let (hex, after) = extract_while(rest, |s| {
                    !matches!(s, ['0'..='9' | 'a'..='f' | 'A'..='F', ..])
                });
                match hex {
                    [] => Err(()),
                    _ => Ok((Some(Token::Hex(hex)), after)),
                }
            }
            ['0', rest @ ..] => {
                let (oct, after) = extract_while(rest, |s| !matches!(s, ['0'..='7', ..]));
                let oct = match oct {
                    [] => &self.rest[0..1], // found only '0' (= slice oct is empty)
                    _ => oct,
                };

                Ok((Some(Token::Oct(oct)), after))
            }
            ['1'..='9', ..] => {
                let (dec, after) = extract_while(&self.rest, |s| !matches!(s, ['0'..='9', ..]));
                Ok((Some(Token::Dec(dec)), after))
            }
            ['\'', rest @ ..] => {
                let (lit, after) = extract_while(rest, |s| matches!(s, ['\n' | '\'', ..]));
                match after {
                    ['\n', ..] => Err(()), // missing closing (found endline)
                    [] => Err(()),         // missing closing (found EOF)
                    _ => Ok((Some(Token::CharLiteral(lit)), &after[1..])),
                }
            }
            ['\"', rest @ ..] => {
                let (lit, after) = extract_while(rest, |s| matches!(s, ['\n' | '\"', ..]));
                match after {
                    ['\n', ..] => Err(()),
                    [] => Err(()),
                    _ => Ok((Some(Token::StrLiteral(lit)), &after[1..])),
                }
            }
            [';', rest @ ..] => Ok((Some(Token::Semicolon), rest)),
            [',', rest @ ..] => Ok((Some(Token::Comma), rest)),
            ['.', rest @ ..] => Ok((Some(Token::Period), rest)),
            ['(', rest @ ..] => Ok((Some(Token::RoundOpen), rest)),
            [')', rest @ ..] => Ok((Some(Token::RoundClose), rest)),
            ['{', rest @ ..] => Ok((Some(Token::CurlyOpen), rest)),
            ['}', rest @ ..] => Ok((Some(Token::CurlyClose), rest)),
            ['[', rest @ ..] => Ok((Some(Token::SquareOpen), rest)),
            [']', rest @ ..] => Ok((Some(Token::SquareClose), rest)),
            ['a'..='z' | 'A'..='Z' | '_', ..] => {
                let (ident, after) = extract_while(&self.rest, |s| {
                    !matches!(s, ['a'..='z' | 'A'..='Z' | '_' | '0'..='9', ..])
                });

                // Rustのstrリテラルでreserved判定の処理を書きやすくなるよう、identをstrに変換する
                let ident_string = ident.iter().collect::<String>();
                let ident_str = ident_string.as_str();

                let reserved_words = ["if", "while", "for", "int", "char"];
                let ident_is_reserved = reserved_words.contains(&ident_str);

                let token = if ident_is_reserved {
                    Token::Reserved(ident)
                } else {
                    Token::Identifier(ident)
                };

                Ok((Some(token), after))
            }
            _ => Err(()),
        };

        let ret = ret
            .map(|(token, rest)| {
                self.rest = rest;
                token
            })
            .inspect_err(|_| self.rest = &[]);

        ret
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        while !self.rest.is_empty() {
            match self.next_token() {
                Ok(Some(token)) => {
                    return Some(Ok(token));
                }
                Err(_) => {
                    return Some(Err(()));
                }
                _ => (),
            };
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::lexer::Token;

    #[test]
    fn tokens() {
        let source = "
            if (flag) {
                int num = arr[((0 == 0123) * 0x255 + bcd) / edf]; // X 0; X
                /* X */
            }
            ";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(chars.as_slice());

        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        let actual = tokens.unwrap();

        assert!(matches!(
            actual.as_slice(),
            [
                Token::Reserved(&['i', 'f']),
                Token::RoundOpen,
                Token::Identifier(&['f', 'l', 'a', 'g']),
                Token::RoundClose,
                Token::CurlyOpen,
                Token::Reserved(&['i', 'n', 't']),
                Token::Identifier(&['n', 'u', 'm']),
                Token::Equal,
                Token::Identifier(&['a', 'r', 'r']),
                Token::SquareOpen,
                Token::RoundOpen,
                Token::RoundOpen,
                Token::Oct(&['0']),
                Token::DoubleEqual,
                Token::Oct(&['1', '2', '3']),
                Token::RoundClose,
                Token::Astarisk,
                Token::Hex(&['2', '5', '5']),
                Token::Plus,
                Token::Identifier(&['b', 'c', 'd']),
                Token::RoundClose,
                Token::Slash,
                Token::Identifier(&['e', 'd', 'f']),
                Token::SquareClose,
                Token::Semicolon,
                Token::Comment(&[' ', 'X', ' ', '0', ';', ' ', 'X']),
                Token::Comment(&[' ', 'X', ' ']),
                Token::CurlyClose
            ]
        ));
    }

    #[test]
    fn char_literal() {
        let source = "char ch = 'abc'";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&chars);

        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        let actual = tokens.unwrap();

        assert!(matches!(
            actual.as_slice(),
            [
                Token::Reserved(&['c', 'h', 'a', 'r']),
                Token::Identifier(&['c', 'h']),
                Token::Equal,
                Token::CharLiteral(&['a', 'b', 'c'])
            ]
        ))
    }

    #[test]
    fn str_literal() {
        let source = "char s[5] = \"abc\"";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&chars);

        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        let actual = tokens.unwrap();
        println!("{:?}", actual);

        assert!(matches!(
            actual.as_slice(),
            [
                Token::Reserved(&['c', 'h', 'a', 'r']),
                Token::Identifier(&['s']),
                Token::SquareOpen,
                Token::Dec(&['5']),
                Token::SquareClose,
                Token::Equal,
                Token::StrLiteral(&['a', 'b', 'c'])
            ]
        ))
    }

    #[test]
    fn die_char_literal_missing_close_eof() {
        let source = "ch = 'abc";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&chars);

        let actual = lexer.collect::<Vec<_>>();

        assert!(matches!(
            actual.as_slice(),
            [Ok(Token::Identifier(&['c', 'h'])), Ok(Token::Equal), Err(_)]
        ))
    }

    #[test]
    fn die_char_literal_missing_close_endline() {
        let source = "ch = 'abc\n'";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&chars);

        let actual = lexer.collect::<Vec<_>>();

        assert!(matches!(
            actual.as_slice(),
            [Ok(Token::Identifier(&['c', 'h'])), Ok(Token::Equal), Err(_)]
        ))
    }
}
