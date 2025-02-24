#[derive(Debug, PartialEq)]
pub enum Token {
    Dec(String),
    Hex(String),
    Oct(String),

    StrLiteral(String),
    CharLiteral(String),

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

    Reserved(String),

    RoundOpen,
    CurlyOpen,
    SquareOpen,

    RoundClose,
    CurlyClose,
    SquareClose,

    Period,

    Semicolon,
    Comma,

    Identifier(String),
    Comment(String),
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

    fn next_token(&mut self) -> Result<Option<Token>, ()> {
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
                    _ => Ok((Some(Token::Comment(comment.iter().collect())), &after[2..])),
                }
            }
            ['/', '/', rest @ ..] => {
                let (comment, after) = extract_while(rest, |s| matches!(s, ['\n', ..]));
                Ok((Some(Token::Comment(comment.iter().collect())), after))
            }
            ['/', '=', rest @ ..] => Ok((Some(Token::SlashEqual), rest)),
            ['/', rest @ ..] => Ok((Some(Token::Slash), rest)),
            ['0', 'x', rest @ ..] => {
                let (hex, after) = extract_while(rest, |s| {
                    !matches!(s, ['0'..='9' | 'a'..='f' | 'A'..='F', ..])
                });
                match hex {
                    [] => Err(()),
                    _ => Ok((Some(Token::Hex(hex.iter().collect())), after)),
                }
            }
            ['0', rest @ ..] => {
                let (oct, after) = extract_while(rest, |s| !matches!(s, ['0'..='7', ..]));
                let oct = match oct {
                    [] => &self.rest[0..1], // found only '0' (= slice oct is empty)
                    _ => oct,
                };

                Ok((Some(Token::Oct(oct.iter().collect())), after))
            }
            ['1'..='9', ..] => {
                let (dec, after) = extract_while(&self.rest, |s| !matches!(s, ['0'..='9', ..]));
                Ok((Some(Token::Dec(dec.iter().collect())), after))
            }
            ['\'', rest @ ..] => {
                let (lit, after) = extract_while(rest, |s| matches!(s, ['\n' | '\'', ..]));
                match after {
                    ['\n', ..] => Err(()), // missing closing (found endline)
                    [] => Err(()),         // missing closing (found EOF)
                    _ => Ok((Some(Token::CharLiteral(lit.iter().collect())), &after[1..])),
                }
            }
            ['\"', rest @ ..] => {
                let (lit, after) = extract_while(rest, |s| matches!(s, ['\n' | '\"', ..]));
                match after {
                    ['\n', ..] => Err(()),
                    [] => Err(()),
                    _ => Ok((Some(Token::StrLiteral(lit.iter().collect())), &after[1..])),
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
                    Token::Reserved(ident.iter().collect())
                } else {
                    Token::Identifier(ident.iter().collect())
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
    type Item = Result<Token, ()>;

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

        let expected = vec![
            Token::Reserved("if".to_string()),
            Token::RoundOpen,
            Token::Identifier("flag".to_string()),
            Token::RoundClose,
            Token::CurlyOpen,
            Token::Reserved("int".to_string()),
            Token::Identifier("num".to_string()),
            Token::Equal,
            Token::Identifier("arr".to_string()),
            Token::SquareOpen,
            Token::RoundOpen,
            Token::RoundOpen,
            Token::Oct("0".to_string()),
            Token::DoubleEqual,
            Token::Oct("123".to_string()),
            Token::RoundClose,
            Token::Astarisk,
            Token::Hex("255".to_string()),
            Token::Plus,
            Token::Identifier("bcd".to_string()),
            Token::RoundClose,
            Token::Slash,
            Token::Identifier("edf".to_string()),
            Token::SquareClose,
            Token::Semicolon,
            Token::Comment(" X 0; X".to_string()),
            Token::Comment(" X ".to_string()),
            Token::CurlyClose,
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn char_literal() {
        let source = "char ch = 'abc'";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&chars);

        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        let actual = tokens.unwrap();

        let expected = vec![
            Token::Reserved("char".to_string()),
            Token::Identifier("ch".to_string()),
            Token::Equal,
            Token::CharLiteral("abc".to_string()),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn str_literal() {
        let source = "char s[5] = \"abc\"";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&chars);

        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        let actual = tokens.unwrap();
        println!("{:?}", actual);

        let expected = vec![
            Token::Reserved("char".to_string()),
            Token::Identifier("s".to_string()),
            Token::SquareOpen,
            Token::Dec("5".to_string()),
            Token::SquareClose,
            Token::Equal,
            Token::StrLiteral("abc".to_string()),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn die_char_literal_missing_close_eof() {
        let source = "ch = 'abc";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&chars);

        let actual = lexer.collect::<Vec<_>>();

        let expected = vec![
            Ok(Token::Identifier("ch".to_string())),
            Ok(Token::Equal),
            Err(()),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn die_char_literal_missing_close_endline() {
        let source = "ch = 'abc\n'";
        let chars = source.chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&chars);

        let actual = lexer.collect::<Vec<_>>();

        let expected = vec![
            Ok(Token::Identifier("ch".to_string())),
            Ok(Token::Equal),
            Err(()),
        ];

        assert_eq!(actual, expected);
    }
}
