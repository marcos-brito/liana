use crate::ast::{self};
use crate::lexer::Lexer;
use crate::token::{Keyword, Token, TokenKind};

pub struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
    token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            lexer: Lexer::new(source),
            token: Token::default(),
        }
    }

    fn parse(&self) -> ast::Module {
        todo!()
    }

    fn advance(&mut self) -> &Token {
        self.token = self.lexer.advance();
        &self.token
    }

    fn peak(&self) -> Token {
        self.lexer.clone().advance()
    }

    fn expect(&mut self, kind: TokenKind) {
        self.advance();
    }

    fn substring(&self, start: usize, len: usize) -> String {
        self.source.chars().skip(start).take(len).collect()
    }

    fn identifier(&mut self) -> ast::Identifier {
        self.expect(TokenKind::Identifier);

        let name = self.substring(self.token.start, self.token.len);

        ast::Identifier { name }
    }

    fn qualified_identifier(&mut self) -> ast::QualifiedIdentifier {
        let mut parts = Vec::new();

        while self.peak().kind == TokenKind::Identifier {
            let ident = self.identifier();

            parts.push(ident);

            if self.peak().kind == TokenKind::Dot {
                self.advance();
            } else {
                break;
            }
        }

        ast::QualifiedIdentifier { parts }
    }
}

mod test {
    use super::*;
    use crate::ast;

    #[test]
    fn test_identifier() {
        let tests = vec![
            (
                "T",
                ast::Identifier {
                    name: "T".to_string(),
                },
            ),
            (
                "Maybe",
                ast::Identifier {
                    name: "Maybe".to_string(),
                },
            ),
        ];

        for test in tests {
            let mut parser = Parser::new(test.0);

            assert_eq!(test.1, parser.identifier())
        }
    }

    #[test]
    fn test_qualified_identifier() {
        let tests = vec![
            (
                "std.opt.Option",
                ast::QualifiedIdentifier {
                    parts: vec![
                        ast::Identifier {
                            name: "std".to_string(),
                        },
                        ast::Identifier {
                            name: "opt".to_string(),
                        },
                        ast::Identifier {
                            name: "Option".to_string(),
                        },
                    ],
                },
            ),
            (
                "Result",
                ast::QualifiedIdentifier {
                    parts: vec![ast::Identifier {
                        name: "Result".to_string(),
                    }],
                },
            ),
            (
                "std.io.",
                ast::QualifiedIdentifier {
                    parts: vec![
                        ast::Identifier {
                            name: "std".to_string(),
                        },
                        ast::Identifier {
                            name: "io".to_string(),
                        },
                    ],
                },
            ),
        ];

        for test in tests {
            let mut parser = Parser::new(test.0);

            assert_eq!(test.1, parser.qualified_identifier())
        }
    }
}
