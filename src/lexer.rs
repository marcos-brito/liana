use crate::{
    ast::CommentKind,
    token::{Token, TokenKind},
};
use std::str::Chars;

#[derive(Clone)]
pub struct Lexer<'a> {
    stream: Chars<'a>,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            stream: input.chars(),
            position: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.advance();
            let is_eof = token.kind == TokenKind::Eof;

            tokens.push(token);

            if is_eof {
                break;
            }
        }

        tokens
    }

    pub fn advance(&mut self) -> Token {
        let start_pos = self.position;
        let c = match self.peak() {
            Some(c) => c,
            None => {
                return Token {
                    start: start_pos,
                    len: 0,
                    kind: TokenKind::Eof,
                };
            }
        };

        self.eat();

        let kind = match c {
            '@' => TokenKind::At,
            '!' => TokenKind::Bang,
            '?' => TokenKind::Question,
            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::SemiColon,
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '%' => TokenKind::Percent,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '-' => self.minus(),
            '/' => self.slash(),
            '&' => self.and(),
            '|' => self.or(),
            '>' => self.greater(),
            '<' => self.lesser(),
            '=' => self.equal(),
            '"' => self.string_literal(),
            '\'' => self.char_literal(),
            c if is_whitespace(c) => TokenKind::Whitespace,
            c if is_ident_start(c) => self.ident(),
            _ => TokenKind::Unexpected,
        };

        Token {
            start: start_pos,
            len: self.position - start_pos,
            kind,
        }
    }

    fn peak(&self) -> Option<char> {
        self.stream.clone().next()
    }

    fn eat(&mut self) {
        self.position += 1;
        self.stream.next();
    }

    fn char_literal(&mut self) -> TokenKind {
        todo!()
    }

    fn string_literal(&mut self) -> TokenKind {
        todo!()
    }

    fn comment(&mut self) -> TokenKind {
        let kind = match self.peak() {
            Some('/') => TokenKind::Comment(CommentKind::Doc),
            Some('!') => TokenKind::Comment(CommentKind::TopLevel),
            _ => TokenKind::Comment(CommentKind::Regular),
        };

        while self.peak().is_some_and(|c| !is_newline(c)) {
            self.eat();
        }

        kind
    }

    fn and(&mut self) -> TokenKind {
        match self.peak() {
            Some('&') => {
                self.eat();
                TokenKind::AndAnd
            }
            _ => TokenKind::And,
        }
    }

    fn or(&mut self) -> TokenKind {
        match self.peak() {
            Some('|') => {
                self.eat();
                TokenKind::AndAnd
            }
            _ => TokenKind::And,
        }
    }

    fn greater(&mut self) -> TokenKind {
        match self.peak() {
            Some('=') => {
                self.eat();
                TokenKind::Ge
            }
            _ => TokenKind::Gt,
        }
    }

    fn lesser(&mut self) -> TokenKind {
        match self.peak() {
            Some('=') => {
                self.eat();
                TokenKind::Le
            }
            _ => TokenKind::Lt,
        }
    }

    fn equal(&mut self) -> TokenKind {
        match self.peak() {
            Some('=') => {
                self.eat();
                TokenKind::EqEq
            }
            _ => TokenKind::Eq,
        }
    }

    fn minus(&mut self) -> TokenKind {
        match self.peak() {
            Some('>') => {
                self.eat();
                TokenKind::Arrow
            }
            _ => TokenKind::Minus,
        }
    }

    fn slash(&mut self) -> TokenKind {
        match self.peak() {
            Some('/') => {
                self.eat();
                self.comment()
            }
            _ => TokenKind::Slash,
        }
    }

    fn ident(&mut self) -> TokenKind {
        while self.peak().is_some_and(is_ident_continue) {
            self.eat()
        }

        TokenKind::Identifier
    }
}

fn is_newline(c: char) -> bool {
    c == '\u{2028}' || c == '\u{2029}'
}

fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_tokens(source: &str, expected: Vec<TokenKind>) {
        let mut lexer = Lexer::new(source);
        let got = lexer.tokenize();

        for (t1, t2) in expected.into_iter().zip(got.into_iter()) {
            assert_eq!(t1, t2.kind);
        }
    }

    #[test]
    fn test_minus() {
        let tests = vec![
            ("-", vec![TokenKind::Minus]),
            ("->", vec![TokenKind::Arrow]),
            (
                "--->",
                vec![TokenKind::Minus, TokenKind::Minus, TokenKind::Arrow],
            ),
        ];

        for test in tests {
            check_tokens(test.0, test.1);
        }
    }

    #[test]
    fn test_comment() {
        let tests = vec![
            (
                "//comment\n",
                vec![TokenKind::Comment(CommentKind::Regular)],
            ),
            (
                "///documentation\n",
                vec![TokenKind::Comment(CommentKind::Doc)],
            ),
            (
                "//!toplevel\n",
                vec![TokenKind::Comment(CommentKind::TopLevel)],
            ),
            ("///!comment\n", vec![TokenKind::Comment(CommentKind::Doc)]),
            (
                "//!//toplevel\n",
                vec![TokenKind::Comment(CommentKind::TopLevel)],
            ),
        ];

        for test in tests {
            check_tokens(test.0, test.1);
        }
    }

    #[test]
    fn test_ident() {
        let tests = vec![
            ("ident3", vec![TokenKind::Identifier]),
            ("ident3.", vec![TokenKind::Identifier, TokenKind::Dot]),
            ("ident3", vec![TokenKind::Identifier]),
            ("ident_3_", vec![TokenKind::Identifier]),
            ("_ident", vec![TokenKind::Identifier]),
            (
                "ide.nt3.",
                vec![TokenKind::Identifier, TokenKind::Dot, TokenKind::Identifier],
            ),
            (
                "ide/nt",
                vec![
                    TokenKind::Identifier,
                    TokenKind::Slash,
                    TokenKind::Identifier,
                ],
            ),
        ];

        for test in tests {
            check_tokens(test.0, test.1);
        }
    }

    #[test]
    fn test_char_lit() {}

    #[test]
    fn test_string_lit() {}

    #[test]
    fn test_position() {
        let tests = vec![("ident3", 6), ("ident3=*", 8), ("T", 1)];

        for test in tests {
            let mut lexer = Lexer::new(test.0);

            lexer.tokenize();
            assert_eq!(test.1, lexer.position);
        }
    }
}
