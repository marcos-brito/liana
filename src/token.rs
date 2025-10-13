use crate::ast::{CommentKind, LiteralKind};

#[derive(Debug)]
pub struct Token {
    pub start: usize,
    pub len: usize,
    pub kind: TokenKind,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            start: 0,
            len: 0,
            kind: TokenKind::Eof,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Identifier,
    Comment(CommentKind),
    Literal(LiteralKind),
    Whitespace,
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// @
    At,
    /// !
    Bang,
    /// ?
    Question,
    /// .
    Dot,
    /// ,
    Comma,
    /// :
    Colon,
    /// ;
    SemiColon,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Star,
    /// /
    Slash,
    /// %
    Percent,
    /// &
    And,
    /// &&
    AndAnd,
    /// |
    Or,
    /// ||
    OrOr,
    /// >
    Gt,
    /// >=
    Ge,
    /// <
    Lt,
    /// <=
    Le,
    /// =
    Eq,
    /// ==
    EqEq,
    /// !=
    Ne,
    /// ->
    Arrow,

    Unexpected,
    Eof,
}

#[derive(Debug)]
pub enum Keyword {
    Val,
    Var,
    Data,
    Imp,
    Exp,
    Fun,
    Return,
    Signature,
    Instance,
    Of,
    When,
    Loop,
    If,
    Else,
    False,
    True,
}

impl Keyword {
    fn is_keyword(value: &str) -> bool {
        Keyword::from_str(value).is_some()
    }

    fn from_str(value: &str) -> Option<Self> {
        let keyword = match value {
            "val" => Keyword::Val,
            "var" => Keyword::Var,
            "data" => Keyword::Data,
            "imp" => Keyword::Imp,
            "exp" => Keyword::Exp,
            "fun" => Keyword::Fun,
            "return" => Keyword::Return,
            "signature" => Keyword::Signature,
            "instance" => Keyword::Instance,
            "of" => Keyword::Of,
            "when" => Keyword::When,
            "loop" => Keyword::Loop,
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "false" => Keyword::False,
            "true" => Keyword::True,
            _ => return None,
        };

        Some(keyword)
    }
}
