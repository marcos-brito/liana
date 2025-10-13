#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct QualifiedIdentifier {
    pub parts: Vec<Identifier>,
}
