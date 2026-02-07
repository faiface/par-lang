#[derive(Debug, Clone, PartialEq)]
pub struct AssertionResult {
    pub description: String,
    pub passed: bool,
}
