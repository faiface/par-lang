use std::sync::mpsc;

#[derive(Debug, Clone, PartialEq)]
pub struct AssertionResult {
    pub description: String,
    pub passed: bool,
}

pub fn create_assertion_channel() -> (
    mpsc::Sender<AssertionResult>,
    mpsc::Receiver<AssertionResult>,
) {
    mpsc::channel()
}
