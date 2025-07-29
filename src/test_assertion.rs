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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assertion_result_creation() {
        let result = AssertionResult {
            description: "Test description".to_string(),
            passed: true,
        };
        assert_eq!(result.description, "Test description");
        assert!(result.passed);
    }

    #[test]
    fn test_assertion_channel() {
        let (sender, receiver) = create_assertion_channel();

        let result1 = AssertionResult {
            description: "First test".to_string(),
            passed: true,
        };

        let result2 = AssertionResult {
            description: "Second test".to_string(),
            passed: false,
        };

        sender.send(result1.clone()).unwrap();
        sender.send(result2.clone()).unwrap();

        assert_eq!(receiver.recv().unwrap(), result1);
        assert_eq!(receiver.recv().unwrap(), result2);
    }

    #[test]
    fn test_multiple_senders() {
        let (sender1, receiver) = create_assertion_channel();
        let sender2 = sender1.clone();

        let result1 = AssertionResult {
            description: "From sender 1".to_string(),
            passed: true,
        };

        let result2 = AssertionResult {
            description: "From sender 2".to_string(),
            passed: false,
        };

        sender1.send(result1.clone()).unwrap();
        sender2.send(result2.clone()).unwrap();

        let received: Vec<AssertionResult> =
            vec![receiver.recv().unwrap(), receiver.recv().unwrap()];

        assert!(received.contains(&result1));
        assert!(received.contains(&result2));
    }
}
