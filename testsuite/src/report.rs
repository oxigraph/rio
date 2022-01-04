use crate::model::OwnedNamedNode;

#[derive(Debug, Clone)]
pub struct TestResult {
    pub test: OwnedNamedNode,
    pub outcome: TestOutcome,
}

#[derive(Debug, Clone)]
pub enum TestOutcome {
    Passed,
    Failed { error: String },
}
