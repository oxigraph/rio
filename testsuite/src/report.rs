use crate::model::OwnedNamedNode;
use chrono::{DateTime, Utc};

#[derive(Debug, Clone)]
pub struct TestResult {
    pub test: OwnedNamedNode,
    pub outcome: TestOutcome,
    pub date: DateTime<Utc>,
}

#[derive(Debug, Clone)]
pub enum TestOutcome {
    Passed,
    Failed { error: String },
}
