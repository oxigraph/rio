pub mod mf {
    use rio_api::model::NamedNode;

    pub const INCLUDE: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#include",
    };
    pub const ENTRIES: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#entries",
    };
    pub const NAME: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name",
    };
    pub const ACTION: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action",
    };
    pub const RESULT: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result",
    };
}

pub mod rdf {
    use rio_api::model::NamedNode;

    pub const FIRST: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#first",
    };
    pub const NIL: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil",
    };
    pub const REST: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest",
    };
    pub const TYPE: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    };
}

pub mod rdfs {
    use rio_api::model::NamedNode;

    pub const COMMENT: NamedNode<'static> = NamedNode {
        iri: "http://www.w3.org/2000/01/rdf-schema#comment",
    };
}
