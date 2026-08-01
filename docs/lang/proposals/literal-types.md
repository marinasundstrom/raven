# Proposal: Literal-value types

> ❌ This proposal has been withdrawn.

Raven does not treat literal values as types and does not synthesize anonymous
union symbols during inference. Literal expressions have their ordinary Raven
types, while literal patterns provide value-specific matching where needed.

The earlier design proposed metadata attributes for encoding sets of literal
and nominal alternatives. That representation was removed with the anonymous
semantic-type model and is not part of Raven's language, compiler API, or ABI.

If finite literal domains are revisited, they should be designed independently
around Raven's current nominal union model and .NET interoperability rather than
reviving the removed semantic type.
