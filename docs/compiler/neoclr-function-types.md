# Deferred neoCLR structural Function types

Native structural Function work stays on Raven's
`codex/neoclr-structural-types` and neoCLR's `codex/structural-types`. The detailed
experiment notes are retained on the Raven feature branch. Main does not claim
structural function identity, assignability or native Function introspection.
Promotion requires neoCLR's native metadata layer and complete compiler support.

Ordinary Raven function syntax and .NET delegate support remain shared. The
neoCLR CLI bridge also retains its nominal Func ABI with inhabited unit results;
this representation is required by neoCLR main's existing callbacks and is not
structural type support. Ordinary .NET unit functions continue to use Action.

Self is a separate, target-gated contract. Its integration does not promote the
structural-types experiment. See [the bridge inventory](neoclr-cli-bridge.md) for
transport behavior and native metadata replacement requirements.
