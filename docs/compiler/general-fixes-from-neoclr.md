# Independent compiler fixes extracted from neoCLR integration

The author requested a dedicated fix branch based on main for changes that benefit
Raven independently. `codex/compiler-fixes-from-neoclr` starts at `d7040e21d`.
The fixes below carry no neoCLR adapter, metadata-library dependency or shared-emission
refactor. Ordinary .NET remains the default; Runtime Contract options are unchanged.
Tests are C# and use normal Raven compilation/emission rather than target plan assertions.
Source commit IDs record provenance from `codex/metadata-consumer`; mixed commits are
extracted by behavior rather than cherry-picked wholesale.

## Parse complete assignment right-hand expressions

Parse logical, coalescing and prefix-not expressions as complete assignment right-hand sides; retain right-associative chained assignments.

Source: 762bebad0. Validation: six focused parser cases; five fail on the original main.
