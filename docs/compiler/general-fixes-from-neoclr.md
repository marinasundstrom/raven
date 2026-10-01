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

## Honor accessible setters independently of public val mutability

Allow assignment, compound assignment and increments through accessible ordinary setters on val properties, while preserving private-setter access checks and public read-only semantics.

Source: 935598201. Validation: four binding cases including outside-scope rejection; positive case fails on original main.

## Preserve property accessor and backing-field identity during binding

Reuse completed auto-property accessors and stored backing fields during repeated binding; complete forward-bound initializers without replacing field identity. Validate ordinary .NET property values, aliases and constructor initialization.

Source: f90784db4, 92c509aa4, eaebf1934. Validation: six Debug/Release semantic-identity and execution cases, failing on original main.

## Check accessibility of qualified type expressions

Check accessibility when qualified member expressions resolve to types; external internal types now report RAV0500 while public wrappers remain usable.

Source: ee07a991e. Validation: two cross-assembly binding cases; inaccessible-type case fails on original main.
