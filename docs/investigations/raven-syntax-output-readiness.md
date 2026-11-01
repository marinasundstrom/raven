# Raven syntax-tree output readiness

The first Raven green-node/factory emitter exists in both `NodeGenerator` and
`NodeGenerator.Raven`. Shared C# tests parse generated Raven for all repository
models, compare complete output between emitters and compile/run a generated
hierarchy against a small runtime-contract fixture. The fixture covers abstract
and concrete nodes, optional children, constructors, slots, update/With identity,
diagnostics, visitors and red-node creation.

This starts syntax-tree source output. The production driver still generates C#;
the authored production syntax runtime, Raven red-node/visitor/rewriter output and
CLI selection remain subsequent slices. The fixture is not a claim that the whole
compiler syntax runtime has been ported.

## Gaps fixed during the first compile/run probe

- Generic overrides compared distinct method type-parameter symbols directly.
  Override lookup now substitutes base parameters into the overriding method's
  context. Member lookup also removes overridden definitions from explicit-call
  candidates. Runtime regressions cover base and derived receivers, with negative
  coverage for different arity and parameter types.
- Covariant return overrides were rejected. Shared binding now accepts implicit
  reference-return covariance when the target contract advertises support. CLI
  emission maps the new slot to the base slot and applies
  PreserveBaseOverridesAttribute. Runtime tests distinguish implementations across
  three levels and check the concrete reflected return type. Numeric, unrelated
  and nullable-weakened returns remain rejected.
- Nominal constraints treated nullable reference wrappers as unrelated types.
  Constraint membership now unwraps reference annotations while preserving
  nullable value types and explicit notnull checks.
- Constructor initializer arguments lacked candidate target types. They now use
  the ordinary invocation argument binder while retaining instance-access checks.
  Runtime tests cover heterogeneous nullable slot collections in base and self
  delegation.

A final collection diagnostic was correct: absent children require nullable
slots. Both generators and the C# runtime now explicitly annotate raw slots as
GreenNode?[] and GetSlot as nullable. This preserves existing behavior instead of
weakening Raven's nullable-to-nonnullable conversion rule.

## Target and integration boundaries

Covariant return emission currently requires target metadata containing
RuntimeFeature.CovariantReturnsOfClasses and the parameterless
PreserveBaseOverridesAttribute constructor. Older CLI targets retain the override
diagnostic. Native neoCLR covariant override metadata/dispatch still needs its own
implementation and validation; the CLI marker requirement is an experimental
bridge limitation, not permanent feature exclusivity.

The corrections live in the shared C# compiler because those compiler components
have not yet been ported. Their future Raven translations must retain them. The
generator changes are present in both source codebases. Tests execute on modern
.NET; no native neoCLR execution is claimed. See the compiler Runtime Contract
and neoCLR bridge documentation for ownership and encoding details.
