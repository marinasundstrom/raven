# Raven syntax-tree output readiness

The first Raven green-node/factory emitter exists in both `NodeGenerator` and
`NodeGenerator.Raven`. Shared C# tests parse generated Raven for all repository
models, compare complete output between emitters and compile/run a generated
hierarchy against a small runtime-contract fixture. The fixture covers abstract
and concrete nodes, optional children, constructors, slots, update/With identity,
diagnostics, visitors and red-node creation.

This starts syntax-tree source output. The production driver still generates C#;
the authored production syntax runtime and CLI selection remain subsequent slices. The fixture is not a claim that the whole
compiler syntax runtime has been ported.

## Visitor output

Both generator implementations now emit generic and non-generic Raven visitor
partial classes. They retain the existing ordinal dispatch order, follow transitive
inheritance and delegate unhandled nodes to the authored `DefaultVisit`. Empty
abstract roots produce no dispatch method. The authored runtime still owns traversal
entry points and list/token handling.

Shared C# tests parse and compare output for all repository models in both red and
green namespaces. The generated green hierarchy executes direct Accept dispatch,
abstract root/intermediate dispatch, unknown-node fallback and inherited concrete
fallback in both semantic query orders. Red visitor output is covered by parsing
and parity, not a production red-runtime execution claim.

## Rewriter output

Both implementations emit concrete Raven rewriter partial methods on the generic
visitor. These delegate token and list traversal to authored runtime hooks and
pass visited slots to Update, retaining explicit kinds and no-op identity. Optional
slots skip visits when absent. Required green lists assert the nullable VisitList
result is present; the list compile probe correctly diagnosed the original missing
assertion, so both emitters now express the required-slot contract explicitly.
This is generator output stabilization, not weakened compiler nullability.

The generated green runtime fixture covers recursive token replacement, list
replacement/deletion, optional generic/typed child deletion, no-op identity and
unknown-node fallback. Repository red output is parsed and compared between the
two emitters; it has not been executed with a production red runtime. Authored
production list traversal and syntax runtime porting remain separate work.

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

A subsequent fresh baseline exposed stale flags on reused expression-bodied
property getters. Binding now refreshes resolved modifiers while retaining the
canonical accessor identity. The runtime fixture covers diagnostics-first and
derived-property-query-first orders and checks override/virtual symbol shape before
emission; PORT-045 records the regression and validation.

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


## Red-node declaration output

Both generator codebases now emit Raven red-node declarations. Shared C# tests
parse repository output and compare it exactly, then compile and execute a
small generated hierarchy with authored red and green runtime fixtures. Runtime
checks cover token/list positions, child parent links and caching, unchanged
Update/With identity, detached changed nodes, explicit kinds and both visitor
forms. The generated typed constructor delegates through `: self(...)`.

This probe exposed qualified source-type lookup during lazy declaration binding:
metadata-name lookup intentionally omits source types on re-entry. Qualified
expression lookup now completes the indexed source declaration in the same
namespace rather than caching a missing-type error. Initializers bind once after
declaration binders are ready, and their errors survive rebinding in the model’s
declaration diagnostic bag. Shared binding owns these fixes;
there is no new target policy or Raven syntax. The production red runtime remains unported. Optional tokens, token/separated lists and closed
hierarchies have parsing/parity coverage here, not production execution coverage.

## Red factory output

Both emitters now produce Raven red factories. Automatic overloads omit default
and optional tokens and nullable slots; explicit XML definitions control parameter
order, defaults, aliases and summaries. Abstract nodes have no factories. Shared
C# tests parse automatic and explicit factories for every repository node and
compare exact output between the C# and Raven emitters.

The generated runtime probe executes full and convenience overloads, explicit
aliases, default tokens, null children, lists and explicit kinds, in both
compilation-first and semantic-model-first query orders. Missing-token defaults
also have a focused source assertion. This validates generated factories against
the small authored fixture on .NET, not the production syntax runtime or native
neoCLR. No language rule changed; nullable definition access in the Raven emitter
uses an explicit unwrap within its null guard, as required by default nullability.
