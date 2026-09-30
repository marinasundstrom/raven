# ADR-0003: Runtime/platform contracts, symbol sources, and code generation

- Status: Accepted
- Date: 2026-09-30
- Owners: Raven project maintainers

## Context

Raven primarily targets .NET, with neoCLR as the next intended platform and
potentially other platforms later. The compiler's current reflection-based
imports and CLI emission are implementation choices, not universal metadata or
execution models. Current consumers are controlled by the project, allowing
deliberate API redesign during this phase.

## Decision

A target is defined by its **runtime/platform contract**: its semantic rules,
available type environment, runtime representations, and feature restrictions.
An existing target framework may provide much of this contract. The compiler may
also encode rules for known platforms and diagnose unsupported features.

One or more supported symbol sources supply definitions under that contract.
Assembly metadata is one source; native declarations, generated/in-memory
definitions, or other supported formats may supply a different type universe.
The shared loading interface must describe semantic definitions, not require
reflection or an assembly format.

One or more code generators may implement the runtime/platform contract. Symbol
loading and code generation are separate interfaces whose compatibility must be
validated against that contract, including representation, ABI, and supported
features. They are not arbitrary interchangeable choices and need not have a
one-to-one relationship. The compiler execution host remains independent.

Raven owns its semantic model. Shared language semantics and compiler queries
consume semantic symbols supplied by the selected target. Roslyn is a useful
structural reference, not a requirement for matching its metadata, semantic, or
codegen APIs. Redesign interfaces when that improves Raven's target model;
migrate controlled consumers together rather than retaining unnecessary
compatibility adapters. Continue to keep semantic state in the compiler and
out of language-service presentation code.

Selecting a different target can require different references, imported symbol
identities, semantic interpretation, and rebinding. Do not promise that an
already-bound compilation can be sent to an arbitrary emitter. Future target
identity/configuration must participate in snapshot reuse and invalidation.
Cross-target reuse or translation requires an explicit compatibility contract.

Symbol sources need not be metadata files or CLI assemblies. Shared abstractions must represent the
language facts required by binding and emission without requiring reflection
objects, CLR opcodes, or .NET assembly identity everywhere. .NET is the first
complete implementation; neoCLR follows the staged bootstrap plan. Introducing
multiple targets does not by itself require a universal IR or a plugin ABI.

## Consequences

Current internal assembly and namespace discovery interfaces are extraction
steps. They may be replaced or generalized as the target-owned symbol model is
developed. In particular, an assembly-shaped input must not become mandatory
for every future platform. Namespace lookup capabilities may be composed by
merged namespaces independently of where their constituent symbols came from.

Tests preserve intended semantics and target contracts, not incidental public
API shapes or accidental .NET assumptions. General abstractions still require
independent main-based validation; neoCLR policy stays isolated until qualified.
Existing frozen-bootstrap provenance and dependency gates remain unchanged.

## Alternatives considered

- Independently selecting arbitrary importers and emitters would allow symbol
  universes whose type identities and representations the emitter cannot honor.
- Reproducing Roslyn APIs exactly would make .NET-specific assumptions part of
  Raven's architecture even where they do not fit the target model.
- Designing every future target upfront would create speculative contracts;
  use .NET and concrete neoCLR requirements to establish the initial boundaries.

## Follow-up

Define target selection, target-owned import sessions and symbol factories,
semantic capabilities, emission inputs, and reuse keys through verified slices.
Define diagnostics for incompatible symbol-source/backend combinations and
unsupported language features before attempting emission. A native backend may
have a different type environment; it is not inherently .NET with a new output
format. Existing .NET Native AOT remains a .NET deployment path.
See the [target boundary plan](../target-boundaries-and-bootstrap-plan.md).
This ADR records direction; selectable non-.NET backends are not implemented yet.
