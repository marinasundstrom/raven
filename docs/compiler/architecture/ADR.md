# Architecture decision records

Raven records architecturally significant decisions as architecture decision
records (ADRs). A record captures why a direction was chosen at a particular
time; design documents and specifications continue to describe the complete
current model.

Create an ADR when a decision changes or constrains one or more of these areas:

- compiler, SDK, runtime-library, macro-library, or tooling dependency layers;
- public compiler API conventions;
- language semantics or their CLR representation;
- bootstrap, build, packaging, release, or compatibility strategy;
- persistent architecture that future work would otherwise be tempted to
  reverse without knowing the original tradeoffs.

Routine implementation details do not need separate records unless they become
an architectural constraint. When several related choices form one decision,
keep them in one ADR and link to the detailed design document.

## Lifecycle

Use monotonically increasing four-digit numbers. Never renumber accepted
records. Start from this template:

```markdown
# ADR-NNNN: Decision title

- Status: Proposed
- Date: YYYY-MM-DD
- Owners: Raven project maintainers

## Context

## Decision

## Consequences

## Alternatives considered

## Follow-up
```

The allowed statuses are:

- **Proposed** — under active discussion and not yet binding.
- **Accepted** — the current architectural decision.
- **Superseded by ADR-NNNN** — replaced; retain the original record unchanged
  apart from its status and a link to the replacement.
- **Rejected** — considered but deliberately not adopted.

Material changes to an accepted decision require a new ADR that supersedes it.
Clarifications, links, and factual corrections may update the existing record
without changing the decision.

## Index

- [ADR-0001: Preserve preview.14 as the pre-bootstrap foundation](decisions/0001-pre-bootstrap-foundation.md)
