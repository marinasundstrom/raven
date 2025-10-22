# Async entry nightly diff

_Last generated: pending first tool execution_

> **Note:** Run `dotnet run --project tools/AsyncEntryDiffRunner/AsyncEntryDiffRunner.csproj` from the repo root to refresh this
> report after the Raven compiler has been built. The command emits the latest pointer trace and IL comparisons below and exits
> with a non-zero status if either regression diverges from the Step 15 baselines.

## Pointer trace regression

- Compilation: ❔ _(awaiting first run)_
- Execution: ❔ _(awaiting first run)_
- Runtime pointer timeline: ❔ _(awaiting first run)_
- Emitted IL pointer timeline: ❔ _(awaiting first run)_

Baseline timeline:
```
Step15:_state:load
Step15:_state:load
Step15:_state:store
Step15:<>awaiter0:store
Step15:_state:store
Step15:<>awaiter0:addr
Step15:<>awaiter0:store
Step15:_state:store
Step15:<>awaiter0:addr
Step15:_state:load
Step15:_state:load
Step15:_state:store
Step15:<>awaiter0:load
Step15:<>awaiter0:store
Step15:_state:store
Step15:_state:load
Step15:_state:load
Step15:_state:store
Step15:<>awaiter0:load
Step15:<>awaiter0:store
Step15:_state:store
Step15:_builder:addr
Step15:_builder:store
Step15:_builder:load
```
_Source: `docs/investigations/snippets/async-entry-step15.log`_

## Raven vs. Roslyn MoveNext IL

- Raven compilation: ❔ _(awaiting first run)_
- Roslyn compilation: ❔ _(awaiting first run)_
- Instruction comparison: ❔ _(awaiting first run)_

Run the tool to capture the latest IL for Raven’s `Program.<MainAsync>d__0.MoveNext` and Roslyn’s `Program.<Main>d__0.MoveNext`.
The generated sections will embed both instruction streams and highlight the first mismatched instructions so nightly automation
can flag regressions immediately.
