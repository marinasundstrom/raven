# Async entry nightly diff

_Last generated: pending first tool execution_

> **Note:** Run `dotnet run --project tools/AsyncEntryDiffRunner/AsyncEntryDiffRunner.csproj` from the repo root to refresh this
> report after the Raven compiler has been built. The command emits the latest pointer trace and IL comparisons below and exits
> with a non-zero status if either regression diverges from the recorded baselines.

## Pointer trace regression

### Generic entry (single await)

- Source: `docs/investigations/assets/async_entry_generic.rav`
- Compilation: ❔ _(awaiting first run)_
- Execution: ❔ _(awaiting first run)_
- Runtime pointer timeline: ❔ _(awaiting first run)_
- Emitted IL pointer timeline: ❔ _(awaiting first run)_

Baseline timeline:
```
Step21:_state:store -> 0xSTATE
Step21:_builder:addr -> 0xBUILDER
Step21:_builder:load -> 0xBUILDER
Step21:<>awaiter0:store -> 0xAWAITER0
Step21:_state:store -> 0xSTATE
Step21:<>awaiter0:addr -> 0xAWAITER0
Step21:_state:load -> 0xSTATE
Step21:<>awaiter0:load -> 0xAWAITER0
Step21:<>awaiter0:store -> 0xAWAITER0
Step21:_state:store -> 0xSTATE
Step21:_builder:addr -> 0xBUILDER
Step21:_builder:load -> 0xBUILDER
Step21:_state:store -> 0xSTATE
```
_Baseline source: `docs/investigations/snippets/async-entry-step21-generic.log`_

_Runtime pointer timeline and emitted IL pointer timeline will appear here after the first tool execution._

### Multi-await entry

- Source: `docs/investigations/assets/async_entry_multi.rav`
- Compilation: ❔ _(awaiting first run)_
- Execution: ❔ _(awaiting first run)_
- Runtime pointer timeline: ❔ _(awaiting first run)_
- Emitted IL pointer timeline: ❔ _(awaiting first run)_

Baseline timeline:
```
Step15:_state:store -> 0xSTATE
Step15:_builder:addr -> 0xBUILDER
Step15:_builder:load -> 0xBUILDER
Step15:<>awaiter0:store -> 0xAWAITER0
Step15:_state:store -> 0xSTATE
Step15:<>awaiter0:addr -> 0xAWAITER0
Step15:_state:load -> 0xSTATE
Step15:<>awaiter0:load -> 0xAWAITER0
Step15:<>awaiter0:store -> 0xAWAITER0
Step15:<>awaiter1:store -> 0xAWAITER1
Step15:_state:store -> 0xSTATE
Step15:<>awaiter1:addr -> 0xAWAITER1
Step15:_state:load -> 0xSTATE
Step15:<>awaiter1:load -> 0xAWAITER1
Step15:<>awaiter1:store -> 0xAWAITER1
Step15:_state:store -> 0xSTATE
Step15:_builder:addr -> 0xBUILDER
Step15:_builder:load -> 0xBUILDER
Step15:_state:store -> 0xSTATE
```
_Baseline source: `docs/investigations/snippets/async-entry-step15.log`_

_Runtime pointer timeline and emitted IL pointer timeline will appear here after the first tool execution._

## Raven vs. Roslyn MoveNext IL

- Raven compilation: ❔ _(awaiting first run)_
- Roslyn compilation: ❔ _(awaiting first run)_
- Instruction comparison: ❔ _(awaiting first run)_

Run the tool to capture the latest IL for Raven’s `Program.<MainAsync>d__0.MoveNext` and Roslyn’s `Program.<Main>d__0.MoveNext`.
The generated sections will embed both instruction streams and highlight the first mismatched instructions so nightly automation
can flag regressions immediately.
