# Step 22 Roslyn diff dashboard sample

The diff runner populates the dashboard with the latest pointer and IL results. A successful run renders a table similar to the
following:

| Permutation | Source asset | Baseline log | Step | Compilation | Execution | Runtime timeline | IL timeline |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Generic entry (single await) | `docs/investigations/assets/async_entry_generic.rav` | `docs/investigations/snippets/async-entry-step21-generic.log` | Step21 | ✅ (exit 0) | ✅ (exit 42) | ✅ | ✅ |
| Multi-await entry | `docs/investigations/assets/async_entry_multi.rav` | `docs/investigations/snippets/async-entry-step15.log` | Step15 | ✅ (exit 0) | ✅ (exit 42) | ✅ | ✅ |
| Async lambda (single await) | `docs/investigations/assets/async_lambda.rav` | `docs/investigations/snippets/async-entry-step23-lambda.log` | Step23/lambda | ✅ (exit 0) | ✅ (exit 42) | ✅ | ✅ |

### MoveNext IL diff summary

- Raven compilation: ✅ (exit 0)
- Roslyn compilation: ✅ (exit 0)
- Instruction comparison: ✅ (96 vs. 96)

These values mirror the nightly pointer trace report and allow the Roslyn diff dashboard to surface async-entry regressions next
to existing semantic and diagnostic deltas.
