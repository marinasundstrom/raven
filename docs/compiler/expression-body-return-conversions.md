# Expression-bodied method return conversions

A static or instance `Box<T>(value: T) -> object => value` could omit boxing in the
ordinary .NET generator. The top-level function path already consumed the bound return
block; ordinary methods instead consumed the unconverted expression. Method arrow bodies
now use GetLoweredArrowExpressionBody, preserving the binder's conversions and existing
async/pattern handling instead of adding another emission-only conversion rule.

C# regressions execute static and instance generic methods with Int32, an object whose
identity must be preserved, and null. Together with existing expression-body execution
checks, ten tests pass (2026-10-03). This is independent of NeoCLR metadata, capabilities
and Runtime Contract settings. The same code path exists on main; the isolated fix is
also being validated on codex/compiler-fixes-from-neoclr. No binder behavior changes.
