# DefaultParameterValueAttribute support for optional arguments

## Goal
Identify the compiler changes needed to honor `System.Runtime.InteropServices.DefaultParameterValueAttribute` as a source of optional parameter defaults. This must cover metadata import (where the attribute typically appears) and the downstream binding and overload-resolution steps that rely on `HasExplicitDefaultValue`/`ExplicitDefaultValue` to permit omitted arguments.

## Current behavior
- **Source-declared defaults** — Parameter default syntax is evaluated during binding and recorded on `SourceParameterSymbol`. Defaults must be constant, and once any default appears, all remaining parameters must also supply defaults; otherwise diagnostics are emitted. Optional status is tracked via `HasExplicitDefaultValue`, and `ExplicitDefaultValue` stores the literal used by invocation binding when materializing omitted arguments. 
- **Invocation and overload resolution** — Candidates are rejected when required parameters lack supplied arguments unless `HasExplicitDefaultValue` is true. `TryMatch` accounts for missing arguments and logs when a default is used, while `TryMapArguments` requires all unmapped parameters to have defaults. When binding a call, `CreateOptionalArgument` turns `ExplicitDefaultValue` into a literal expression for the missing argument, but only supports a narrow set of primitive shapes (`bool`, `string`, `char`, numeric primitives) and `null`.
- **Metadata import** — `PEParameterSymbol` relies on `ParameterInfo.RawDefaultValue` to decide whether a metadata parameter is optional, treating the parameter as optional only when the runtime default is neither `DBNull.Value` nor `Type.Missing`. No explicit inspection of custom attributes is performed, so `DefaultParameterValueAttribute` is ignored unless it flows through `RawDefaultValue` (which often reports `Type.Missing` when the parameter lacks a metadata default constant).

## Gaps around DefaultParameterValueAttribute
- .NET methods that rely solely on `DefaultParameterValueAttribute` to establish a default value expose `ParameterInfo.RawDefaultValue` as `Type.Missing`, so `HasExplicitDefaultValue` stays false. Such parameters are treated as required, causing overload resolution and argument mapping to reject invocations that omit the argument even though other .NET callers see a default.
- Even if a default value is detected, invocation binding can currently fail to synthesize the missing argument when the attribute supplies a constant outside the limited literal set (for example, enums, decimals, typed nulls, or `DateTime`).
- Name-based argument mapping is already capable of placing omitted arguments, but it depends entirely on `HasExplicitDefaultValue`; ignoring the attribute means named calls cannot skip those parameters either.

## Implementation sketch
- **Status:**
  - ✅ Attribute decoding in metadata symbols (optional defaults flow from `DefaultParameterValueAttribute`/`OptionalAttribute`).
  - ✅ Default literal synthesis for metadata defaults, including enums, decimal, and `DateTime` constants.
  - ✅ Invocation and overload resolution honor defaults imported from `DefaultParameterValueAttribute`.
  - ✅ Diagnostics and semantic model parity for defaults supplied via `DefaultParameterValueAttribute`.
- **Attribute decoding in metadata symbols** — Extend `PEParameterSymbol` to scan `GetCustomAttributesData()` for `DefaultParameterValueAttribute` (and possibly `OptionalAttribute` as a fallback). When present, extract the constructor argument as the default, set `HasExplicitDefaultValue` to true, and prefer it when `RawDefaultValue` is `Type.Missing`. Preserve existing handling for metadata-encoded default constants so reflection and attribute paths both work.
- **Default literal synthesis** — Broaden `CreateOptionalArgument` to construct literals (or constant conversions) for additional constant kinds: enums (using their underlying value), decimal, `DateTime`, and other primitive-backed constants commonly emitted by `DefaultParameterValueAttribute`. Consider reusing the constant folding/conversion pipeline instead of hard-coding types to ensure parity with source defaults.
- **Invocation and overload resolution checks** — No structural changes are required, but defaults discovered via the attribute must flow into `HasExplicitDefaultValue`/`ExplicitDefaultValue` so that positional and named-argument mapping, as well as `TryMatch`, allow the omission. Validation coverage should include calls that skip parameters defined only via `DefaultParameterValueAttribute`.
- **Diagnostics and semantic model** — Ensure diagnostics that mention optional/defaulted parameters (such as trailing default enforcement) recognize defaults sourced from metadata attributes, and surface the default value via the semantic model for tooling parity.

## Validation ideas
- Add metadata-based tests that import a .NET method using only `DefaultParameterValueAttribute` and verify calls without the argument bind successfully, including named-argument cases. Cover uncommon constant shapes (e.g., `decimal`, enums, `DateTime`, `null`) to validate literal synthesis. Retain regression coverage for existing default-constant metadata and source-declared defaults to confirm no regressions.
