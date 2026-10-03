# Explicit unit arguments in overload resolution

Development fix, 2026-10-03. A RuntimeUnitContract may select an inhabited metadata
type, including the NeoCLR bootstrap System.Void representation. Constructor overload
resolution previously rejected arguments with SpecialType.System_Void unconditionally,
so a generic union case with a configured unit payload could not bind.

Argument validation now rejects special Void unless the exact type is the resolved
RuntimeUnitContract representation. Ordinary .NET behavior without that explicit contract
is unchanged. No target name checks or imported metadata access are introduced into
binding. This does not allow arbitrary CLI void arguments or change callable no-result
semantics; emission remains responsible for materializing values in value positions.

The regression checks a generic union constructor with and without the explicit contract.
The positive case failed before the fix with RAV1501. All 16 focused RuntimeUnitContract
and NeoClrUnitContract tests pass afterward. Native execution is separately checked by
the unit-storage driver, which now carries the unit through a generic union payload and
pattern matching as well as an out parameter and an ordinary function argument.
