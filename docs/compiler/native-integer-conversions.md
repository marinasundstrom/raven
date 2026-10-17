# Native integral conversions

Raven treats conversions between integral types and `nint`/`nuint` as built-in
numeric conversions, independent of nominal IntPtr/UIntPtr conversion operators.
This slice covers integral operands; it does not claim complete floating-point or
decimal native-integer conversion coverage.

Implicit conversions to nint accept sbyte, byte, short, ushort, int and char;
implicit conversions to nuint accept byte, ushort, uint and char. Native signed
integers widen implicitly to long, and native unsigned integers to ulong. Other
integral pairs involving native integers require explicit casts. Target-typed native
integer literal assignment remains incomplete; use explicit casts. Target-specific character rules still
apply, including neoCLR's nonnumeric grapheme Char.

These rules follow the platform-independent integral subset of the
[.NET/C# numeric conversion rules](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/numeric-conversions).
The compiler does not assume native storage is 64 bits. CLI emission preserves source
signedness when widening or changing native storage category. Checked conversions use
standard overflow-checked instructions; .NET reports OverflowException. Native backend
support and fault behavior require their own qualification.

Focused semantic checks classify conversions without user-defined operators, and
execution tests cover signed widening, high UInt32 bits and checked negative-to-nuint
conversion. Current execution validation is on a 64-bit host; 32-bit runtime execution
remains unverified. No new Runtime Contract setting is introduced.
