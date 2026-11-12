# Unary operator type provider

Built-in unary lookup consumes the internal `OperatorTypeProvider` contract:
canonical special types and the error type. `Compilation` implements the contract
with its existing target/runtime-aware type resolution. The default remains .NET;
the contract does not introduce a second type cache or target mapping.

Literal operands expose their underlying type through `ILiteralTypeInfo`, avoiding
a dependency on the concrete source symbol implementation. Lookup preserves the
existing promotion rules, enum complement, error recovery, and checked-negation
descriptor behavior. Operator descriptors retain their existing class identity.

This boundary allows the Raven port and the C# implementation to run the same
C# contract tests with a small type provider, while existing compilation-based
tests exercise the real provider. Binary lookup also needs conversion and target
services and is not covered by this contract extraction.

Validation uses modern .NET. Shared contract tests do not establish execution on
native neoCLR, .NET Framework, or NanoFramework.

The Raven port now includes `BoundUnaryOperator`, its operator-kind enum, and
`OperatorTypeProvider`. The shared C# suite is linked into both parity projects.
All 31 provider contract cases pass against C# and the isolated emitted Raven
translation; six existing compilation-based cases also pass against C#. The
preceding extraction passed 79 tests including adjacent checked-expression and
checked-mutation coverage.

The ongoing full 42-node parity build predates this addition. A subsequent full
Raven parity build must validate this slice in the complete port assembly.
