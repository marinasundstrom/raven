# Symbol display configuration

`SymbolDisplayFormat` is a reference-type value object. Its eleven display options
and exact runtime type define equality. Equal configurations compare equal through
`IEquatable<SymbolDisplayFormat>`, `Equals(object)`, `==` and `!=`, and have equal
hash codes. Null is unequal to a non-null configuration. Distinct derived runtime
types do not compare equal to the base configuration.

The predefined formats preserve their existing options. Every `With…` method
returns a distinct shallow clone with one option replaced, preserving the source
and all other options. Public getters and private setters retain the existing
configuration boundary. Callers should use the `With…` methods to customize a
preset, rather than relying on reference identity to compare configurations.

The C# bootstrap and Raven implementation expose the same configuration API.
Raven uses `val` properties with explicit private setters, consistent with its
public writability rules. The type remains a class with explicit value members:
this preserves the API established by its initial port. Raven records now admit
body storage and constructors, so the former language restriction is no longer
an obstacle to a later, separately validated conversion.

Shared C# tests validate cloning, every option, null handling, equality, hash
consistency and accessors against both implementations. A differential test also
compares all preset options and enum names/values with the bootstrap assembly.
The rendering implementation is also translated to Raven, with shared C# contract
coverage for types, members, signatures, constants, modifiers and qualification.
The translation consumes port-owned symbol interfaces; concrete binding and
metadata providers remain later porting work. Full integration acceptance is
recorded in the port progress ledger.

Type rendering preserves grouping: a structural union nested inside an
intersection is displayed as `(A | B) & C`. Intersection types are neoCLR-only;
this display contract does not imply native runtime execution. Function parameter
rendering recognizes unit through `IUnitTypeIdentity`, allowing both compiler
implementations to preserve the same unit shape without requiring a concrete
bootstrap symbol class.
