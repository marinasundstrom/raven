# Raven generator model and serializer

This is the Raven translation of `tools/NodesShared`, maintained beside the C#
bootstrap version. `NodesShared.Raven.rvnproj` is a standalone .NET library; the
production generators continue using the C# project until their ports are ready.

Model objects remain mutable classes with reference identity and independent
collection instances. They represent a model being assembled and edited; record
equality over mutable collection identities would not provide structural value
semantics. Private serialization helpers are explicitly private in Raven.

The serializer uses `XName.Get` in both languages: .NET's implicit string-to-XName
operator declares a nullable return refined by `NotNullIfNotNull`, which Raven
intentionally does not use for flow narrowing. `EqualityComparer<T?>` preserves
nullable equality without nullable member access. Guarded values use `!` at the
non-null `XAttribute` boundary; this follows Raven's existing explicit-nullability
rules rather than changing them during the port.

Run `scripts/test-generator-parity.sh` for shared C# tests against both libraries.
