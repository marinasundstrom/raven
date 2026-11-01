# Diagnostic value contracts

`DiagnosticDescriptor` identity is its ID. Display strings, category, severity,
and enablement do not participate in equality. Typed equality, object equality,
hashing, and `==`/`!=` agree in the C# and Raven implementations. The type retains
its factory API and class representation; positional record equality would change
this contract.

`LocalizableString` supplies typed equality and null-safe equality operators.
Custom implementations retain reference identity unless they override object
equality and hashing. The built-in literal implementation compares text and exact
runtime type. Subclasses with additional state must provide a matching equality
and hash implementation if that state participates in identity.

`LocalizableResourceString` resolves its resource key through the supplied
`ResourceManager` using `CultureInfo.CurrentUICulture` on every `ToString()` or
implicit conversion to string. A missing key returns the empty string; resource
manager failures propagate to the caller. It compares the key, resource-source
type, exact runtime type, and resource-manager reference identity. Its hash does
not depend on translated text or the current culture, so changing display culture
does not invalidate hash-based collections. The manager is an identity-bearing
service; two different manager instances are not assumed interchangeable.

These are compiler API contracts, not new Raven language rules. Shared C# tests
exercise both implementations on modern .NET; native runtime execution is not
claimed. The original C# compiler remains the maintained bootstrap seed.
