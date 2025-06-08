# Type Union Analyzer for C#

The purpose of this analyzer is to enable the `TypeUnionAttribute` for C#, and to allow for basic type checking.

For every usage of an attribute with signature `[TypeUnionAttribute(params Type[] types)]`, diagnostics will be reported so to discover them more easily.

Since type unions are built on `object`, similar to type `dynamic`, it is not possible to overload on parameter representing type unions.