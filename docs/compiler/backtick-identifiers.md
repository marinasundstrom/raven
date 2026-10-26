# Backtick identifiers

The lexer produces an ordinary `IdentifierToken` for a backtick-delimited name.
Its `Text` and width include both delimiters, while its value contains the exact
interior spelling. Binding, lookup, and emission use that value, sharing the same
symbol namespace as ordinary and `@`-escaped names. No normalization is applied.

Syntax factories accept the escaped source spelling and preserve the same
Text/ValueText distinction, including for syntax returned by macros. Completion,
escaped symbol display, and interface implementation generation quote names that
cannot be written as ordinary identifiers. No new syntax-node kinds or operations
are required; semantic queries continue through the ordinary compiler APIs.

The lexer diagnoses empty names and missing closing backticks, stopping before
any source line break. Backticks cannot be embedded in a name. The language does
not add target-specific name mangling: metadata consumers and target runtimes may
have their own naming restrictions.

Validation covers lexical preservation and recovery, symbol lookup and duplicate
names, macro-generated references, completion insertion, and exact emitted method
names with invocation on modern .NET. This does not establish execution support
on .NET Framework, NanoFramework, or native neoCLR.
