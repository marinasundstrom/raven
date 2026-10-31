# Grammar

The following is the non-normative EBNF grammar for Raven.

Unmarked productions describe shared syntax. Exclusive alternatives carry
**.NET only** or **neoCLR only** comments; for example, `func` as a generic
category constraint is neoCLR only. Parsing and syntax highlighting recognize
Raven source independently of target selection. Binding and target capability
diagnostics determine whether a construct is valid for the selected target.
The `A & B` spelling is shared in generic constraints, where .NET treats it as
ordinary conjunctive bounds. Intersection types in value/signature positions are
**neoCLR only**. Structural union syntax `A | B` is shared; its runtime mapping
is target-specific.

See [target availability](../../compiler/target-platforms.md#reading-target-availability).


[Source file](grammar.ebnf)

[!code-ebnf[](grammar.ebnf)]