# Incremental Parsing

The compiler is designed to intelligently detect changes in the source code, re-parsing only the affected sections and replacing the corresponding nodes with updated ones.

## How It Works

When both the new and old source texts are available, along with the spans where changes occurred:

1. From the original syntax tree (based on the old source text), identify the innermost node within the change span.

   Example hierarchy:
   `GlobalStatementSyntax > IfStatementSyntax > BlockSyntax > ReturnStatementSyntax > IdentifierNameSyntax`

2. Use the position of this original node as the starting point for parsing the new source text.

3. Parse the modified text span to produce a new node.

4. Replace the original node with the newly parsed node.

This process generates an updated syntax tree.

### Example

This example demonstrates a common scenario where a numeric literal is replaced with an identifier.

```
if (isValid) {
    return 0;
}
```

Let us change `0` to `result`:

```
if (isValid) {
    return result;
}
```

The hierarchy of the original syntax tree is:
`GlobalStatementSyntax > IfStatementSyntax > BlockSyntax > ReturnStatementSyntax > NumericLiteralExpressionSyntax`

1. We identify the `NumericLiteralExpressionSyntax` node affected by the change.
2. Use its start position to parse the new source text.
3. Replace the `NumericLiteralExpressionSyntax` node (0) with a new node of type `IdentifierNameSyntax` (result).

### Special Scenario

This section highlights a unique challenge when adding clauses to `IfStatementSyntax`, as it involves detecting and handling changes that introduce entirely new syntax elements, which impacts how and where parsing begins.

This may be dealt with in other ways in the future.

Consider this:

```
if (isValid) {
    return 0;
}
```

What if we add an `else if` statement?

```
if (isValid) {
    return 0;
} else if (isComplete) {
    return 1;
}
```

Here the changed span has length 0, meaning that something new has been added.

1. Select the parent of the identified node (in this case, `IfStatementSyntax`).
2. Use its start position as the place to begin parsing the new source text.
3. The parent node has been re-parsed and the  `else if` clause is incorporated with the updated `IfStatementSyntax` node, resulting in an updated syntax tree.

While this approach simplifies handling such scenarios, there is room for future optimization.

## Considerations

Additional factors must be accounted for, such as determining when it’s necessary to re-parse the entire syntax tree.

## Fallback policy and stabilization

Incremental parsing is an optimization; a fallback must produce the same syntax
and diagnostics as parsing the current document from scratch. Released editor
hosts therefore reparse the complete document when the incremental parser cannot
safely update a fragment. The resulting tree records a reason such as an existing
recovery node, a fragment parse failure, or a reconstructed-text mismatch. The
language server logs that reason together with the document and project versions
so fallback frequency and reproducible edit sequences remain visible during
stabilization.

Compiler tests can set the internal
`ParseOptions.ThrowOnIncrementalParseFallback` option. In that strict mode a
required fallback throws `IncrementalParseFallbackException`, including its
reason, instead of reparsing. Use strict mode in focused incremental tests to
surface unexpected fallback paths; keep ordinary product and parity tests in the
default resilient mode to verify that users still receive authoritative results.

### Ownership boundaries

`SyntaxTree.WithChangedText` owns syntax integration. It first widens an edit to
the nearest ancestor that the fragment parser can parse and replace exactly. If
that still cannot reconstruct the current source, the syntax tree owns the
full-document parse. Workspace and editor code must not duplicate this decision.

The workspace owns immutable document and project snapshots and tells the
compilation which trees changed or were reused. The compilation owns semantic
state transfer and invalidation: unchanged trees and matched executable owners
may retain reusable descriptors, while declaration-sensitive changes block
unsafe transfer and are rebound lazily. A syntax fallback does not by itself
justify reparsing or eagerly rebinding the entire solution.

The language server is an observer at this boundary. It logs the compiler-owned
fallback reason and presents the resulting diagnostics and semantic answers; it
does not select parse regions or bypass compiler binding APIs.
