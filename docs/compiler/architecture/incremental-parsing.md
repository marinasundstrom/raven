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

