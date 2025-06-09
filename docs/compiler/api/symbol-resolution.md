# Symbol resolution

From the `SemanticModel` you can resolve symbols using the following methods.

## GetSymbolInfo(SyntaxNode node)

Gets the referenced symbol for an expression node.

Will return a SymbolInfo containing the actual symbol, and if not, it will state a reason.

## GetDeclaredSymbol(SyntaxNode node)

Gets the actual symbol for a declaration node, for instance a method or type declaration from its syntax.

Will directly return the declared symbol.

## GetTypeInfo(ExpressionSyntax node)

Gets the resolved type for an expression node. Including information about conversions.

## Internal

### GetBoundNode(ExpressionSyntax node)

Gets the actual bound node from the semantic tree.

This is used internally, such as when doing code generation.