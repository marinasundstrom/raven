# Symbol resolution

## GetSymbolInfo

Gets the referenced symbol for an expression node.

Will return a SymbolInfo containing the actual symbol, and if not, it will state a reason.

## GetDeclaredSymbol

Gets the actual symbol for a declaration node, for instance a method declaration from its syntax.

Will directly return the symbol.

## GetTypeInfo

Gets the resolved type for an expression node. Including information about conversions.