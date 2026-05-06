using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    internal void RegisterMethodSymbol(MethodDeclarationSyntax node, IMethodSymbol symbol)
        => Compilation.RegisterMethodSymbol(node, symbol);

    internal bool TryGetMethodSymbol(MethodDeclarationSyntax node, out IMethodSymbol symbol)
        => Compilation.TryGetMethodSymbol(node, out symbol!);

    internal SourceMethodSymbol GetOrCreateMethodSymbolForBinding(
        MethodDeclarationSyntax node,
        Func<SourceMethodSymbol> createSymbol)
    {
        if (TryGetMethodSymbol(node, out var existingSymbol) &&
            existingSymbol is SourceMethodSymbol existingSourceSymbol)
        {
            return existingSourceSymbol;
        }

        var createdSymbol = createSymbol();
        RegisterMethodSymbol(node, createdSymbol);
        return createdSymbol;
    }

    internal void RegisterPropertySymbol(PropertyDeclarationSyntax node, IPropertySymbol symbol)
        => Compilation.RegisterPropertySymbol(node, symbol);

    internal bool TryGetPropertySymbol(PropertyDeclarationSyntax node, out IPropertySymbol symbol)
        => Compilation.TryGetPropertySymbol(node, out symbol!);

    internal SourcePropertySymbol GetOrCreatePropertySymbolForBinding(
        PropertyDeclarationSyntax node,
        Func<SourcePropertySymbol> createSymbol)
    {
        if (TryGetPropertySymbol(node, out var existingSymbol) &&
            existingSymbol is SourcePropertySymbol existingSourceSymbol)
        {
            return existingSourceSymbol;
        }

        var createdSymbol = createSymbol();
        RegisterPropertySymbol(node, createdSymbol);
        return createdSymbol;
    }
}
