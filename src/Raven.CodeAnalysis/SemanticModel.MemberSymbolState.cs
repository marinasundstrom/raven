using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    internal void RegisterMethodSymbol(MethodDeclarationSyntax node, MethodSymbol symbol)
        => Compilation.RegisterMethodSymbol(node, symbol);

    internal bool TryGetMethodSymbol(MethodDeclarationSyntax node, out MethodSymbol symbol)
        => Compilation.TryGetMethodSymbol(node, out symbol!);

    internal void RegisterMethodSymbol(ConstructorDeclarationSyntax node, MethodSymbol symbol)
        => Compilation.RegisterMethodSymbol(node, symbol);

    internal bool TryGetMethodSymbol(ConstructorDeclarationSyntax node, out MethodSymbol symbol)
        => Compilation.TryGetMethodSymbol(node, out symbol!);

    internal void RegisterMethodSymbol(ParameterlessConstructorDeclarationSyntax node, MethodSymbol symbol)
        => Compilation.RegisterMethodSymbol(node, symbol);

    internal bool TryGetMethodSymbol(ParameterlessConstructorDeclarationSyntax node, out MethodSymbol symbol)
        => Compilation.TryGetMethodSymbol(node, out symbol!);

    internal void RegisterMethodSymbol(FunctionStatementSyntax node, MethodSymbol symbol)
        => Compilation.RegisterMethodSymbol(node, symbol);

    internal bool TryGetMethodSymbol(FunctionStatementSyntax node, out MethodSymbol symbol)
        => Compilation.TryGetMethodSymbol(node, out symbol!);

    internal void RegisterMacroDeclarationSymbol(MacroDeclarationSyntax node, MacroDeclarationSymbol symbol)
        => Compilation.RegisterMacroDeclarationSymbol(node, symbol);

    internal bool TryGetMacroSymbol(MacroDeclarationSyntax node, out MacroDeclarationSymbol symbol)
        => Compilation.TryGetMacroSymbol(node, out symbol!);

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

    internal void RegisterPropertySymbol(PropertyDeclarationSyntax node, PropertySymbol symbol)
        => Compilation.RegisterPropertySymbol(node, symbol);

    internal bool TryGetPropertySymbol(PropertyDeclarationSyntax node, out PropertySymbol symbol)
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

    internal void RegisterEventSymbol(EventDeclarationSyntax node, EventSymbol symbol)
        => Compilation.RegisterEventSymbol(node, symbol);

    internal bool TryGetEventSymbol(EventDeclarationSyntax node, out EventSymbol symbol)
        => Compilation.TryGetEventSymbol(node, out symbol!);
}
