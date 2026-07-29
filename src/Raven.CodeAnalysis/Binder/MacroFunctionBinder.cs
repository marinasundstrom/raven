using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class MacroFunctionBinder : Binder
{
    private readonly MacroFunctionDeclarationSyntax _syntax;
    private IMacroFunctionSymbol? _symbol;

    public MacroFunctionBinder(Binder parent, MacroFunctionDeclarationSyntax syntax)
        : base(parent)
    {
        _syntax = syntax;
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
        => node == _syntax
            ? GetMacroFunctionSymbol()
            : base.BindDeclaredSymbol(node);

    public override Compilation Compilation
        => ParentBinder?.Compilation
            ?? (_symbol?.ContainingAssembly as SourceAssemblySymbol)?.Compilation
            ?? base.Compilation;

    public override SemanticModel SemanticModel
        => ParentBinder?.SemanticModel
            ?? Compilation.GetSemanticModel(_syntax.SyntaxTree);

    public IMacroFunctionSymbol GetMacroFunctionSymbol()
    {
        if (_symbol is not null)
            return _symbol;

        Compilation.EnsureSourceDeclarationsDeclared();
        return Compilation.TryGetMacroFunctionSymbol(_syntax, out _symbol)
            ? _symbol
            : throw new InvalidOperationException("Unable to resolve macro function declaration.");
    }
}
