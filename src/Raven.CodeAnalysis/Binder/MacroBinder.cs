using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class MacroBinder : Binder
{
    private readonly MacroDeclarationSyntax _syntax;
    private IMacroDeclarationSymbol? _symbol;

    public MacroBinder(Binder parent, MacroDeclarationSyntax syntax)
        : base(parent)
    {
        _syntax = syntax;
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
        => node == _syntax
            ? GetMacroSymbol()
            : base.BindDeclaredSymbol(node);

    public override Compilation Compilation
        => ParentBinder?.Compilation
            ?? (_symbol?.ContainingAssembly as SourceAssemblySymbol)?.Compilation
            ?? base.Compilation;

    public override SemanticModel SemanticModel
        => ParentBinder?.SemanticModel
            ?? Compilation.GetSemanticModel(_syntax.SyntaxTree);

    public IMacroDeclarationSymbol GetMacroSymbol()
    {
        if (_symbol is not null)
            return _symbol;

        Compilation.EnsureSourceDeclarationsDeclared();
        return Compilation.TryGetMacroSymbol(_syntax, out _symbol)
            ? _symbol
            : throw new InvalidOperationException("Unable to resolve macro declaration.");
    }
}
