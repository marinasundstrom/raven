using System.Collections.Immutable;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class SourceSymbol : Symbol
{
    protected SourceSymbol(SymbolKind kind, string name, ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(kind, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
    }

    public override Compilation Compilation => ContainingSymbol!.Compilation;
}