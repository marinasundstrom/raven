using System.Collections.Immutable;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class SourceSymbol : Symbol
{
    protected SourceSymbol(SymbolKind kind, string name, ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Name = name;
    }

    public override string Name { get; }
}