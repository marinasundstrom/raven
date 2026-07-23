using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal abstract partial class Binder
{
    protected ISymbol? ResolveName(NameSyntax name)
    {
        return name switch
        {
            IdentifierNameSyntax id => LookupSymbol(id.Identifier.ValueText)
                ?? (ISymbol?)LookupNamespace(id.Identifier.ValueText)
                ?? LookupType(id.Identifier.ValueText),
            GenericNameSyntax gen => ResolveGenericName(gen),
            QualifiedNameSyntax qn => ResolveQualifiedName(qn),
            _ => null
        };
    }

    private ISymbol? ResolveGenericName(GenericNameSyntax gen)
    {
        var arity = ComputeGenericArity(gen);
        var typeArgs = ResolveGenericTypeArguments(gen);

        var symbol = FindNamedTypeForGeneric(gen, arity);

        if (symbol is null)
            return null;

        if (!ValidateTypeArgumentConstraints(symbol, typeArgs, i => GetTypeArgumentLocation(gen.TypeArgumentList.Arguments, gen.GetLocation(), i), symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
            return Compilation.ErrorTypeSymbol;

        return TryConstructGeneric(symbol, typeArgs, arity) ?? symbol;
    }

    private ISymbol? ResolveQualifiedName(QualifiedNameSyntax qn)
    {
        var left = ResolveName(qn.Left);

        if (left is INamespaceSymbol ns)
        {
            if (qn.Right is IdentifierNameSyntax id)
                return (ISymbol?)ns.LookupNamespace(id.Identifier.ValueText)
                    ?? SelectByArity(ns.GetMembers(id.Identifier.ValueText)
                        .OfType<INamedTypeSymbol>(), 0)
                    ?? ns.LookupType(id.Identifier.ValueText);

            if (qn.Right is GenericNameSyntax gen)
                return ResolveGenericMember(ns, gen);

            return null;
        }

        if (left is ITypeSymbol type)
        {
            if (qn.Right is IdentifierNameSyntax id)
                return SelectByArity(type.GetMembers(id.Identifier.ValueText)
                    .OfType<INamedTypeSymbol>(), 0);

            if (qn.Right is GenericNameSyntax gen)
                return ResolveGenericMember(type, gen);
        }

        return null;
    }
}
