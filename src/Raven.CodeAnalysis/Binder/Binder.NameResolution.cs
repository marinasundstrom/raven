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
            GenericNameSyntax or QualifiedNameSyntax => ResolveTypeOrNamespace(name),
            _ => null
        };
    }

    private ISymbol? ResolveTypeOrNamespace(NameSyntax name)
    {
        ResolveTypeResult result;
        using (_diagnostics.CreateNonReportingScope())
            result = BindTypeSyntax(name);

        if (result.Success)
            return result.ResolvedType;

        return ResolveNamespace(name);
    }

    private INamespaceSymbol? ResolveNamespace(NameSyntax name)
    {
        if (name is IdentifierNameSyntax identifier)
            return LookupNamespace(identifier.Identifier.ValueText);

        if (name is QualifiedNameSyntax qualified &&
            ResolveNamespace(qualified.Left) is { } namespaceSymbol &&
            qualified.Right is IdentifierNameSyntax right)
        {
            return namespaceSymbol.LookupNamespace(right.Identifier.ValueText);
        }

        return null;
    }
}
