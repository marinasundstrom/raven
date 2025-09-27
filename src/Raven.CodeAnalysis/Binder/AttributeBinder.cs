using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class AttributeBinder : BlockBinder
{
    private readonly ISymbol _owner;

    public AttributeBinder(ISymbol owner, Binder parent)
        : base(owner, parent)
    {
        _owner = owner;
    }

    public override ISymbol ContainingSymbol => _owner;

    public override SymbolInfo BindSymbol(SyntaxNode node)
    {
        if (node is AttributeSyntax attribute)
        {
            var bound = BindAttribute(attribute);
            return bound.GetSymbolInfo();
        }

        return base.BindSymbol(node);
    }

    internal BoundExpression BindAttribute(AttributeSyntax attribute)
    {
        if (TryGetCachedBoundNode(attribute) is BoundExpression cached)
            return cached;

        var bound = BindAttributeCore(attribute);
        CacheBoundNode(attribute, bound);
        return bound;
    }

    private BoundExpression BindAttributeCore(AttributeSyntax attribute)
    {
        var attributeType = TryResolveAttributeType(attribute.Name);

        if (attributeType is null)
        {
            var typeExpression = BindTypeSyntax(attribute.Name);

            if (typeExpression is not BoundTypeExpression boundType || boundType.Type is not INamedTypeSymbol resolvedType)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);

            attributeType = resolvedType;
        }

        if (attributeType.TypeKind != TypeKind.Error)
        {
            var validated = EnsureTypeAccessible(attributeType, attribute.Name.GetLocation());
            if (validated.TypeKind == TypeKind.Error)
                return new BoundErrorExpression(attributeType, null, BoundExpressionReason.Inaccessible);
        }

        var argumentList = attribute.ArgumentList;
        var argumentSyntaxes = argumentList?.Arguments ?? SeparatedSyntaxList<ArgumentSyntax>.Empty;

        var boundArguments = ImmutableArray.CreateBuilder<BoundExpression>(argumentSyntaxes.Count);
        bool hasErrors = false;

        for (int i = 0; i < argumentSyntaxes.Count; i++)
        {
            var argumentSyntax = argumentSyntaxes[i];
            var boundArgument = BindExpression(argumentSyntax.Expression);

            if (boundArgument is BoundErrorExpression)
                hasErrors = true;

            boundArguments.Add(boundArgument);
        }

        if (hasErrors)
            return new BoundErrorExpression(attributeType, null, BoundExpressionReason.ArgumentBindingFailed);

        var arguments = boundArguments.ToArray();
        var resolution = OverloadResolver.ResolveOverload(attributeType.Constructors, arguments, Compilation);

        if (resolution.Success)
        {
            var constructor = resolution.Method!;

            if (!EnsureMemberAccessible(constructor, attribute.Name.GetLocation(), "constructor"))
                return new BoundErrorExpression(attributeType, constructor, BoundExpressionReason.Inaccessible);

            var converted = ConvertArguments(constructor.Parameters, arguments, argumentSyntaxes);
            return new BoundObjectCreationExpression(constructor, converted);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(attributeType.Name, resolution.AmbiguousCandidates, attribute.GetLocation());
            return new BoundErrorExpression(attributeType, null, BoundExpressionReason.Ambiguous);
        }

        _diagnostics.ReportNoOverloadForMethod(attributeType.Name, arguments.Length, attribute.GetLocation());
        return new BoundErrorExpression(attributeType, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    private INamedTypeSymbol? TryResolveAttributeType(TypeSyntax attributeName)
    {
        var hasAttributeSuffix = HasAttributeSuffix(attributeName);

        if (!hasAttributeSuffix)
        {
            var withSuffix = TryLookupAttributeType(attributeName, appendAttributeSuffix: true);
            if (withSuffix is not null)
                return withSuffix;
        }

        return TryLookupAttributeType(attributeName, appendAttributeSuffix: false);
    }

    private INamedTypeSymbol? TryLookupAttributeType(TypeSyntax attributeName, bool appendAttributeSuffix)
    {
        switch (attributeName)
        {
            case IdentifierNameSyntax identifier:
            {
                var candidateName = AppendAttributeSuffixIfNeeded(identifier.Identifier.ValueText, appendAttributeSuffix);
                return FindAccessibleNamedType(candidateName, 0);
            }

            case QualifiedNameSyntax qualified when qualified.Right is IdentifierNameSyntax rightIdentifier:
            {
                var container = TryLookupNamespaceOrType(qualified.Left);
                if (container is null)
                    return null;

                var candidateName = AppendAttributeSuffixIfNeeded(rightIdentifier.Identifier.ValueText, appendAttributeSuffix);
                return container.LookupType(candidateName) as INamedTypeSymbol;
            }
        }

        return null;
    }

    private INamespaceOrTypeSymbol? TryLookupNamespaceOrType(TypeSyntax syntax)
    {
        switch (syntax)
        {
            case IdentifierNameSyntax identifier:
            {
                var ns = LookupNamespace(identifier.Identifier.ValueText);
                if (ns is not null)
                    return ns;

                var type = FindAccessibleNamedType(identifier.Identifier.ValueText, 0);
                if (type is not null)
                    return type;

                return LookupType(identifier.Identifier.ValueText) as INamespaceOrTypeSymbol;
            }

            case QualifiedNameSyntax qualified when qualified.Right is IdentifierNameSyntax rightIdentifier:
            {
                var left = TryLookupNamespaceOrType(qualified.Left);
                if (left is null)
                    return null;

                if (left is INamespaceSymbol namespaceSymbol)
                {
                    var nestedNamespace = namespaceSymbol.LookupNamespace(rightIdentifier.Identifier.ValueText);
                    if (nestedNamespace is not null)
                        return nestedNamespace;
                }

                return left.LookupType(rightIdentifier.Identifier.ValueText);
            }
        }

        return null;
    }

    private static string AppendAttributeSuffixIfNeeded(string name, bool append)
    {
        if (!append)
            return name;

        return name.EndsWith("Attribute", StringComparison.Ordinal)
            ? name
            : string.Concat(name, "Attribute");
    }

    private static bool HasAttributeSuffix(TypeSyntax syntax)
    {
        return syntax switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText.EndsWith("Attribute", StringComparison.Ordinal),
            GenericNameSyntax generic => generic.Identifier.ValueText.EndsWith("Attribute", StringComparison.Ordinal),
            QualifiedNameSyntax qualified => HasAttributeSuffix(qualified.Right),
            _ => false
        };
    }
}
