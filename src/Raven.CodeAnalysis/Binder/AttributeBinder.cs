using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

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

    public override BoundNode GetOrBind(SyntaxNode node)
    {
        if (node is AttributeSyntax attribute)
            return BindAttribute(attribute);

        return base.GetOrBind(node);
    }

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
        if (HasIncompleteAttributeName(attribute.Name))
            return ErrorExpression(reason: BoundExpressionReason.NotFound);

        var attributeType = BindAttributeType(attribute.Name);

        if (attributeType is null)
        {
            var typeExpression = BindTypeSyntaxAsExpression(attribute.Name);

            if (typeExpression is not BoundTypeExpression boundType || boundType.Type is not INamedTypeSymbol resolvedType)
                return ErrorExpression(reason: BoundExpressionReason.NotFound);

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

        var boundArguments = ImmutableArray.CreateBuilder<BoundArgument>(argumentSyntaxes.Count);
        var namedAssignments = ImmutableArray.CreateBuilder<BoundObjectInitializerEntry>();
        bool hasErrors = false;

        for (int i = 0; i < argumentSyntaxes.Count; i++)
        {
            var argumentSyntax = argumentSyntaxes[i];
            var name = argumentSyntax.NameColon?.Name.Identifier.ValueText;

            // In attribute argument lists, named entries can target assignable
            // instance properties/fields on the attribute type.
            if (!string.IsNullOrEmpty(name) &&
                TryBindAttributeNamedArgument(attributeType, name, argumentSyntax.Expression, out var assignmentEntry, ref hasErrors))
            {
                namedAssignments.Add(assignmentEntry);
                continue;
            }

            var targetType = TryGetAttributeConstructorParameterType(attributeType, argumentSyntaxes, i);
            var enumTargetType = targetType ?? TryGetAttributeConstructorEnumParameterType(attributeType, argumentSyntaxes, i);
            BoundExpression boundArgumentExpression;
            if (enumTargetType is not null &&
                TryBindAttributeEnumConstant(argumentSyntax.Expression, enumTargetType, out var enumConstant))
            {
                boundArgumentExpression = enumConstant;
            }
            else if (TryBindImportedAttributeEnumConstant(argumentSyntax.Expression, out enumConstant))
            {
                boundArgumentExpression = enumConstant;
            }
            else
            {
                boundArgumentExpression = targetType is null
                    ? BindExpression(argumentSyntax.Expression)
                    : BindAttributeArgumentExpression(argumentSyntax.Expression, targetType);
            }

            var hasArgumentErrors = boundArgumentExpression is BoundErrorExpression ||
                boundArgumentExpression.Type?.ContainsErrorType() == true;

            if (hasArgumentErrors)
            {
                hasErrors = true;
            }
            else if (!AttributeDataFactory.TryCreateTypedConstant(boundArgumentExpression, out _))
            {
                _diagnostics.ReportAttributeArgumentMustBeConstant(argumentSyntax.Expression.GetLocation());
                boundArgumentExpression = new BoundErrorExpression(
                    boundArgumentExpression.Type ?? Compilation.ErrorTypeSymbol,
                    null,
                    BoundExpressionReason.ConstantExpected);
                hasErrors = true;
            }

            if (string.IsNullOrEmpty(name))
                name = null;

            var isSpread = argumentSyntax.DotDotDotToken.Kind == SyntaxKind.DotDotDotToken;
            boundArguments.Add(new BoundArgument(boundArgumentExpression, RefKind.None, name, argumentSyntax, isSpread));
        }

        if (hasErrors)
            return new BoundErrorExpression(attributeType, null, BoundExpressionReason.ArgumentBindingFailed);

        var arguments = boundArguments.ToArray();
        var resolution = OverloadResolver.ResolveOverload(attributeType.Constructors, arguments, Compilation, binder: this, callSyntax: attribute);

        if (resolution.Success)
        {
            var constructor = resolution.Method!;

            if (!EnsureMemberAccessible(constructor, attribute.Name.GetLocation(), "constructor"))
                return new BoundErrorExpression(attributeType, constructor, BoundExpressionReason.Inaccessible);

            var converted = ConvertArguments(constructor.Parameters, arguments);
            var initializer = namedAssignments.Count > 0
                ? new BoundObjectInitializer(namedAssignments.ToImmutable())
                : null;
            return new BoundObjectCreationExpression(constructor, converted, initializer: initializer);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(attributeType.Name, resolution.AmbiguousCandidates, attribute.GetLocation());
            return new BoundErrorExpression(
                attributeType,
                null,
                BoundExpressionReason.Ambiguous,
                ImmutableArray.CreateRange(resolution.AmbiguousCandidates, static c => (ISymbol)c));
        }

        _diagnostics.ReportNoOverloadForMethod("constructor for type", attributeType.Name, arguments.Length, attribute.GetLocation());
        return new BoundErrorExpression(attributeType, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    private BoundExpression BindAttributeArgumentExpression(ExpressionSyntax expression, ITypeSymbol targetType)
    {
        using var _ = PushTargetType(targetType);
        return BindExpression(expression);
    }

    private bool TryBindAttributeEnumConstant(ExpressionSyntax expression, ITypeSymbol targetType, out BoundExpression bound)
    {
        bound = null!;

        targetType = targetType.UnwrapLiteralType() ?? targetType;
        if (targetType is not INamedTypeSymbol { TypeKind: TypeKind.Enum } enumType)
            return false;

        var memberName = expression switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            MemberBindingExpressionSyntax memberBinding => memberBinding.Name.Identifier.ValueText,
            _ => null
        };

        if (string.IsNullOrWhiteSpace(memberName))
            return false;

        var field = GetFieldsByName(enumType, memberName)
            .FirstOrDefault()
            ?? new SymbolQuery(memberName, enumType, IsStatic: true)
                .Lookup(this)
                .OfType<IFieldSymbol>()
                .FirstOrDefault();
        if (field is null)
            return false;

        bound = new BoundMemberAccessExpression(new BoundTypeExpression(enumType), field);
        return true;
    }

    private static IEnumerable<IFieldSymbol> GetFieldsByName(INamedTypeSymbol type, string name)
    {
        foreach (var member in type.GetMembers(name).OfType<IFieldSymbol>())
            yield return member;

        foreach (var member in type.GetMembers().OfType<IFieldSymbol>())
        {
            if (member.Name == name)
                yield return member;
        }
    }

    private bool TryBindImportedAttributeEnumConstant(ExpressionSyntax expression, out BoundExpression bound)
    {
        bound = null!;

        if (expression is not IdentifierNameSyntax identifier)
            return false;

        var memberName = identifier.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(memberName))
            return false;

        var field = LookupSymbols(memberName)
            .OfType<IFieldSymbol>()
            .FirstOrDefault(static field => field.ContainingType?.TypeKind == TypeKind.Enum);
        if (field?.ContainingType is not INamedTypeSymbol enumType)
            return false;

        bound = new BoundMemberAccessExpression(new BoundTypeExpression(enumType), field);
        return true;
    }

    private static ITypeSymbol? TryGetAttributeConstructorParameterType(
        INamedTypeSymbol attributeType,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        int argumentIndex)
    {
        if (arguments[argumentIndex].NameColon is not null)
            return null;

        var positionalIndex = 0;
        for (var i = 0; i < argumentIndex; i++)
        {
            if (arguments[i].NameColon is null)
                positionalIndex++;
        }

        ITypeSymbol? targetType = null;
        foreach (var constructor in attributeType.Constructors.Where(static constructor => !constructor.IsStatic))
        {
            if (constructor.Parameters.Length <= positionalIndex)
                continue;

            var parameterType = constructor.Parameters[positionalIndex].Type;
            if (targetType is null)
            {
                targetType = parameterType;
                continue;
            }

            if (!HaveSameTypeIdentity(targetType, parameterType))
                return null;
        }

        return targetType;
    }

    private static ITypeSymbol? TryGetAttributeConstructorEnumParameterType(
        INamedTypeSymbol attributeType,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        int argumentIndex)
    {
        if (arguments[argumentIndex].NameColon is not null)
            return null;

        var positionalIndex = 0;
        for (var i = 0; i < argumentIndex; i++)
        {
            if (arguments[i].NameColon is null)
                positionalIndex++;
        }

        ITypeSymbol? enumType = null;
        foreach (var constructor in attributeType.Constructors.Where(static constructor => !constructor.IsStatic))
        {
            if (constructor.Parameters.Length <= positionalIndex)
                continue;

            var parameterType = constructor.Parameters[positionalIndex].Type.UnwrapLiteralType()
                ?? constructor.Parameters[positionalIndex].Type;
            if (parameterType is not INamedTypeSymbol { TypeKind: TypeKind.Enum })
                continue;

            if (enumType is null || HaveSameTypeIdentity(enumType, parameterType))
            {
                enumType = parameterType;
                continue;
            }

            return null;
        }

        return enumType;
    }

    private static bool HaveSameTypeIdentity(ITypeSymbol left, ITypeSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        if (left is INamedTypeSymbol leftNamed &&
            right is INamedTypeSymbol rightNamed)
        {
            return string.Equals(
                leftNamed.ToFullyQualifiedMetadataName(),
                rightNamed.ToFullyQualifiedMetadataName(),
                StringComparison.Ordinal);
        }

        return false;
    }

    private INamedTypeSymbol? BindAttributeType(TypeSyntax attributeName)
    {
        if (!HasAttributeSuffix(attributeName))
        {
            var suffixed = TryAppendAttributeSuffix(attributeName);
            if (suffixed is not null && TryBindAttributeTypeCore(suffixed, out var resolved))
                return resolved;
        }

        if (TryBindAttributeTypeCore(attributeName, out var unsuffixedResolved))
            return unsuffixedResolved;

        return TryResolveAttributeTypeFallback(attributeName);
    }

    private bool TryBindAttributeTypeCore(
        TypeSyntax attributeName,
        out INamedTypeSymbol? resolved)
    {
        if (TryBindNamedTypeFromTypeSyntax(attributeName, out resolved, reportDiagnostics: false))
            return true;

        resolved = null;
        return false;
    }

    private INamedTypeSymbol? TryResolveAttributeTypeFallback(TypeSyntax attributeName)
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

    private static bool HasIncompleteAttributeName(TypeSyntax attributeName)
        => attributeName.DescendantTokens().Any(static token => token.IsMissing);

    private bool TryBindAttributeNamedArgument(
        INamedTypeSymbol attributeType,
        string name,
        ExpressionSyntax expression,
        out BoundObjectInitializerAssignmentEntry assignmentEntry,
        ref bool hasErrors)
    {
        assignmentEntry = null!;

        var member = FindAttributeNamedMember(attributeType, name);
        if (member is null)
            return false;

        if (member.IsStatic)
            return false;

        var memberType = member switch
        {
            IPropertySymbol property => property.Type,
            IFieldSymbol field => field.Type,
            _ => null
        };

        if (memberType is null)
            return false;

        var memberKind = member is IPropertySymbol ? "property" : "field";
        if (!EnsureMemberAccessible(member, expression.GetLocation(), memberKind))
            hasErrors = true;

        var boundValue = BindExpression(expression);
        if (boundValue is BoundErrorExpression)
        {
            hasErrors = true;
        }
        else if (!AttributeDataFactory.TryCreateTypedConstant(boundValue, out _))
        {
            _diagnostics.ReportAttributeArgumentMustBeConstant(expression.GetLocation());
            boundValue = new BoundErrorExpression(
                boundValue.Type ?? Compilation.ErrorTypeSymbol,
                null,
                BoundExpressionReason.ConstantExpected);
            hasErrors = true;
        }

        assignmentEntry = new BoundObjectInitializerAssignmentEntry(member, boundValue);
        return true;
    }

    private static ISymbol? FindAttributeNamedMember(INamedTypeSymbol attributeType, string name)
    {
        for (INamedTypeSymbol? current = attributeType; current is not null; current = current.BaseType as INamedTypeSymbol)
        {
            var members = current.GetMembers(name);

            var property = members
                .OfType<IPropertySymbol>()
                .FirstOrDefault(p => !p.IsStatic && p.SetMethod is not null);
            if (property is not null)
                return property;

            var field = members
                .OfType<IFieldSymbol>()
                .FirstOrDefault(f => !f.IsStatic);
            if (field is not null)
                return field;
        }

        return null;
    }

    private static TypeSyntax? TryAppendAttributeSuffix(TypeSyntax syntax)
    {
        return syntax switch
        {
            IdentifierNameSyntax identifier => SyntaxFactory.IdentifierName(
                SyntaxFactory.Identifier(AppendAttributeSuffix(identifier.Identifier.ValueText))),
            GenericNameSyntax generic => SyntaxFactory.GenericName(
                SyntaxFactory.Identifier(AppendAttributeSuffix(generic.Identifier.ValueText)),
                generic.TypeArgumentList),
            QualifiedNameSyntax qualified => TryAppendAttributeSuffix(qualified.Right) is UnqualifiedNameSyntax suffixedRight
                ? SyntaxFactory.QualifiedName(qualified.Left, qualified.DotToken, suffixedRight)
                : null,
            AliasQualifiedNameSyntax aliasQualified => TryAppendAttributeSuffix(aliasQualified.Name) is SimpleNameSyntax suffixedName
                ? SyntaxFactory.AliasQualifiedName(aliasQualified.Alias, aliasQualified.ColonColonToken, suffixedName)
                : null,
            _ => null
        };
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

            case GenericNameSyntax generic:
                {
                    var candidateName = AppendAttributeSuffixIfNeeded(generic.Identifier.ValueText, appendAttributeSuffix);
                    return FindAccessibleNamedType(candidateName, ComputeGenericArity(generic));
                }

            case QualifiedNameSyntax qualified when qualified.Right is IdentifierNameSyntax rightIdentifier:
                {
                    var container = TryLookupNamespaceOrType(qualified.Left);
                    if (container is null)
                        return null;

                    var candidateName = AppendAttributeSuffixIfNeeded(rightIdentifier.Identifier.ValueText, appendAttributeSuffix);
                    return container.LookupType(candidateName) as INamedTypeSymbol;
                }

            case QualifiedNameSyntax qualified when qualified.Right is GenericNameSyntax rightGeneric:
                {
                    var container = TryLookupNamespaceOrType(qualified.Left);
                    if (container is null)
                        return null;

                    var candidateName = AppendAttributeSuffixIfNeeded(rightGeneric.Identifier.ValueText, appendAttributeSuffix);
                    return TryLookupNestedNamedType(container, candidateName, ComputeGenericArity(rightGeneric));
                }

            case AliasQualifiedNameSyntax aliasQualified:
                {
                    var container = TryLookupAliasTarget(aliasQualified.Alias);
                    if (container is null)
                        return null;

                    switch (aliasQualified.Name)
                    {
                        case IdentifierNameSyntax identifierName:
                            {
                                var candidateName = AppendAttributeSuffixIfNeeded(identifierName.Identifier.ValueText, appendAttributeSuffix);
                                return container.LookupType(candidateName) as INamedTypeSymbol;
                            }
                        case GenericNameSyntax genericName:
                            {
                                var candidateName = AppendAttributeSuffixIfNeeded(genericName.Identifier.ValueText, appendAttributeSuffix);
                                return TryLookupNestedNamedType(container, candidateName, ComputeGenericArity(genericName));
                            }
                    }

                    break;
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

            case AliasQualifiedNameSyntax aliasQualified:
                {
                    return TryLookupAliasTarget(aliasQualified.Alias);
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

    private INamespaceOrTypeSymbol? TryLookupAliasTarget(IdentifierNameSyntax alias)
    {
        var symbol = LookupSymbol(alias.Identifier.ValueText);
        return symbol switch
        {
            INamespaceSymbol namespaceSymbol => namespaceSymbol,
            INamedTypeSymbol typeSymbol => typeSymbol,
            _ => null
        };
    }

    private static INamedTypeSymbol? TryLookupNestedNamedType(INamespaceOrTypeSymbol container, string name, int arity)
    {
        if (container.LookupType(name) is not INamedTypeSymbol typeSymbol)
            return null;

        var candidate = NormalizeDefinition(typeSymbol);
        return candidate.Arity == arity ? candidate : null;
    }

    private static int ComputeGenericArity(GenericNameSyntax generic)
    {
        var argumentCount = generic.TypeArgumentList.Arguments.Count;
        var separators = generic.TypeArgumentList.Arguments.SeparatorCount + 1;

        if (argumentCount == 0)
            return Math.Max(1, separators);

        return Math.Max(argumentCount, separators);
    }

    private static string AppendAttributeSuffix(string name)
    {
        return name.EndsWith("Attribute", StringComparison.Ordinal)
            ? name
            : string.Concat(name, "Attribute");
    }

    private static string AppendAttributeSuffixIfNeeded(string name, bool append)
    {
        if (!append)
            return name;

        return AppendAttributeSuffix(name);
    }

    private static bool HasAttributeSuffix(TypeSyntax syntax)
    {
        return syntax switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText.EndsWith("Attribute", StringComparison.Ordinal),
            GenericNameSyntax generic => generic.Identifier.ValueText.EndsWith("Attribute", StringComparison.Ordinal),
            QualifiedNameSyntax qualified => HasAttributeSuffix(qualified.Right),
            AliasQualifiedNameSyntax aliasQualified => HasAttributeSuffix(aliasQualified.Name),
            _ => false
        };
    }
}
