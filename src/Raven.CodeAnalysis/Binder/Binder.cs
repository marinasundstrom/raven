using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal abstract class Binder
{
    protected readonly DiagnosticBag _diagnostics;

    protected Binder(Binder? parent, DiagnosticBag? diagnostics = null)
    {
        ParentBinder = parent!;
        _diagnostics = diagnostics ?? new DiagnosticBag();
    }

    public Binder? ParentBinder { get; }

    public DiagnosticBag Diagnostics => _diagnostics;

    public virtual ISymbol ContainingSymbol { get; }

    public virtual Compilation Compilation
    {
        get
        {
            if (ParentBinder is null)
                return null!;

            if (this is GlobalBinder globalBinder)
            {
                return globalBinder.Compilation;
            }

            return ParentBinder.Compilation;
        }
    }

    protected BoundNodeFactory BoundFactory => Compilation.BoundNodeFactory;

    protected BoundErrorExpression ErrorExpression(
        ITypeSymbol? type = null,
        ISymbol? symbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        ImmutableArray<ISymbol> candidates = default)
        => BoundFactory.ErrorExpression(type, symbol, reason, candidates);

    protected internal bool IsSymbolAccessible(ISymbol symbol)
    {
        if (symbol is null)
            return true;

        var within = GetAccessibilityContext();
        return AccessibilityUtilities.IsAccessible(symbol, within);
    }

    private ISymbol? GetAccessibilityContext()
    {
        for (var current = this; current is not null; current = current.ParentBinder)
        {
            if (current.ContainingSymbol is not null)
                return current.ContainingSymbol;
        }

        return Compilation.Assembly;
    }

    public virtual SemanticModel SemanticModel
    {
        get
        {
            if (ParentBinder is null)
                return null!;

            if (this is TopLevelBinder topLevelBinder)
            {
                return topLevelBinder.SemanticModel;
            }

            return ParentBinder.SemanticModel;
        }
    }

    public virtual INamespaceSymbol? CurrentNamespace => ParentBinder?.CurrentNamespace;

    public virtual ISymbol? BindDeclaredSymbol(SyntaxNode node) => ParentBinder?.BindDeclaredSymbol(node);

    public virtual SymbolInfo BindReferencedSymbol(SyntaxNode node)
    {
        // Handle type syntax first so qualified names and generics are resolved correctly.
        if (node is TypeSyntax typeSyntax && node is not IdentifierNameSyntax)
        {
            try
            {
                var type = ResolveType(typeSyntax);
                return new SymbolInfo(type);
            }
            catch
            {
                return SymbolInfo.None;
            }
        }

        return node switch
        {
            IdentifierNameSyntax id => BindIdentifierReference(id),
            MemberAccessExpressionSyntax memberAccess => BindMemberAccessReference(memberAccess),
            MemberBindingExpressionSyntax memberBinding => BindMemberBindingReference(memberBinding),
            InvocationExpressionSyntax invocation => BindInvocationReference(invocation),
            _ => ParentBinder?.BindReferencedSymbol(node) ?? SymbolInfo.None,
        };
    }

    public virtual SymbolInfo BindSymbol(SyntaxNode node)
    {
        var declared = BindDeclaredSymbol(node);
        return declared is not null ? new SymbolInfo(declared) : BindReferencedSymbol(node);
    }

    internal virtual SymbolInfo BindIdentifierReference(IdentifierNameSyntax node)
    {
        if (node.Parent is QualifiedNameSyntax qn && qn.Right == node)
        {
            var resolved = ResolveName(qn);
            if (resolved is not null)
                return new SymbolInfo(resolved);
        }

        var name = node.Identifier.ValueText;
        var symbol = LookupSymbol(name);
        if (symbol != null)
            return new SymbolInfo(symbol);

        var type = LookupType(name);
        if (type != null)
            return new SymbolInfo(type);

        var ns = LookupNamespace(name);
        if (ns != null)
            return new SymbolInfo(ns);

        return SymbolInfo.None;
    }

    internal virtual SymbolInfo BindMemberAccessReference(MemberAccessExpressionSyntax node)
    {
        return SymbolInfo.None; // To be overridden in specific binders
    }

    internal virtual SymbolInfo BindMemberBindingReference(MemberBindingExpressionSyntax node)
    {
        return SymbolInfo.None; // To be overridden in specific binders
    }

    internal virtual SymbolInfo BindInvocationReference(InvocationExpressionSyntax node)
    {
        return SymbolInfo.None; // To be overridden in specific binders
    }

    public virtual ITypeSymbol? LookupType(string name)
    {
        var type = CurrentNamespace?.LookupType(name);
        if (type != null)
            return type;

        return ParentBinder?.LookupType(name);
    }

    public virtual INamespaceSymbol? LookupNamespace(string name)
    {
        var currentNamespace = CurrentNamespace?.LookupNamespace(name);
        if (currentNamespace is not null)
            return currentNamespace;

        var namespaceFromParent = ParentBinder?.LookupNamespace(name);
        if (namespaceFromParent is not null)
            return namespaceFromParent;

        return Compilation.GlobalNamespace.LookupNamespace(name);
    }

    public virtual ISymbol? LookupSymbol(string name)
    {
        return ParentBinder?.LookupSymbol(name);
    }

    public virtual IEnumerable<ISymbol> LookupSymbols(string name)
    {
        return ParentBinder?.LookupSymbols(name) ?? Enumerable.Empty<ISymbol>();
    }

    public virtual IEnumerable<IMethodSymbol> LookupExtensionMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null)
            return Enumerable.Empty<IMethodSymbol>();

        if (receiverType.TypeKind == TypeKind.Error)
            return Enumerable.Empty<IMethodSymbol>();

        return ParentBinder?.LookupExtensionMethods(name, receiverType, includePartialMatches) ?? Enumerable.Empty<IMethodSymbol>();
    }

    public virtual IEnumerable<IPropertySymbol> LookupExtensionProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null)
            return Enumerable.Empty<IPropertySymbol>();

        if (receiverType.TypeKind == TypeKind.Error)
            return Enumerable.Empty<IPropertySymbol>();

        return ParentBinder?.LookupExtensionProperties(name, receiverType, includePartialMatches) ?? Enumerable.Empty<IPropertySymbol>();
    }

    public virtual IEnumerable<IMethodSymbol> LookupExtensionStaticMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null)
            return Enumerable.Empty<IMethodSymbol>();

        if (receiverType.TypeKind == TypeKind.Error)
            return Enumerable.Empty<IMethodSymbol>();

        return ParentBinder?.LookupExtensionStaticMethods(name, receiverType, includePartialMatches) ?? Enumerable.Empty<IMethodSymbol>();
    }

    public virtual IEnumerable<IPropertySymbol> LookupExtensionStaticProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null)
            return Enumerable.Empty<IPropertySymbol>();

        if (receiverType.TypeKind == TypeKind.Error)
            return Enumerable.Empty<IPropertySymbol>();

        return ParentBinder?.LookupExtensionStaticProperties(name, receiverType, includePartialMatches) ?? Enumerable.Empty<IPropertySymbol>();
    }

    protected IEnumerable<IMethodSymbol> GetExtensionMethodsFromScope(
        INamespaceOrTypeSymbol scope,
        string? name,
        ITypeSymbol receiverType,
        bool includePartialMatches)
    {
        foreach (var method in EnumerateExtensionMethods(scope, name, includePartialMatches))
        {
            if (!IsExtensionCandidateForReceiver(method, receiverType, includePartialMatches))
                continue;

            if (!IsSymbolAccessible(method))
                continue;

            yield return AdjustExtensionForReceiver(method, receiverType);
        }
    }

    protected IEnumerable<IPropertySymbol> GetExtensionPropertiesFromScope(
        INamespaceOrTypeSymbol scope,
        string? name,
        ITypeSymbol receiverType,
        bool includePartialMatches)
    {
        foreach (var property in EnumerateExtensionProperties(scope, name, includePartialMatches))
        {
            if (!IsExtensionPropertyCandidateForReceiver(property, receiverType, includePartialMatches))
                continue;

            if (!IsSymbolAccessible(property))
                continue;

            yield return AdjustExtensionPropertyForReceiver(property, receiverType);
        }
    }

    protected IEnumerable<IMethodSymbol> GetExtensionStaticMethodsFromScope(
        INamespaceOrTypeSymbol scope,
        string? name,
        ITypeSymbol receiverType,
        bool includePartialMatches)
    {
        foreach (var method in EnumerateExtensionStaticMethods(scope, name, includePartialMatches))
        {
            if (!IsExtensionStaticCandidateForReceiver(method, receiverType, includePartialMatches))
                continue;

            if (!IsSymbolAccessible(method))
                continue;

            yield return AdjustExtensionStaticMethodForReceiver(method, receiverType);
        }
    }

    protected IEnumerable<IPropertySymbol> GetExtensionStaticPropertiesFromScope(
        INamespaceOrTypeSymbol scope,
        string? name,
        ITypeSymbol receiverType,
        bool includePartialMatches)
    {
        foreach (var property in EnumerateExtensionStaticProperties(scope, name, includePartialMatches))
        {
            if (!IsExtensionStaticCandidateForReceiver(property, receiverType, includePartialMatches))
                continue;

            if (!IsSymbolAccessible(property))
                continue;

            yield return AdjustExtensionStaticPropertyForReceiver(property, receiverType);
        }
    }

    private IMethodSymbol AdjustExtensionForReceiver(IMethodSymbol method, ITypeSymbol receiverType)
    {
        if (!method.IsExtensionMethod || receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return method;

        if (!TryCreateConstructedExtension(method, receiverType, out var constructed))
            return method;

        return constructed;
    }

    private IPropertySymbol AdjustExtensionPropertyForReceiver(IPropertySymbol property, ITypeSymbol receiverType)
    {
        if (!property.IsExtensionProperty() || receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return property;

        var getMethod = property.GetMethod;
        var setMethod = property.SetMethod;

        IMethodSymbol? constructedGet = null;
        IMethodSymbol? constructedSet = null;

        if (getMethod is not null && TryCreateConstructedExtension(getMethod, receiverType, out var getConstructed))
            constructedGet = getConstructed;

        if (setMethod is not null && TryCreateConstructedExtension(setMethod, receiverType, out var setConstructed))
            constructedSet = setConstructed;

        if (constructedGet is null && constructedSet is null)
            return property;

        return new ConstructedExtensionPropertySymbol(
            property,
            constructedGet ?? getMethod,
            constructedSet ?? setMethod);
    }

    private IMethodSymbol AdjustExtensionStaticMethodForReceiver(IMethodSymbol method, ITypeSymbol receiverType)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return method;

        var extensionReceiverType = method.GetExtensionReceiverType();
        if (extensionReceiverType is null || extensionReceiverType.TypeKind == TypeKind.Error)
            return method;

        if (!TryCreateConstructedStaticExtension(method, receiverType, extensionReceiverType, out var constructed))
            return method;

        return constructed;
    }

    private IPropertySymbol AdjustExtensionStaticPropertyForReceiver(IPropertySymbol property, ITypeSymbol receiverType)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return property;

        var extensionReceiverType = property.GetExtensionReceiverType();
        if (extensionReceiverType is null || extensionReceiverType.TypeKind == TypeKind.Error)
            return property;

        var getMethod = property.GetMethod;
        var setMethod = property.SetMethod;

        IMethodSymbol? constructedGet = null;
        IMethodSymbol? constructedSet = null;

        if (getMethod is not null &&
            TryCreateConstructedStaticExtension(getMethod, receiverType, extensionReceiverType, out var getConstructed))
        {
            constructedGet = getConstructed;
        }

        if (setMethod is not null &&
            TryCreateConstructedStaticExtension(setMethod, receiverType, extensionReceiverType, out var setConstructed))
        {
            constructedSet = setConstructed;
        }

        if (constructedGet is null && constructedSet is null)
            return property;

        return new ConstructedExtensionPropertySymbol(
            property,
            constructedGet ?? getMethod,
            constructedSet ?? setMethod);
    }

    private bool TryCreateConstructedExtension(IMethodSymbol method, ITypeSymbol receiverType, out IMethodSymbol constructed)
    {
        constructed = method;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var methodDefinition = method.OriginalDefinition ?? method;

        if (methodDefinition.Parameters.IsDefaultOrEmpty || methodDefinition.Parameters.Length == 0)
            return false;

        var receiverParameterType = methodDefinition.Parameters[0].Type;
        if (receiverParameterType is null || receiverParameterType.TypeKind == TypeKind.Error)
            return false;

        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);

        if (!TryUnifyExtensionReceiver(receiverParameterType, receiverType, substitutions))
            return false;

        if (!methodDefinition.TypeParameters.IsDefaultOrEmpty && methodDefinition.TypeParameters.Length > 0)
        {
            var methodTypeArguments = new ITypeSymbol[methodDefinition.TypeParameters.Length];

            for (int i = 0; i < methodDefinition.TypeParameters.Length; i++)
            {
                var typeParameter = methodDefinition.TypeParameters[i];
                if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                    return false;

                methodTypeArguments[i] = typeArgument;
            }

            constructed = methodDefinition.Construct(methodTypeArguments);
            return true;
        }

        var container = methodDefinition.ContainingType as INamedTypeSymbol;
        if (container is null || !container.IsGenericType || container.TypeParameters.IsDefaultOrEmpty || container.TypeParameters.Length == 0)
            return false;

        var typeArguments = new ITypeSymbol[container.TypeParameters.Length];

        for (int i = 0; i < container.TypeParameters.Length; i++)
        {
            var typeParameter = container.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                return false;

            typeArguments[i] = typeArgument;
        }

        var constructedContainer = container.Construct(typeArguments);
        if (constructedContainer is not INamedTypeSymbol namedContainer)
            return false;

        var originalDefinition = methodDefinition;

        foreach (var candidate in namedContainer.GetMembers(method.Name).OfType<IMethodSymbol>())
        {
            var candidateOriginal = candidate.OriginalDefinition ?? candidate;
            if (SymbolEqualityComparer.Default.Equals(candidateOriginal, originalDefinition))
            {
                constructed = candidate;
                return true;
            }
        }

        return false;
    }

    private bool TryCreateConstructedStaticExtension(
        IMethodSymbol method,
        ITypeSymbol receiverType,
        ITypeSymbol extensionReceiverType,
        out IMethodSymbol constructed)
    {
        constructed = method;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var methodDefinition = method.OriginalDefinition ?? method;

        if (methodDefinition.ContainingType is not INamedTypeSymbol container)
            return false;

        if (extensionReceiverType is null || extensionReceiverType.TypeKind == TypeKind.Error)
            return false;

        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);

        if (!TryUnifyExtensionReceiver(extensionReceiverType, receiverType, substitutions))
            return false;

        if (!methodDefinition.TypeParameters.IsDefaultOrEmpty && methodDefinition.TypeParameters.Length > 0)
        {
            var methodTypeArguments = new ITypeSymbol[methodDefinition.TypeParameters.Length];

            for (int i = 0; i < methodDefinition.TypeParameters.Length; i++)
            {
                var typeParameter = methodDefinition.TypeParameters[i];
                if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                    return false;

                methodTypeArguments[i] = typeArgument;
            }

            constructed = methodDefinition.Construct(methodTypeArguments);
            return true;
        }

        var containerDefinition = container.ConstructedFrom as INamedTypeSymbol ?? container;
        if (!containerDefinition.IsGenericType || containerDefinition.TypeParameters.IsDefaultOrEmpty || containerDefinition.TypeParameters.Length == 0)
            return false;

        var typeArguments = new ITypeSymbol[containerDefinition.TypeParameters.Length];

        for (int i = 0; i < containerDefinition.TypeParameters.Length; i++)
        {
            var typeParameter = containerDefinition.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                return false;

            typeArguments[i] = typeArgument;
        }

        var constructedContainer = containerDefinition.Construct(typeArguments);
        if (constructedContainer is not INamedTypeSymbol namedContainer)
            return false;

        var originalDefinition = methodDefinition;

        foreach (var candidate in namedContainer.GetMembers(method.Name).OfType<IMethodSymbol>())
        {
            var candidateOriginal = candidate.OriginalDefinition ?? candidate;
            if (SymbolEqualityComparer.Default.Equals(candidateOriginal, originalDefinition))
            {
                constructed = candidate;
                return true;
            }
        }

        return false;
    }

    private bool IsExtensionPropertyCandidateForReceiver(
        IPropertySymbol property,
        ITypeSymbol receiverType,
        bool includePartialMatches)
    {
        var accessor = property.GetMethod ?? property.SetMethod;
        if (accessor is null || !accessor.IsExtensionMethod)
            return false;

        if (includePartialMatches)
            return true;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return true;

        if (accessor.Parameters.IsDefaultOrEmpty || accessor.Parameters.Length == 0)
            return false;

        var parameterType = accessor.Parameters[0].Type;
        if (parameterType is null || parameterType.TypeKind == TypeKind.Error)
            return true;

        if (ContainsTypeParameters(parameterType))
        {
            var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
            return TryUnifyExtensionReceiver(parameterType, receiverType, substitutions);
        }

        if (SymbolEqualityComparer.Default.Equals(parameterType, receiverType))
            return true;

        var conversion = Compilation.ClassifyConversion(receiverType, parameterType);
        return conversion.Exists && conversion.IsImplicit;
    }

    private bool IsExtensionStaticCandidateForReceiver(
        ISymbol member,
        ITypeSymbol receiverType,
        bool includePartialMatches)
    {
        var extensionReceiverType = member.GetExtensionReceiverType();
        if (extensionReceiverType is null)
            return false;

        if (includePartialMatches)
            return true;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return true;

        if (extensionReceiverType.TypeKind == TypeKind.Error)
            return true;

        if (ContainsTypeParameters(extensionReceiverType))
        {
            var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
            return TryUnifyExtensionReceiver(extensionReceiverType, receiverType, substitutions);
        }

        if (SymbolEqualityComparer.Default.Equals(extensionReceiverType, receiverType))
            return true;

        var conversion = Compilation.ClassifyConversion(receiverType, extensionReceiverType);
        return conversion.Exists && conversion.IsImplicit;
    }

    private bool TryUnifyExtensionReceiver(
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        parameterType = NormalizeTypeForExtensionInference(parameterType);
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (parameterType is ITypeParameterSymbol parameter)
            return TryRecordExtensionSubstitution(parameter, argumentType, substitutions);

        if (parameterType is INamedTypeSymbol paramNamed)
        {
            if (argumentType is INamedTypeSymbol argNamed)
            {
                if (TryUnifyNamedType(paramNamed, argNamed, substitutions))
                    return true;

                foreach (var iface in argNamed.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface, substitutions))
                        return true;
                }

                for (var baseType = argNamed.BaseType; baseType is not null; baseType = baseType.BaseType)
                {
                    if (TryUnifyNamedType(paramNamed, baseType, substitutions))
                        return true;
                }

                return false;
            }

            if (argumentType is IArrayTypeSymbol arrayArgument)
            {
                if (TryUnifyArrayLike(paramNamed, arrayArgument, substitutions))
                    return true;

                foreach (var iface in arrayArgument.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface, substitutions))
                        return true;
                }

                return false;
            }

            if (Conversion.IsNullable(argumentType))
            {
                var plainArgument = argumentType.GetPlainType();

                if (TryUnifyNamedType(paramNamed, plainArgument as INamedTypeSymbol, substitutions))
                    return true;

                foreach (var iface in plainArgument.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface, substitutions))
                        return true;
                }

                return false;
            }
        }

        if (parameterType is IArrayTypeSymbol paramArray && argumentType is IArrayTypeSymbol argArray)
            return TryUnifyExtensionReceiver(paramArray.ElementType, argArray.ElementType, substitutions);

        if (Conversion.IsNullable(parameterType))
        {
            var plainParameter = parameterType.GetPlainType();

            if (Conversion.IsNullable(argumentType))
                return TryUnifyExtensionReceiver(plainParameter, argumentType.GetPlainType(), substitutions);

            if (!argumentType.IsValueType)
                return TryUnifyExtensionReceiver(plainParameter, argumentType, substitutions);

            return false;
        }

        return SymbolEqualityComparer.Default.Equals(parameterType, argumentType);

        bool TryUnifyNamedType(
            INamedTypeSymbol parameterNamed,
            INamedTypeSymbol? argumentNamed,
            Dictionary<ITypeParameterSymbol, ITypeSymbol> map)
        {
            if (argumentNamed is null)
                return false;

            var parameterDefinition = parameterNamed.OriginalDefinition ?? parameterNamed;
            var argumentDefinition = argumentNamed.OriginalDefinition ?? argumentNamed;

            if (!SymbolEqualityComparer.Default.Equals(parameterDefinition, argumentDefinition))
                return false;

            var parameterArguments = parameterNamed.TypeArguments;
            var argumentArguments = argumentNamed.TypeArguments;

            if (parameterArguments.IsDefault || argumentArguments.IsDefault)
                return false;

            if (parameterArguments.Length != argumentArguments.Length)
                return false;

            for (int i = 0; i < parameterArguments.Length; i++)
            {
                if (!TryUnifyExtensionReceiver(parameterArguments[i], argumentArguments[i], map))
                    return false;
            }

            return true;
        }

        bool TryUnifyArrayLike(
            INamedTypeSymbol parameterNamed,
            IArrayTypeSymbol argumentArray,
            Dictionary<ITypeParameterSymbol, ITypeSymbol> map)
        {
            var constructedFrom = parameterNamed.ConstructedFrom ?? parameterNamed;

            if (constructedFrom.SpecialType is SpecialType.System_Collections_Generic_IEnumerable_T or
                SpecialType.System_Collections_Generic_ICollection_T or
                SpecialType.System_Collections_Generic_IList_T ||
                IsGenericCollectionInterface(parameterNamed, "IReadOnlyCollection") ||
                IsGenericCollectionInterface(parameterNamed, "IReadOnlyList"))
            {
                return TryUnifyExtensionReceiver(parameterNamed.TypeArguments[0], argumentArray.ElementType, map);
            }

            return false;
        }
    }

    private bool TryRecordExtensionSubstitution(
        ITypeParameterSymbol typeParameter,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (substitutions.TryGetValue(typeParameter, out var existing))
        {
            existing = NormalizeTypeForExtensionInference(existing);

            if (SymbolEqualityComparer.Default.Equals(existing, argumentType))
                return true;

            if (Compilation.ClassifyConversion(argumentType, existing).IsImplicit)
                return true;

            if (Compilation.ClassifyConversion(existing, argumentType).IsImplicit)
            {
                substitutions[typeParameter] = argumentType;
                return true;
            }

            return false;
        }

        substitutions[typeParameter] = argumentType;
        return true;
    }

    private static ITypeSymbol NormalizeTypeForExtensionInference(ITypeSymbol type)
    {
        return type switch
        {
            LiteralTypeSymbol literal => literal.UnderlyingType,
            _ => type
        };
    }

    private static bool IsGenericCollectionInterface(INamedTypeSymbol parameterNamed, string interfaceName)
    {
        var definition = parameterNamed.ConstructedFrom ?? parameterNamed;

        if (!string.Equals(definition.Name, interfaceName, StringComparison.Ordinal))
            return false;

        var ns = definition.ContainingNamespace?.ToDisplayString();
        return string.Equals(ns, "System.Collections.Generic", StringComparison.Ordinal);
    }

    private static IEnumerable<IMethodSymbol> EnumerateExtensionMethods(
        INamespaceOrTypeSymbol scope,
        string? name,
        bool includePartialMatches)
    {
        if (scope is INamespaceSymbol ns)
        {
            foreach (var member in ns.GetMembers())
            {
                if (member is INamedTypeSymbol typeMember)
                {
                    foreach (var method in EnumerateExtensionMethods(typeMember, name, includePartialMatches))
                        yield return method;
                }
                else if (member is INamespaceSymbol nestedNs)
                {
                    foreach (var method in EnumerateExtensionMethods(nestedNs, name, includePartialMatches))
                        yield return method;
                }
            }

            yield break;
        }

        if (scope is not INamedTypeSymbol type)
            yield break;

        var members = includePartialMatches || string.IsNullOrEmpty(name)
            ? type.GetMembers().OfType<IMethodSymbol>()
            : type.GetMembers(name!).OfType<IMethodSymbol>();

        foreach (var member in members)
        {
            if (!member.IsExtensionMethod)
                continue;

            if (!includePartialMatches && name is not null && member.Name != name)
                continue;

            yield return member;
        }

        foreach (var nested in type.GetMembers().OfType<INamedTypeSymbol>())
        {
            foreach (var method in EnumerateExtensionMethods(nested, name, includePartialMatches))
                yield return method;
        }
    }

    private static IEnumerable<IMethodSymbol> EnumerateExtensionStaticMethods(
        INamespaceOrTypeSymbol scope,
        string? name,
        bool includePartialMatches)
    {
        if (scope is INamespaceSymbol ns)
        {
            foreach (var member in ns.GetMembers())
            {
                if (member is INamedTypeSymbol typeMember)
                {
                    foreach (var method in EnumerateExtensionStaticMethods(typeMember, name, includePartialMatches))
                        yield return method;
                }
                else if (member is INamespaceSymbol nestedNs)
                {
                    foreach (var method in EnumerateExtensionStaticMethods(nestedNs, name, includePartialMatches))
                        yield return method;
                }
            }

            yield break;
        }

        if (scope is not INamedTypeSymbol type)
            yield break;

        var members = includePartialMatches || string.IsNullOrEmpty(name)
            ? type.GetMembers().OfType<IMethodSymbol>()
            : type.GetMembers(name!).OfType<IMethodSymbol>();

        foreach (var member in members)
        {
            if (member.IsExtensionMethod || !member.IsStatic)
                continue;

            if (member.GetExtensionReceiverType() is null)
                continue;

            if (!includePartialMatches && name is not null && member.Name != name)
                continue;

            yield return member;
        }

        foreach (var nested in type.GetMembers().OfType<INamedTypeSymbol>())
        {
            foreach (var method in EnumerateExtensionStaticMethods(nested, name, includePartialMatches))
                yield return method;
        }
    }

    private static IEnumerable<IPropertySymbol> EnumerateExtensionProperties(
        INamespaceOrTypeSymbol scope,
        string? name,
        bool includePartialMatches)
    {
        if (scope is INamespaceSymbol ns)
        {
            foreach (var member in ns.GetMembers())
            {
                if (member is INamedTypeSymbol typeMember)
                {
                    foreach (var property in EnumerateExtensionProperties(typeMember, name, includePartialMatches))
                        yield return property;
                }
                else if (member is INamespaceSymbol nestedNs)
                {
                    foreach (var property in EnumerateExtensionProperties(nestedNs, name, includePartialMatches))
                        yield return property;
                }
            }

            yield break;
        }

        if (scope is not INamedTypeSymbol type)
            yield break;

        var members = includePartialMatches || string.IsNullOrEmpty(name)
            ? type.GetMembers().OfType<IPropertySymbol>()
            : type.GetMembers(name!).OfType<IPropertySymbol>();

        foreach (var property in members)
        {
            if (!property.IsExtensionProperty())
                continue;

            if (!includePartialMatches && name is not null && property.Name != name)
                continue;

            yield return property;
        }

        foreach (var nested in type.GetMembers().OfType<INamedTypeSymbol>())
        {
            foreach (var property in EnumerateExtensionProperties(nested, name, includePartialMatches))
                yield return property;
        }
    }

    private static IEnumerable<IPropertySymbol> EnumerateExtensionStaticProperties(
        INamespaceOrTypeSymbol scope,
        string? name,
        bool includePartialMatches)
    {
        if (scope is INamespaceSymbol ns)
        {
            foreach (var member in ns.GetMembers())
            {
                if (member is INamedTypeSymbol typeMember)
                {
                    foreach (var property in EnumerateExtensionStaticProperties(typeMember, name, includePartialMatches))
                        yield return property;
                }
                else if (member is INamespaceSymbol nestedNs)
                {
                    foreach (var property in EnumerateExtensionStaticProperties(nestedNs, name, includePartialMatches))
                        yield return property;
                }
            }

            yield break;
        }

        if (scope is not INamedTypeSymbol type)
            yield break;

        var members = includePartialMatches || string.IsNullOrEmpty(name)
            ? type.GetMembers().OfType<IPropertySymbol>()
            : type.GetMembers(name!).OfType<IPropertySymbol>();

        foreach (var property in members)
        {
            if (property.IsExtensionProperty() || !property.IsStatic)
                continue;

            if (property.GetExtensionReceiverType() is null)
                continue;

            if (!includePartialMatches && name is not null && property.Name != name)
                continue;

            yield return property;
        }

        foreach (var nested in type.GetMembers().OfType<INamedTypeSymbol>())
        {
            foreach (var property in EnumerateExtensionStaticProperties(nested, name, includePartialMatches))
                yield return property;
        }
    }

    private bool IsExtensionCandidateForReceiver(
        IMethodSymbol method,
        ITypeSymbol receiverType,
        bool includePartialMatches)
    {
        if (!method.IsExtensionMethod)
            return false;

        if (includePartialMatches)
            return true;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return true;

        if (method.Parameters.IsDefaultOrEmpty || method.Parameters.Length == 0)
            return false;

        var parameterType = method.Parameters[0].Type;
        if (parameterType is null || parameterType.TypeKind == TypeKind.Error)
            return true;

        if (ContainsTypeParameters(parameterType))
        {
            var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
            return TryUnifyExtensionReceiver(parameterType, receiverType, substitutions);
        }

        if (SymbolEqualityComparer.Default.Equals(parameterType, receiverType))
            return true;

        var conversion = Compilation.ClassifyConversion(receiverType, parameterType);
        return conversion.Exists && conversion.IsImplicit;
    }

    private static bool ContainsTypeParameters(ITypeSymbol type)
    {
        switch (type)
        {
            case ITypeParameterSymbol:
                return true;
            case INamedTypeSymbol named when !named.TypeArguments.IsDefaultOrEmpty:
                foreach (var argument in named.TypeArguments)
                {
                    if (ContainsTypeParameters(argument))
                        return true;
                }

                return false;
            case IArrayTypeSymbol array:
                return ContainsTypeParameters(array.ElementType);
            case NullableTypeSymbol nullable:
                return ContainsTypeParameters(nullable.UnderlyingType);
            case ITypeUnionSymbol union:
                foreach (var member in union.Types)
                {
                    if (ContainsTypeParameters(member))
                        return true;
                }

                return false;
            default:
                return false;
        }
    }

    public virtual BoundExpression BindExpression(ExpressionSyntax expression)
    {
        if (TryGetCachedBoundNode(expression) is BoundExpression cached)
            return cached;

        var result = ParentBinder?.BindExpression(expression)
                     ?? ErrorExpression();

        CacheBoundNode(expression, result);

        return result;
    }

    public virtual BoundStatement BindStatement(StatementSyntax statement)
    {
        if (TryGetCachedBoundNode(statement) is BoundStatement cached)
            return cached;

        var result = ParentBinder?.BindStatement(statement)
                     ?? new BoundExpressionStatement(ErrorExpression());

        CacheBoundNode(statement, result);

        return result;
    }

    public ITypeSymbol ResolveType(TypeSyntax typeSyntax, RefKind refKindHint)
        => ResolveTypeInternal(typeSyntax, refKindHint);

    public virtual ITypeSymbol ResolveType(TypeSyntax typeSyntax)
        => ResolveTypeInternal(typeSyntax, refKindHint: null);

    private ITypeSymbol ResolveTypeInternal(TypeSyntax typeSyntax, RefKind? refKindHint)
    {
        if (typeSyntax is NullTypeSyntax)
            return ApplyRefKindHint(Compilation.NullTypeSymbol, refKindHint);

        if (typeSyntax is LiteralTypeSyntax literalType)
        {
            var token = literalType.Token;
            var value = token.Value ?? token.Text!;
            ITypeSymbol underlying = value switch
            {
                int => Compilation.GetSpecialType(SpecialType.System_Int32),
                long => Compilation.GetSpecialType(SpecialType.System_Int64),
                float => Compilation.GetSpecialType(SpecialType.System_Single),
                double => Compilation.GetSpecialType(SpecialType.System_Double),
                bool => Compilation.GetSpecialType(SpecialType.System_Boolean),
                char => Compilation.GetSpecialType(SpecialType.System_Char),
                string => Compilation.GetSpecialType(SpecialType.System_String),
                _ => Compilation.ErrorTypeSymbol
            };

            return ApplyRefKindHint(new LiteralTypeSymbol(underlying, value, Compilation), refKindHint);
        }

        if (typeSyntax is ByRefTypeSyntax byRef)
        {
            var elementType = ResolveTypeInternal(byRef.ElementType, refKindHint: null);
            return new ByRefTypeSymbol(elementType);
        }

        if (typeSyntax is PointerTypeSyntax pointer)
        {
            var elementType = ResolveTypeInternal(pointer.ElementType, refKindHint: null);
            var pointerType = Compilation.CreatePointerTypeSymbol(elementType);
            return ApplyRefKindHint(pointerType, refKindHint);
        }

        if (typeSyntax is NullableTypeSyntax nb)
        {
            var elementType = ResolveTypeInternal(nb.ElementType, refKindHint: null);
            if (elementType is ITypeParameterSymbol typeParameter &&
                (typeParameter.ConstraintKind & TypeParameterConstraintKind.NotNull) != 0)
            {
                ReportNotNullConstraintViolation(typeParameter, nb.GetLocation());
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
            }

            return ApplyRefKindHint(elementType.MakeNullable(), refKindHint);
        }

        if (typeSyntax is UnionTypeSyntax ut)
        {
            var types = new List<ITypeSymbol>();
            foreach (var t in ut.Types)
            {
                if (t is NullableTypeSyntax nt)
                {
                    _diagnostics.ReportNullableTypeInUnion(nt.GetLocation());
                    return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
                }

                types.Add(ResolveTypeInternal(t, refKindHint: null));
            }

            var normalized = TypeSymbolNormalization.NormalizeUnion(
                types,
                _diagnostics,
                ut.GetLocation(),
                Compilation.ErrorTypeSymbol);
            return ApplyRefKindHint(normalized, refKindHint);
        }

        if (typeSyntax is ArrayTypeSyntax arrayTypeSyntax)
        {
            var currentElementType = ResolveTypeInternal(arrayTypeSyntax.ElementType, refKindHint: null);

            foreach (var rankSpecifier in arrayTypeSyntax.RankSpecifiers)
            {
                var rank = rankSpecifier.CommaTokens.Count + 1;
                currentElementType = Compilation.CreateArrayTypeSymbol(currentElementType, rank);
            }

            return ApplyRefKindHint(currentElementType, refKindHint);
        }

        if (typeSyntax is TupleTypeSyntax tupleTypeSyntax)
        {
            var elements = tupleTypeSyntax.Elements
                .Select(e => (e.NameColon?.Name.ToString(), ResolveTypeInternal(e.Type, refKindHint: null)))
                .ToArray();
            return ApplyRefKindHint(Compilation.CreateTupleTypeSymbol(elements), refKindHint);
        }

        if (typeSyntax is FunctionTypeSyntax functionTypeSyntax)
        {
            var parameterTypes = new List<ITypeSymbol>();

            if (functionTypeSyntax.ParameterList is not null)
            {
                foreach (var parameter in functionTypeSyntax.ParameterList.Parameters)
                {
                    parameterTypes.Add(ResolveTypeInternal(parameter, refKindHint: null));
                }
            }
            else if (functionTypeSyntax.Parameter is not null)
            {
                parameterTypes.Add(ResolveTypeInternal(functionTypeSyntax.Parameter, refKindHint: null));
            }

            var returnType = ResolveTypeInternal(functionTypeSyntax.ReturnType, refKindHint: null);
            var delegateType = Compilation.CreateFunctionTypeSymbol(parameterTypes.ToArray(), returnType);
            return ApplyRefKindHint(delegateType, refKindHint);
        }

        if (typeSyntax is PredefinedTypeSyntax predefinedTypeSyntax)
            return ApplyRefKindHint(Compilation.ResolvePredefinedType(predefinedTypeSyntax), refKindHint);

        if (typeSyntax is UnitTypeSyntax)
            return ApplyRefKindHint(Compilation.GetSpecialType(SpecialType.System_Unit), refKindHint);

        if (typeSyntax is IdentifierNameSyntax ident)
        {
            if (ident.Identifier.IsMissing)
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);

            var type = LookupType(ident.Identifier.ValueText);
            if (type is INamedTypeSymbol named)
            {
                if (named.IsAlias)
                    return ApplyAccessibilityAndRefKind(named, ident.Identifier.GetLocation(), refKindHint);

                if (named.Arity > 0 && named.IsUnboundGenericType)
                {
                    var zeroArity = FindAccessibleNamedType(ident.Identifier.ValueText, 0);
                    if (zeroArity is null)
                    {
                        _diagnostics.ReportTypeRequiresTypeArguments(named.Name, named.Arity, ident.Identifier.GetLocation());
                        return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
                    }

                    return ApplyAccessibilityAndRefKind(zeroArity, ident.Identifier.GetLocation(), refKindHint);
                }

                var normalized = NormalizeDefinition(named);
                if (!IsSymbolAccessible(normalized))
                    return ReportInaccessibleType(normalized, ident.Identifier.GetLocation(), refKindHint);

                return ApplyRefKindHint(normalized, refKindHint);
            }

            if (type is not null)
            {
                if (!IsSymbolAccessible(type))
                    return ReportInaccessibleType(type, ident.Identifier.GetLocation(), refKindHint);

                return ApplyRefKindHint(type, refKindHint);
            }
        }

        if (typeSyntax is GenericNameSyntax generic)
        {
            var arity = ComputeGenericArity(generic);
            var typeArgs = ResolveGenericTypeArguments(generic);

            var symbol = FindNamedTypeForGeneric(generic, arity);

            if (symbol is null)
            {
                _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(generic.Identifier.ValueText, generic.GetLocation());
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
            }

            if (!ValidateTypeArgumentConstraints(symbol, typeArgs, i => GetTypeArgumentLocation(generic.TypeArgumentList.Arguments, generic.GetLocation(), i), symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);

            var constructed = TryConstructGeneric(symbol, typeArgs, arity);
            if (constructed is not null)
                return ApplyAccessibilityAndRefKind(constructed, generic.Identifier.GetLocation(), refKindHint);

            return ApplyAccessibilityAndRefKind(symbol, generic.Identifier.GetLocation(), refKindHint);
        }

        if (typeSyntax is QualifiedNameSyntax qualified)
        {
            var symbol = ResolveQualifiedType(qualified);
            if (symbol is not null)
            {
                if (!IsSymbolAccessible(symbol))
                    return ReportInaccessibleType(symbol, qualified.GetLocation(), refKindHint);

                return ApplyRefKindHint(symbol, refKindHint);
            }
        }

        var name = typeSyntax switch
        {
            IdentifierNameSyntax id => id.Identifier.ValueText,
            _ => typeSyntax.ToString()
        };

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, typeSyntax.GetLocation());
        return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
    }

    private static ITypeSymbol ApplyRefKindHint(ITypeSymbol type, RefKind? refKindHint)
    {
        if (refKindHint is not RefKind.Ref and not RefKind.Out and not RefKind.In and not RefKind.RefReadOnly and not RefKind.RefReadOnlyParameter)
            return type;

        if (type is ByRefTypeSymbol existing)
            return existing;

        return new ByRefTypeSymbol(type);
    }

    private ITypeSymbol ApplyAccessibilityAndRefKind(
        ITypeSymbol type,
        Location location,
        RefKind? refKindHint)
    {
        if (IsSymbolAccessible(type))
            return ApplyRefKindHint(type, refKindHint);

        return ReportInaccessibleType(type, location, refKindHint);
    }

    private ITypeSymbol ReportInaccessibleType(
        ITypeSymbol type,
        Location location,
        RefKind? refKindHint)
    {
        var display = type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        _diagnostics.ReportSymbolIsInaccessible("type", display, location);
        return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
    }

    private ITypeSymbol? ResolveQualifiedType(QualifiedNameSyntax qualified)
    {
        var symbol = ResolveQualifiedNamespaceOrType(qualified);
        return symbol as ITypeSymbol;
    }

    private INamespaceOrTypeSymbol? ResolveQualifiedNamespaceOrType(QualifiedNameSyntax qualified)
    {
        var left = ResolveQualifiedLeft(qualified.Left);
        INamespaceOrTypeSymbol? resolved = null;

        if (left is INamespaceSymbol ns)
        {
            if (qualified.Right is IdentifierNameSyntax id)
            {
                resolved = (INamespaceOrTypeSymbol?)ns.LookupNamespace(id.Identifier.ValueText)
                    ?? SelectByArity(ns.GetMembers(id.Identifier.ValueText).OfType<INamedTypeSymbol>(), 0)
                    ?? ns.LookupType(id.Identifier.ValueText);

                if (resolved is INamedTypeSymbol named && NormalizeDefinition(named).Arity > 0)
                {
                    _diagnostics.ReportTypeRequiresTypeArguments(named.Name, named.Arity, id.Identifier.GetLocation());
                    return Compilation.ErrorTypeSymbol;
                }
            }
            else if (qualified.Right is GenericNameSyntax gen)
            {
                resolved = ResolveGenericMember(ns, gen);
            }
        }
        else if (left is ITypeSymbol leftType)
        {
            if (qualified.Right is IdentifierNameSyntax id)
            {
                resolved = SelectByArity(leftType.GetMembers(id.Identifier.ValueText)
                    .OfType<INamedTypeSymbol>(), 0);
            }
            else if (qualified.Right is GenericNameSyntax gen)
            {
                resolved = ResolveGenericMember(leftType, gen);
            }
        }

        if (resolved is null)
        {
            var metadataName = ToMetadataName(qualified);
            if (!string.IsNullOrEmpty(metadataName) && Compilation.GetTypeByMetadataName(metadataName) is { } metadataType)
                return metadataType;

            return left;
        }

        return resolved;
    }

    private string? ToMetadataName(QualifiedNameSyntax qualified)
    {
        // Convert a qualified name syntax into a metadata name usable with GetTypeByMetadataName.
        // Examples:
        //   System.Collections.Generic.List<int>  -> System.Collections.Generic.List`1
        //   System.Result<int, CustomError>.Ok    -> System.Result`2+Ok
        //
        // Notes:
        // - Namespace separators are '.'
        // - Nested type separators are '+'
        // - Generic arity is encoded as `N on the type name
        // - Type arguments are ignored (metadata lookup expects definitions)

        var parts = new List<NameSyntax>();
        CollectQualifiedParts(qualified, parts);

        if (parts.Count == 0)
            return null;

        // First, consume namespace segments from the global namespace.
        var nsParts = new List<string>();
        var currentNs = Compilation.GlobalNamespace;
        int typeStartIndex = -1;

        for (int i = 0; i < parts.Count; i++)
        {
            var part = parts[i];

            // A generic name can never be a namespace.
            if (part is GenericNameSyntax)
            {
                typeStartIndex = i;
                break;
            }

            if (part is IdentifierNameSyntax id)
            {
                var text = id.Identifier.ValueText;
                var nestedNs = currentNs.LookupNamespace(text);
                if (nestedNs is not null)
                {
                    nsParts.Add(text);
                    currentNs = nestedNs;
                    continue;
                }

                // Not a namespace: this is the first type segment.
                typeStartIndex = i;
                break;
            }

            // Unknown name kind -> cannot build metadata name reliably.
            return null;
        }

        if (typeStartIndex < 0)
            return null;

        // Build type segments (first is top-level type, remaining are nested types).
        var typeSegments = new List<string>();
        for (int i = typeStartIndex; i < parts.Count; i++)
        {
            var part = parts[i];
            switch (part)
            {
                case IdentifierNameSyntax id:
                    typeSegments.Add(id.Identifier.ValueText);
                    break;
                case GenericNameSyntax gen:
                    var arity = ComputeGenericArity(gen);
                    typeSegments.Add(gen.Identifier.ValueText + "`" + arity);
                    break;
                default:
                    return null;
            }
        }

        if (typeSegments.Count == 0)
            return null;

        var nsPrefix = nsParts.Count > 0 ? string.Join(".", nsParts) : null;
        var typePath = string.Join("+", typeSegments);

        return nsPrefix is null ? typePath : nsPrefix + "." + typePath;

        static void CollectQualifiedParts(QualifiedNameSyntax q, List<NameSyntax> builder)
        {
            // Left side can be another qualified name.
            if (q.Left is QualifiedNameSyntax leftQualified)
            {
                CollectQualifiedParts(leftQualified, builder);
            }
            else if (q.Left is NameSyntax leftName)
            {
                builder.Add(leftName);
            }

            if (q.Right is NameSyntax rightName)
                builder.Add(rightName);
        }
    }

    private INamespaceOrTypeSymbol? ResolveQualifiedLeft(TypeSyntax left)
    {
        if (left is IdentifierNameSyntax id)
        {
            var type = LookupType(id.Identifier.ValueText);
            if (type is INamedTypeSymbol named)
            {
                var definition = NormalizeDefinition(named);
                if (definition.Arity > 0)
                {
                    var zeroArity = FindAccessibleNamedType(id.Identifier.ValueText, 0);
                    if (zeroArity is null)
                    {
                        _diagnostics.ReportTypeRequiresTypeArguments(named.Name, named.Arity, id.Identifier.GetLocation());
                        return Compilation.ErrorTypeSymbol;
                    }

                    return zeroArity;
                }

                return definition;
            }

            if (type is not null)
                return type;

            var ns = LookupNamespace(id.Identifier.ValueText);
            if (ns is not null)
                return ns;

            return null;
        }

        if (left is GenericNameSyntax gen)
        {
            var arity = ComputeGenericArity(gen);
            var typeArgs = ResolveGenericTypeArguments(gen);

            var symbol = FindNamedTypeForGeneric(gen, arity);
            if (symbol is null)
                return null;

            if (!ValidateTypeArgumentConstraints(symbol, typeArgs, i => GetTypeArgumentLocation(gen.TypeArgumentList.Arguments, gen.GetLocation(), i), symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                return Compilation.ErrorTypeSymbol;

            var constructed = TryConstructGeneric(symbol, typeArgs, arity);
            return constructed ?? symbol;
        }

        if (left is QualifiedNameSyntax qualified)
        {
            return ResolveQualifiedNamespaceOrType(qualified);
        }

        return null;
    }

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
            {
                return ResolveGenericMember(ns, gen);
            }

            return null;
        }

        if (left is ITypeSymbol type)
        {
            if (qn.Right is IdentifierNameSyntax id)
                return SelectByArity(type.GetMembers(id.Identifier.ValueText)
                    .OfType<INamedTypeSymbol>(), 0);

            if (qn.Right is GenericNameSyntax gen)
            {
                return ResolveGenericMember(type, gen);
            }
        }

        return null;
    }

    public virtual IEnumerable<ISymbol> LookupAvailableSymbols()
    {
        return [];
    }

    public ISymbol? LookupLocalSymbol(string name)
    {
        return LookupSymbol(name);
    }

    public virtual BoundFunctionStatement BindFunction(FunctionStatementSyntax function)
    {
        return ParentBinder?.BindFunction(function)
             ?? throw new NotImplementedException("BindFunction not implemented in root binder.");
    }

    protected BoundNode? TryGetCachedBoundNode(SyntaxNode node)
        => SemanticModel?.TryGetCachedBoundNode(node);

    protected void CacheBoundNode(SyntaxNode node, BoundNode bound)
    {
        SemanticModel?.CacheBoundNode(node, bound, this);
    }

    protected void RemoveCachedBoundNode(SyntaxNode node)
    {
        SemanticModel?.RemoveCachedBoundNode(node);
    }

    public virtual BoundNode GetOrBind(SyntaxNode node)
    {
        if (TryGetCachedBoundNode(node) is BoundNode cached)
            return cached;

        if (node is LambdaExpressionSyntax lambdaSyntax &&
            lambdaSyntax.Parent is ArgumentSyntax argument &&
            argument.Parent is ArgumentListSyntax argumentList &&
            argumentList.Parent is InvocationExpressionSyntax invocation)
        {
            _ = BindExpression(invocation);

            if (TryGetCachedBoundNode(node) is BoundNode rebound)
                return rebound;
        }

        BoundNode result = node switch
        {
            ExpressionSyntax expr => BindExpression(expr),
            StatementSyntax stmt => BindStatement(stmt),
            _ => throw new NotSupportedException($"Unsupported node kind: {node.Kind}")
        };
        return result;
    }

    private static int ComputeGenericArity(GenericNameSyntax generic)
    {
        var argumentCount = generic.TypeArgumentList.Arguments.Count;
        var separators = generic.TypeArgumentList.Arguments.SeparatorCount + 1;

        if (argumentCount == 0)
            return Math.Max(1, separators);

        return Math.Max(argumentCount, separators);
    }

    private ImmutableArray<ITypeSymbol> ResolveGenericTypeArguments(GenericNameSyntax generic)
    {
        if (generic.TypeArgumentList.Arguments.Count == 0)
            return ImmutableArray<ITypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(generic.TypeArgumentList.Arguments.Count);
        foreach (var argument in generic.TypeArgumentList.Arguments)
            builder.Add(ResolveType(argument.Type));

        return builder.MoveToImmutable();
    }

    protected INamedTypeSymbol? FindAccessibleNamedType(string name, int arity)
    {
        foreach (var symbol in LookupSymbols(name))
        {
            if (symbol is INamedTypeSymbol named)
            {
                var candidate = NormalizeDefinition(named);
                if (candidate.Arity == arity && IsSymbolAccessible(candidate))
                    return candidate;
            }
        }

        if (LookupType(name) is INamedTypeSymbol fallback)
        {
            var candidate = NormalizeDefinition(fallback);
            if (candidate.Arity == arity && IsSymbolAccessible(candidate))
                return candidate;
        }

        return null;
    }

    protected static INamedTypeSymbol NormalizeDefinition(INamedTypeSymbol named)
        => named.ConstructedFrom as INamedTypeSymbol ?? named;

    protected ITypeSymbol? TryConstructGeneric(INamedTypeSymbol definition, ImmutableArray<ITypeSymbol> typeArguments, int arity)
    {
        if (typeArguments.Length != arity)
            return null;

        if (typeArguments.Any(t => t == Compilation.ErrorTypeSymbol))
            return Compilation.ErrorTypeSymbol;

        return Compilation.ConstructGenericType(definition, typeArguments.ToArray());
    }

    protected bool ValidateTypeArgumentConstraints(
        INamedTypeSymbol definition,
        ImmutableArray<ITypeSymbol> typeArguments,
        Func<int, Location> getArgumentLocation,
        string? genericDisplayName = null)
    {
        var typeParameters = definition.TypeParameters;
        if (typeParameters.Length != typeArguments.Length)
            return true;

        bool allSatisfied = true;
        var displayName = genericDisplayName ?? definition.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        EnsureTypeParameterConstraintTypesResolved(typeParameters);

        for (int i = 0; i < typeParameters.Length; i++)
        {
            var typeParameter = typeParameters[i];
            var typeArgument = typeArguments[i];
            var constraintKind = typeParameter.ConstraintKind;

            if ((constraintKind & TypeParameterConstraintKind.ReferenceType) != 0 && !SemanticFacts.SatisfiesReferenceTypeConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "class", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if ((constraintKind & TypeParameterConstraintKind.ValueType) != 0 && !SemanticFacts.SatisfiesValueTypeConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "struct", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if ((constraintKind & TypeParameterConstraintKind.NotNull) != 0 && !SemanticFacts.SatisfiesNotNullConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "notnull", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if ((constraintKind & TypeParameterConstraintKind.Constructor) != 0 && !SemanticFacts.SatisfiesConstructorConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "new()", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if ((constraintKind & TypeParameterConstraintKind.TypeConstraint) != 0)
            {
                var constraintTypes = typeParameter.ContainingSymbol is SubstitutedMethodSymbol substituted &&
                    substituted.TryGetSubstitutedConstraintTypes(typeParameter, out var substitutedConstraints)
                        ? substitutedConstraints
                        : typeParameter.ConstraintTypes;

                foreach (var constraintType in constraintTypes)
                {
                    if (constraintType is IErrorTypeSymbol)
                        continue;

                    if (constraintType is INamedTypeSymbol namedConstraint)
                    {
                        if (SemanticFacts.SatisfiesNamedTypeConstraint(typeArgument, namedConstraint))
                            continue;

                        var constraintDisplay = namedConstraint.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                        ReportConstraintViolation(typeArgument, constraintDisplay, typeParameter, displayName, getArgumentLocation(i));
                        allSatisfied = false;
                        continue;
                    }

                    if (SemanticFacts.SatisfiesTypeConstraint(typeArgument, constraintType))
                        continue;

                    var display = constraintType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    ReportConstraintViolation(typeArgument, display, typeParameter, displayName, getArgumentLocation(i));
                    allSatisfied = false;
                }
            }
        }

        return allSatisfied;
    }

    internal void EnsureTypeParameterConstraintTypesResolved(ImmutableArray<ITypeParameterSymbol> typeParameters)
    {
        foreach (var parameter in typeParameters)
            EnsureTypeParameterConstraintTypesResolved(parameter);
    }

    private void EnsureTypeParameterConstraintTypesResolved(ITypeParameterSymbol typeParameter)
    {
        if (typeParameter is not SourceTypeParameterSymbol source || source.HasResolvedConstraintTypes)
            return;

        if (source.ConstraintTypeReferences.IsDefaultOrEmpty)
        {
            source.SetConstraintTypes(ImmutableArray<ITypeSymbol>.Empty);
            return;
        }

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(source.ConstraintTypeReferences.Length);
        foreach (var reference in source.ConstraintTypeReferences)
        {
            if (reference.GetSyntax() is TypeConstraintSyntax typeConstraint)
            {
                var resolved = ResolveType(typeConstraint.Type);
                builder.Add(resolved);
            }
        }

        source.SetConstraintTypes(builder.MoveToImmutable());
    }

    private void ReportConstraintViolation(ITypeSymbol typeArgument, string constraintDisplay, ITypeParameterSymbol typeParameter, string genericDisplayName, Location location)
    {
        var argumentDisplay = typeArgument.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);
        _diagnostics.ReportTypeArgumentDoesNotSatisfyConstraint(argumentDisplay, constraintDisplay, typeParameter.Name, genericDisplayName, location);
    }

    private void ReportNotNullConstraintViolation(ITypeParameterSymbol typeParameter, Location location)
    {
        var argumentDisplay = $"{typeParameter.Name}?";
        var genericDisplayName = typeParameter.ContainingSymbol?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)
            ?? typeParameter.Name;
        _diagnostics.ReportTypeArgumentDoesNotSatisfyConstraint(argumentDisplay, "notnull", typeParameter.Name, genericDisplayName, location);
    }

    protected static Location GetTypeArgumentLocation<TNode>(SeparatedSyntaxList<TNode> arguments, Location fallback, int index)
        where TNode : SyntaxNode
    {
        if (index >= 0 && index < arguments.Count)
            return arguments[index].GetLocation();

        return fallback;
    }

    protected bool ValidateMethodTypeArgumentConstraints(
        IMethodSymbol method,
        ImmutableArray<ITypeSymbol> typeArguments,
        Func<int, Location> getArgumentLocation)
    {
        var typeParameters = method.TypeParameters;
        if (typeParameters.Length != typeArguments.Length)
            return true;

        bool allSatisfied = true;
        var displayName = method.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        for (int i = 0; i < typeParameters.Length; i++)
        {
            var typeParameter = typeParameters[i];
            var typeArgument = typeArguments[i];
            var constraintKind = typeParameter.ConstraintKind;

            if ((constraintKind & TypeParameterConstraintKind.ReferenceType) != 0 && !SemanticFacts.SatisfiesReferenceTypeConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "class", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if ((constraintKind & TypeParameterConstraintKind.ValueType) != 0 && !SemanticFacts.SatisfiesValueTypeConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "struct", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if ((constraintKind & TypeParameterConstraintKind.NotNull) != 0 && !SemanticFacts.SatisfiesNotNullConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "notnull", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if ((constraintKind & TypeParameterConstraintKind.Constructor) != 0 && !SemanticFacts.SatisfiesConstructorConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "new()", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if ((constraintKind & TypeParameterConstraintKind.TypeConstraint) != 0)
            {
                foreach (var constraintType in typeParameter.ConstraintTypes)
                {
                    if (constraintType is IErrorTypeSymbol)
                        continue;

                    if (constraintType is INamedTypeSymbol namedConstraint)
                    {
                        if (SemanticFacts.SatisfiesNamedTypeConstraint(typeArgument, namedConstraint))
                            continue;

                        var constraintDisplay = namedConstraint.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                        ReportConstraintViolation(typeArgument, constraintDisplay, typeParameter, displayName, getArgumentLocation(i));
                        allSatisfied = false;
                        continue;
                    }

                    if (SemanticFacts.SatisfiesTypeConstraint(typeArgument, constraintType))
                        continue;

                    var display = constraintType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    ReportConstraintViolation(typeArgument, display, typeParameter, displayName, getArgumentLocation(i));
                    allSatisfied = false;
                }
            }
        }

        return allSatisfied;
    }

    private ITypeSymbol? ResolveGenericMember(INamespaceOrTypeSymbol container, GenericNameSyntax generic)
    {
        var arity = ComputeGenericArity(generic);
        var definition = SelectByArity(container.GetMembers(generic.Identifier.ValueText)
            .OfType<INamedTypeSymbol>(), arity);

        if (definition is null)
            return null;

        var typeArguments = ResolveGenericTypeArguments(generic);
        if (!ValidateTypeArgumentConstraints(definition, typeArguments, i => GetTypeArgumentLocation(generic.TypeArgumentList.Arguments, generic.GetLocation(), i), definition.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
            return Compilation.ErrorTypeSymbol;

        var constructed = TryConstructGeneric(definition, typeArguments, arity);

        return constructed ?? definition;
    }

    private static INamedTypeSymbol? SelectByArity(IEnumerable<INamedTypeSymbol> candidates, int arity)
    {
        foreach (var candidate in candidates)
        {
            var definition = NormalizeDefinition(candidate);
            if (definition.Arity != arity)
                continue;

            if (candidate is ConstructedNamedTypeSymbol constructed)
                return constructed;

            return definition;
        }

        return null;
    }

    private INamedTypeSymbol? FindNamedTypeForGeneric(GenericNameSyntax generic, int arity)
    {
        var symbol = LookupType(generic.Identifier.ValueText) as INamedTypeSymbol;
        if (symbol is not null)
        {
            symbol = NormalizeDefinition(symbol);
            if (symbol.Arity != arity)
                symbol = FindAccessibleNamedType(generic.Identifier.ValueText, arity);
        }
        else
        {
            symbol = FindAccessibleNamedType(generic.Identifier.ValueText, arity);
        }

        return symbol;
    }

    protected bool IsValidAsyncReturnType(ITypeSymbol? type)
    {
        if (type is null)
            return false;

        if (type.TypeKind == TypeKind.Error)
            return true;

        if (type is NullableTypeSymbol nullable)
            type = nullable.UnderlyingType;

        if (type.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return true;

        if (type is INamedTypeSymbol named &&
            named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T)
        {
            return true;
        }

        return false;
    }
}
