using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal readonly struct BinderExtensionLookupKey : IEquatable<BinderExtensionLookupKey>
{
    public BinderExtensionLookupKey(string? name, TypeSymbol receiverType, bool includePartialMatches)
    {
        Name = name;
        ReceiverType = receiverType;
        ReceiverIdentity = receiverType.GetLookupIdentityKey();
        IncludePartialMatches = includePartialMatches;
    }

    public string? Name { get; }

    public TypeSymbol ReceiverType { get; }

    private string ReceiverIdentity { get; }

    public bool IncludePartialMatches { get; }

    public bool Equals(BinderExtensionLookupKey other)
    {
        return string.Equals(Name, other.Name, StringComparison.Ordinal) &&
               IncludePartialMatches == other.IncludePartialMatches &&
               string.Equals(ReceiverIdentity, other.ReceiverIdentity, StringComparison.Ordinal);
    }

    public override bool Equals(object? obj)
        => obj is BinderExtensionLookupKey other && Equals(other);

    public override int GetHashCode()
    {
        var hash = new HashCode();
        hash.Add(Name, StringComparer.Ordinal);
        hash.Add(IncludePartialMatches);
        hash.Add(ReceiverIdentity, StringComparer.Ordinal);
        return hash.ToHashCode();
    }
}

internal abstract partial class Binder
{
    protected readonly DiagnosticBag _diagnostics;

    protected Binder(Binder? parent, DiagnosticBag? diagnostics = null)
    {
        ParentBinder = parent!;
        _diagnostics = diagnostics ?? new DiagnosticBag();
    }

    public Binder? ParentBinder { get; }

    public DiagnosticBag Diagnostics => _diagnostics;

    public virtual Symbol? ContainingSymbol => null;

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

    protected virtual bool IsInUnsafeContext => ParentBinder?.IsInUnsafeContext ?? false;

    protected bool IsUnsafeEnabled => Compilation.Options.AllowUnsafe || IsInUnsafeContext;

    protected BoundErrorExpression ErrorExpression(
        TypeSymbol? type = null,
        Symbol? symbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        ImmutableArray<Symbol> candidates = default)
        => BoundFactory.ErrorExpression(type, symbol, reason, candidates);

    protected void ReportCannotConvertFromTypeToType(object? fromType, object? toType, Location location)
    {
        if (fromType is ArrayTypeSymbol fromArray && toType is ArrayTypeSymbol toArray)
        {
            var fromDisplay = fromArray.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);
            var toDisplay = toArray.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);

            if (fromArray.FixedLength is null && toArray.FixedLength is not null)
            {
                _diagnostics.ReportCannotConvertOpenArrayToFixedSizeArray(fromDisplay, toDisplay, location);
                return;
            }

            if (fromArray.FixedLength is int fromLength && toArray.FixedLength is int toLength && fromLength != toLength)
            {
                _diagnostics.ReportCannotConvertFixedSizeArrayToDifferentSize(fromDisplay, toDisplay, fromLength, toLength, location);
                return;
            }
        }

        var fromArg = fromType is TypeSymbol fromTypeSymbolForDisplay
            ? fromTypeSymbolForDisplay.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat)
            : fromType;
        var toArg = toType is TypeSymbol toTypeSymbolForDisplay
            ? toTypeSymbolForDisplay.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat)
            : toType;

        _diagnostics.ReportCannotConvertFromTypeToType(fromArg, toArg, location);

        ReportExplicitConversionHint(fromType, toType, location);
    }

    protected void ReportExplicitConversionHint(object? fromType, object? toType, Location location)
    {
        if (fromType is not TypeSymbol fromTypeSymbol || toType is not TypeSymbol toTypeSymbol)
            return;

        if (fromTypeSymbol.ContainsErrorType() || toTypeSymbol.ContainsErrorType())
            return;

        var conversion = Compilation.ClassifyConversion(fromTypeSymbol, toTypeSymbol);
        if (!conversion.Exists || conversion.IsImplicit)
            return;

        _diagnostics.ReportExplicitConversionExists(
            fromTypeSymbol.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
            toTypeSymbol.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
            location);
    }

    protected void ReportCannotAssignFromTypeToType(TypeSymbol fromType, TypeSymbol toType, Location location)
    {
        if (fromType is ArrayTypeSymbol fromArray && toType is ArrayTypeSymbol toArray)
        {
            var fromDisplay = fromArray.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);
            var toDisplay = toArray.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);

            if (fromArray.FixedLength is null && toArray.FixedLength is not null)
            {
                _diagnostics.ReportCannotConvertOpenArrayToFixedSizeArray(fromDisplay, toDisplay, location);
                return;
            }

            if (fromArray.FixedLength is int fromLength && toArray.FixedLength is int toLength && fromLength != toLength)
            {
                _diagnostics.ReportCannotConvertFixedSizeArrayToDifferentSize(fromDisplay, toDisplay, fromLength, toLength, location);
                return;
            }
        }

        var toArg = toType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);
        if (fromType.TypeKind == TypeKind.Null)
        {
            _diagnostics.ReportCannotAssignNullToType(toArg, location);
            return;
        }

        var fromArg = fromType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);
        _diagnostics.ReportCannotAssignFromTypeToType(fromArg, toArg, location);
        ReportExplicitConversionHint(fromType, toType, location);
    }

    protected void ReportCannotAssignExpressionToType(BoundExpression expression, TypeSymbol toType, Location location)
    {
        if (IsNullValueExpression(expression) && !toType.IsNullable)
        {
            var toArg = toType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);
            _diagnostics.ReportCannotAssignNullToType(toArg, location);
            return;
        }

        ReportCannotAssignFromTypeToType(expression.Type!, toType, location);
    }

    protected void ReportCannotConvertExpressionToType(BoundExpression expression, TypeSymbol toType, Location location)
    {
        if (IsNullValueExpression(expression) && !toType.IsNullable)
        {
            var toArg = toType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);
            _diagnostics.ReportCannotAssignNullToType(toArg, location);
            return;
        }

        ReportCannotConvertFromTypeToType(
            expression.Type!.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            toType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            location);
    }

    private static bool IsNullValueExpression(BoundExpression expression)
        => expression switch
        {
            BoundLiteralExpression { Kind: BoundLiteralExpressionKind.NullLiteral } => true,
            BoundDefaultValueExpression { Type.IsNullable: true } => true,
            BoundParenthesizedExpression parenthesized => IsNullValueExpression(parenthesized.Expression),
            _ => false,
        };

    protected internal bool IsSymbolAccessible(Symbol symbol)
    {
        if (symbol is null)
            return true;

        if (symbol is not SymbolBase { IsFileScoped: true } &&
            symbol.DeclaredAccessibility is (Accessibility.Internal or Accessibility.ProtectedOrInternal) &&
            Compilation.IsPreviousSubmissionAssembly(symbol.ContainingAssembly))
        {
            return true;
        }

        var within = GetAccessibilityContext();
        return AccessibilityUtilities.IsAccessible(symbol, within, GetCurrentSourceFilePath());
    }

    private Symbol? GetAccessibilityContext()
    {
        for (var current = this; current is not null; current = current.ParentBinder)
        {
            // Explicit ExtensionAttribute methods still belong to ordinary static
            // classes and retain access to their private implementation helpers.
            if (current.ContainingSymbol is MethodSymbol { IsExtensionMethod: true } method &&
                method.ContainingType?.OriginalDefinition is SourceNamedTypeSymbol { IsExtensionDeclaration: true })
            {
                return Compilation.Assembly;
            }

            if (current.ContainingSymbol is not null)
                return current.ContainingSymbol;
        }

        return Compilation.Assembly;
    }

    private string? GetCurrentSourceFilePath()
        => SemanticModel?.SyntaxTree?.FilePath;

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

    public virtual NamespaceSymbol? CurrentNamespace => ParentBinder?.CurrentNamespace;

    public virtual Symbol? BindDeclaredSymbol(SyntaxNode node) => ParentBinder?.BindDeclaredSymbol(node);

    internal virtual void EnsureStatementDeclarations(StatementSyntax statement)
        => ParentBinder?.EnsureStatementDeclarations(statement);

    public virtual SymbolInfo BindReferencedSymbol(SyntaxNode node)
    {
        // Handle type syntax first so qualified names and generics are resolved correctly.
        if (node is TypeSyntax typeSyntax && node is not IdentifierNameSyntax)
        {
            try
            {
                using (_diagnostics.CreateNonReportingScope())
                {
                    var result = BindTypeSyntax(typeSyntax);
                    return result.Success
                        ? new SymbolInfo(result.ResolvedType)
                        : SymbolInfo.None;
                }
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

    public virtual TypeSymbol? LookupType(string name)
    {
        var type = LookupTypeInNamespace(CurrentNamespace, name, includeGlobalFallback: CurrentNamespace?.IsGlobalNamespace != false);
        return type ?? ParentBinder?.LookupType(name);
    }

    protected TypeSymbol? LookupTypeInNamespace(NamespaceSymbol? scope, string name, bool includeGlobalFallback)
        => Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
            ? Compilation.SymbolLookup.LookupTypeSourceFirst(scope, name, includeGlobalFallback)
            : scope?.LookupType(name) ?? Compilation.SymbolLookup.LookupTypeSourceFirst(scope, name, includeGlobalFallback);

    public virtual NamespaceSymbol? LookupNamespace(string name)
    {
        var currentNamespace = Compilation.SymbolLookup.LookupNamespaceSourceFirst(CurrentNamespace, name,
            includeGlobalFallback: CurrentNamespace?.IsGlobalNamespace != false);
        if (currentNamespace is not null)
            return currentNamespace;

        var namespaceFromParent = ParentBinder?.LookupNamespace(name);
        if (namespaceFromParent is not null)
            return namespaceFromParent;

        return Compilation.SymbolLookup.LookupNamespaceSourceFirst(null, name);
    }

    public virtual Symbol? LookupSymbol(string name)
    {
        return ParentBinder?.LookupSymbol(name);
    }

    public virtual IEnumerable<Symbol> LookupSymbols(string name)
    {
        return ParentBinder?.LookupSymbols(name) ?? Enumerable.Empty<Symbol>();
    }

    protected IEnumerable<NamedTypeSymbol> LookupNamedTypeCandidates(string name)
    {
        var seen = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var symbol in LookupSymbols(name))
        {
            if (symbol is not NamedTypeSymbol named)
                continue;

            var normalized = NormalizeDefinition(named);
            if (IsSymbolAccessible(normalized) && seen.Add(normalized))
                yield return normalized;
        }

        // LookupType returns one declaration. A namespace can contain the same
        // simple name at several arities, so retain the complete local family for
        // arity selection (including unqualified constructor expressions).
        if (CurrentNamespace is { } currentNamespace)
        {
            foreach (var named in currentNamespace.GetMembers(name).OfType<NamedTypeSymbol>())
            {
                var normalized = NormalizeDefinition(named);
                if (IsSymbolAccessible(normalized) && seen.Add(normalized))
                    yield return normalized;
            }
        }

        if (LookupType(name) is NamedTypeSymbol fallback)
        {
            var normalized = NormalizeDefinition(fallback);
            if (IsSymbolAccessible(normalized) && seen.Add(normalized))
                yield return normalized;
        }
    }

    protected static IEnumerable<Symbol> EnumerateTypeAndBaseMembers(NamedTypeSymbol containingType, string name)
    {
        if (containingType.TypeKind == TypeKind.Interface)
        {
            // Interface inheritance is represented by Interfaces, not BaseType.
            // AllInterfaces preserves constructed type arguments and visits a
            // shared ancestor only once, including diamond inheritance.
            foreach (var member in containingType.GetMembers(name))
                yield return member;
            foreach (var inherited in containingType.AllInterfaces)
            {
                foreach (var member in inherited.GetMembers(name))
                    yield return member;
            }
            yield break;
        }

        HashSet<Symbol>? overridden = null;
        List<MethodSymbol>? overridingMethods = null;
        for (var current = containingType; current is not null; current = current.BaseType)
        {
            foreach (var member in current.GetMembers(name))
            {
                // Unqualified calls see the most-derived override, just like self.M().
                // Preserve unrelated inherited overloads and generic substitutions.
                if (member is MethodSymbol method)
                {
                    if (overridden?.Contains(method.OriginalDefinition) == true ||
                        overridingMethods?.Any(derived => OverridesInheritedSignature(derived, method)) == true)
                        continue;
                    if (method.IsOverride)
                    {
                        overridingMethods ??= [];
                        overridingMethods.Add(method);
                    }
                    for (var replaced = (method.OriginalDefinition as SourceMethodSymbol)?.OverriddenMethod; replaced is not null; replaced = (replaced.OriginalDefinition as SourceMethodSymbol)?.OverriddenMethod)
                    {
                        overridden ??= new HashSet<Symbol>(SymbolEqualityComparer.Default);
                        overridden.Add(replaced.OriginalDefinition);
                    }
                }
                yield return member;
            }
        }
    }

    private static bool OverridesInheritedSignature(MethodSymbol derived, MethodSymbol inherited)
    {
        // Imported overrides do not expose SourceMethodSymbol's override link. Match
        // their inherited virtual slot in the derived method's generic context.
        if (!(inherited.IsVirtual || inherited.IsOverride || inherited.IsAbstract) ||
            derived.IsStatic != inherited.IsStatic || derived.Arity != inherited.Arity ||
            derived.Parameters.Length != inherited.Parameters.Length ||
            SymbolEqualityComparer.Default.Equals(derived.ContainingType, inherited.ContainingType))
            return false;

        var signature = inherited.Arity == 0
            ? inherited
            : inherited.Construct(derived.TypeParameters.Cast<TypeSymbol>().ToArray());
        for (var i = 0; i < derived.Parameters.Length; i++)
        {
            if (derived.Parameters[i].RefKind != signature.Parameters[i].RefKind ||
                !SymbolEqualityComparer.Default.Equals(derived.Parameters[i].Type, signature.Parameters[i].Type))
                return false;
        }
        return true;
    }

    public virtual IEnumerable<MethodSymbol> LookupExtensionMethods(string? name, TypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null)
            return Enumerable.Empty<MethodSymbol>();

        if (receiverType.TypeKind == TypeKind.Error)
            return Enumerable.Empty<MethodSymbol>();

        return ParentBinder?.LookupExtensionMethods(name, receiverType, includePartialMatches) ?? Enumerable.Empty<MethodSymbol>();
    }

    public virtual bool TryGetCachedExtensionMethods(
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<MethodSymbol> methods)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            methods = ImmutableArray<MethodSymbol>.Empty;
            return false;
        }

        if (ParentBinder is not null)
            return ParentBinder.TryGetCachedExtensionMethods(name, receiverType, includePartialMatches, out methods);

        methods = ImmutableArray<MethodSymbol>.Empty;
        return false;
    }

    public virtual IEnumerable<PropertySymbol> LookupExtensionProperties(string? name, TypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null)
            return Enumerable.Empty<PropertySymbol>();

        if (receiverType.TypeKind == TypeKind.Error)
            return Enumerable.Empty<PropertySymbol>();

        return ParentBinder?.LookupExtensionProperties(name, receiverType, includePartialMatches) ?? Enumerable.Empty<PropertySymbol>();
    }

    public virtual bool TryGetCachedExtensionProperties(
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<PropertySymbol> properties)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            properties = ImmutableArray<PropertySymbol>.Empty;
            return false;
        }

        if (ParentBinder is not null)
            return ParentBinder.TryGetCachedExtensionProperties(name, receiverType, includePartialMatches, out properties);

        properties = ImmutableArray<PropertySymbol>.Empty;
        return false;
    }

    public virtual IEnumerable<MethodSymbol> LookupExtensionStaticMethods(string? name, TypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null)
            return Enumerable.Empty<MethodSymbol>();

        if (receiverType.TypeKind == TypeKind.Error)
            return Enumerable.Empty<MethodSymbol>();

        return ParentBinder?.LookupExtensionStaticMethods(name, receiverType, includePartialMatches) ?? Enumerable.Empty<MethodSymbol>();
    }

    public virtual bool TryGetCachedExtensionStaticMethods(
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<MethodSymbol> methods)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            methods = ImmutableArray<MethodSymbol>.Empty;
            return false;
        }

        if (ParentBinder is not null)
            return ParentBinder.TryGetCachedExtensionStaticMethods(name, receiverType, includePartialMatches, out methods);

        methods = ImmutableArray<MethodSymbol>.Empty;
        return false;
    }

    public virtual IEnumerable<PropertySymbol> LookupExtensionStaticProperties(string? name, TypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null)
            return Enumerable.Empty<PropertySymbol>();

        if (receiverType.TypeKind == TypeKind.Error)
            return Enumerable.Empty<PropertySymbol>();

        return ParentBinder?.LookupExtensionStaticProperties(name, receiverType, includePartialMatches) ?? Enumerable.Empty<PropertySymbol>();
    }

    public virtual bool TryGetCachedExtensionStaticProperties(
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<PropertySymbol> properties)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            properties = ImmutableArray<PropertySymbol>.Empty;
            return false;
        }

        if (ParentBinder is not null)
            return ParentBinder.TryGetCachedExtensionStaticProperties(name, receiverType, includePartialMatches, out properties);

        properties = ImmutableArray<PropertySymbol>.Empty;
        return false;
    }

    public virtual IEnumerable<MethodSymbol> LookupExtensionMethodsByName(string name)
    {
        if (string.IsNullOrEmpty(name))
            return Enumerable.Empty<MethodSymbol>();

        return ParentBinder?.LookupExtensionMethodsByName(name) ?? Enumerable.Empty<MethodSymbol>();
    }

    protected bool CanCacheExtensionLookupResults
        => Compilation.SourceDeclarationsDeclared;

    protected IEnumerable<MethodSymbol> GetExtensionMethodsFromScope(
        NamespaceOrTypeSymbol scope,
        string? name,
        TypeSymbol receiverType,
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

    protected IEnumerable<MethodSymbol> GetExtensionMethodsByNameFromScope(
        NamespaceOrTypeSymbol scope,
        string name)
    {
        foreach (var method in EnumerateExtensionMethods(scope, name, includePartialMatches: false))
        {
            if (!IsSymbolAccessible(method))
                continue;

            yield return method;
        }
    }

    protected IEnumerable<PropertySymbol> GetExtensionPropertiesFromScope(
        NamespaceOrTypeSymbol scope,
        string? name,
        TypeSymbol receiverType,
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

    protected IEnumerable<MethodSymbol> GetExtensionStaticMethodsFromScope(
        NamespaceOrTypeSymbol scope,
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches)
    {
        foreach (var method in EnumerateExtensionStaticMethods(scope, name, includePartialMatches))
        {
            if (FrameworkProjectionCatalog.IsProjectionAdapter(method))
                continue;

            if (!IsExtensionStaticCandidateForReceiver(method, receiverType, includePartialMatches))
                continue;

            if (!IsSymbolAccessible(method))
                continue;

            yield return AdjustExtensionStaticMethodForReceiver(method, receiverType);
        }
    }

    protected IEnumerable<PropertySymbol> GetExtensionStaticPropertiesFromScope(
        NamespaceOrTypeSymbol scope,
        string? name,
        TypeSymbol receiverType,
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

    private MethodSymbol AdjustExtensionForReceiver(MethodSymbol method, TypeSymbol receiverType)
    {
        if (!TryIsInstanceExtensionMember(method) || receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return method;

        if (!TryCreateConstructedExtension(method, receiverType, out var constructed))
            return method;

        return constructed;
    }

    private PropertySymbol AdjustExtensionPropertyForReceiver(PropertySymbol property, TypeSymbol receiverType)
    {
        if (!property.IsExtensionProperty || receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return property;

        var getMethod = property.GetMethod;
        var setMethod = property.SetMethod;

        MethodSymbol? constructedGet = null;
        MethodSymbol? constructedSet = null;

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

    private MethodSymbol AdjustExtensionStaticMethodForReceiver(MethodSymbol method, TypeSymbol receiverType)
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

    private PropertySymbol AdjustExtensionStaticPropertyForReceiver(PropertySymbol property, TypeSymbol receiverType)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return property;

        var extensionReceiverType = property.GetExtensionReceiverType();
        if (extensionReceiverType is null || extensionReceiverType.TypeKind == TypeKind.Error)
            return property;

        var getMethod = property.GetMethod;
        var setMethod = property.SetMethod;

        MethodSymbol? constructedGet = null;
        MethodSymbol? constructedSet = null;

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

    private bool TryCreateConstructedExtension(MethodSymbol method, TypeSymbol receiverType, out MethodSymbol constructed)
    {
        constructed = method;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var methodDefinition = method.OriginalDefinition ?? method;

        if (!TryGetMethodParameterType(methodDefinition, 0, out var receiverParameterType))
            return false;

        if (receiverParameterType is null || receiverParameterType.TypeKind == TypeKind.Error)
            return false;

        var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);

        if (!TryUnifyExtensionReceiver(receiverParameterType, receiverType, substitutions))
            return false;

        if (!SatisfiesInferredTypeParameterConstraints(methodDefinition, substitutions))
            return false;

        if (!methodDefinition.TypeParameters.IsDefaultOrEmpty && methodDefinition.TypeParameters.Length > 0)
        {
            var methodTypeArguments = new TypeSymbol[methodDefinition.TypeParameters.Length];

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

        var container = methodDefinition.ContainingType as NamedTypeSymbol;
        if (container is null || !container.IsGenericType || container.TypeParameters.IsDefaultOrEmpty || container.TypeParameters.Length == 0)
            return false;

        var typeArguments = new TypeSymbol[container.TypeParameters.Length];

        for (int i = 0; i < container.TypeParameters.Length; i++)
        {
            var typeParameter = container.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                return false;

            typeArguments[i] = typeArgument;
        }

        var constructedContainer = container.Construct(typeArguments);
        if (constructedContainer is not NamedTypeSymbol namedContainer)
            return false;

        var originalDefinition = methodDefinition;

        foreach (var candidate in namedContainer.GetMembers(method.Name).OfType<MethodSymbol>())
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
        MethodSymbol method,
        TypeSymbol receiverType,
        TypeSymbol extensionReceiverType,
        out MethodSymbol constructed)
    {
        constructed = method;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var methodDefinition = method.OriginalDefinition ?? method;

        if (methodDefinition.ContainingType is not NamedTypeSymbol container)
            return false;

        if (extensionReceiverType is null || extensionReceiverType.TypeKind == TypeKind.Error)
            return false;

        var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);

        if (!TryUnifyExtensionReceiver(extensionReceiverType, receiverType, substitutions))
            return false;

        if (!SatisfiesInferredTypeParameterConstraints(methodDefinition, substitutions))
            return false;

        if (!methodDefinition.TypeParameters.IsDefaultOrEmpty && methodDefinition.TypeParameters.Length > 0)
        {
            var methodTypeArguments = new TypeSymbol[methodDefinition.TypeParameters.Length];

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

        var containerDefinition = container.ConstructedFrom as NamedTypeSymbol ?? container;
        if (!containerDefinition.IsGenericType || containerDefinition.TypeParameters.IsDefaultOrEmpty || containerDefinition.TypeParameters.Length == 0)
            return false;

        var typeArguments = new TypeSymbol[containerDefinition.TypeParameters.Length];

        for (int i = 0; i < containerDefinition.TypeParameters.Length; i++)
        {
            var typeParameter = containerDefinition.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                return false;

            typeArguments[i] = typeArgument;
        }

        var constructedContainer = containerDefinition.Construct(typeArguments);
        if (constructedContainer is not NamedTypeSymbol namedContainer)
            return false;

        var originalDefinition = methodDefinition;

        foreach (var candidate in namedContainer.GetMembers(method.Name).OfType<MethodSymbol>())
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
        PropertySymbol property,
        TypeSymbol receiverType,
        bool includePartialMatches)
    {
        var accessor = property.GetMethod ?? property.SetMethod;
        if (accessor is null || !accessor.IsInstanceExtensionMember)
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
            var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);
            return TryUnifyExtensionReceiver(parameterType, receiverType, substitutions) &&
                   SatisfiesInferredTypeParameterConstraints(accessor, substitutions);
        }

        if (SymbolEqualityComparer.Default.Equals(parameterType, receiverType))
            return true;

        var conversion = Compilation.ClassifyConversion(receiverType, parameterType, includeUserDefined: false);
        return conversion.Exists && conversion.IsImplicit;
    }

    private bool IsExtensionStaticCandidateForReceiver(
        Symbol member,
        TypeSymbol receiverType,
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
            var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);
            if (!TryUnifyExtensionReceiver(extensionReceiverType, receiverType, substitutions))
                return false;

            return member switch
            {
                MethodSymbol method => SatisfiesInferredTypeParameterConstraints(method, substitutions),
                PropertySymbol property => SatisfiesInferredTypeParameterConstraints(property.GetMethod ?? property.SetMethod, substitutions),
                _ => false
            };
        }

        if (SymbolEqualityComparer.Default.Equals(extensionReceiverType, receiverType))
            return true;

        var conversion = Compilation.ClassifyConversion(receiverType, extensionReceiverType, includeUserDefined: false);
        return conversion.Exists && conversion.IsImplicit;
    }

    private bool TryUnifyExtensionReceiver(
        TypeSymbol parameterType,
        TypeSymbol argumentType,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
    {
        parameterType = NormalizeTypeForExtensionInference(parameterType);
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (parameterType is TypeParameterSymbol parameter)
            return TryRecordExtensionSubstitution(parameter, argumentType, substitutions);

        if (parameterType is NamedTypeSymbol paramNamed)
        {
            if (argumentType is NamedTypeSymbol argNamed)
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

            if (argumentType is ArrayTypeSymbol arrayArgument)
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
                var plainArgument = argumentType.GetNonNullableType();

                if (TryUnifyNamedType(paramNamed, plainArgument as NamedTypeSymbol, substitutions))
                    return true;

                foreach (var iface in plainArgument.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface, substitutions))
                        return true;
                }

                return false;
            }
        }

        if (parameterType is ArrayTypeSymbol paramArray && argumentType is ArrayTypeSymbol argArray)
            return TryUnifyExtensionReceiver(paramArray.ElementType, argArray.ElementType, substitutions);

        if (Conversion.IsNullable(parameterType))
        {
            var plainParameter = parameterType.GetNonNullableType();

            if (Conversion.IsNullable(argumentType))
                return TryUnifyExtensionReceiver(plainParameter, argumentType.GetNonNullableType(), substitutions);

            if (!argumentType.IsValueType)
                return TryUnifyExtensionReceiver(plainParameter, argumentType, substitutions);

            return false;
        }

        return SymbolEqualityComparer.Default.Equals(parameterType, argumentType);

        bool TryUnifyNamedType(
            NamedTypeSymbol parameterNamed,
            NamedTypeSymbol? argumentNamed,
            Dictionary<TypeParameterSymbol, TypeSymbol> map)
        {
            if (argumentNamed is null)
                return false;

            var parameterDefinition = parameterNamed.OriginalDefinition ?? parameterNamed;
            var argumentDefinition = argumentNamed.OriginalDefinition ?? argumentNamed;

            if (!SymbolEqualityComparer.Default.Equals(parameterDefinition, argumentDefinition))
                return false;

            var parameterArguments = parameterNamed is ConstructedNamedTypeSymbol constructedParameter
                ? constructedParameter.GetExplicitTypeArgumentsForInference()
                : parameterNamed.TypeArguments;

            var argumentArguments = argumentNamed is ConstructedNamedTypeSymbol constructedArgument
                ? constructedArgument.GetExplicitTypeArgumentsForInference()
                : argumentNamed.TypeArguments;

            if (parameterArguments.IsDefault)
                parameterArguments = ImmutableArray<TypeSymbol>.Empty;

            if (argumentArguments.IsDefault)
                argumentArguments = ImmutableArray<TypeSymbol>.Empty;

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
            NamedTypeSymbol parameterNamed,
            ArrayTypeSymbol argumentArray,
            Dictionary<TypeParameterSymbol, TypeSymbol> map)
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
        TypeParameterSymbol typeParameter,
        TypeSymbol argumentType,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
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

    private static TypeSymbol NormalizeTypeForExtensionInference(TypeSymbol type)
    {
        return type switch
        {
            LiteralTypeSymbol literal => literal.UnderlyingType,
            _ => type
        };
    }

    private static bool IsGenericCollectionInterface(NamedTypeSymbol parameterNamed, string interfaceName)
    {
        var definition = parameterNamed.ConstructedFrom ?? parameterNamed;

        if (!string.Equals(definition.Name, interfaceName, StringComparison.Ordinal))
            return false;

        var ns = definition.ContainingNamespace?.ToDisplayString();
        return string.Equals(ns, "System.Collections.Generic", StringComparison.Ordinal);
    }

    private IEnumerable<MethodSymbol> EnumerateExtensionMethods(
        NamespaceOrTypeSymbol scope,
        string? name,
        bool includePartialMatches)
    {
        if (scope is NamespaceSymbol ns)
        {
            Compilation.EnsureSourceExtensionScopeDeclared(ns);
            if (!string.IsNullOrEmpty(name) && !includePartialMatches)
            {
                var extensionContainers = Compilation.SymbolLookup.GetExtensionContainers(
                    ns,
                    name,
                    ExtensionMemberKinds.InstanceMethods);

                foreach (var typeMember in extensionContainers)
                {
                    foreach (var method in EnumerateExtensionMethods(typeMember, name, includePartialMatches))
                        yield return method;
                }

                if (ns is not SourceNamespaceSymbol || Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed)
                    yield break;
            }

            foreach (var member in ns.GetMembers())
            {
                if (member is NamedTypeSymbol typeMember)
                {
                    foreach (var method in EnumerateExtensionMethods(typeMember, name, includePartialMatches))
                        yield return method;
                }
            }

            yield break;
        }

        if (scope is not NamedTypeSymbol type)
            yield break;

        var typeMembers = GetDeclaredMembersForExtensionLookup(type, includePartialMatches ? null : name);
        var members = typeMembers.OfType<MethodSymbol>();

        foreach (var member in members)
        {
            if (!TryIsInstanceExtensionMember(member))
                continue;

            if (!ExtensionMemberNameMatches(member.Name, name, includePartialMatches))
                continue;

            yield return member;
        }

        foreach (var nested in typeMembers.OfType<NamedTypeSymbol>())
        {
            foreach (var method in EnumerateExtensionMethods(nested, name, includePartialMatches))
                yield return method;
        }
    }

    private IEnumerable<MethodSymbol> EnumerateExtensionStaticMethods(
        NamespaceOrTypeSymbol scope,
        string? name,
        bool includePartialMatches)
    {
        if (scope is NamespaceSymbol ns)
        {
            Compilation.EnsureSourceExtensionScopeDeclared(ns);
            if (!string.IsNullOrEmpty(name) && !includePartialMatches)
            {
                foreach (var typeMember in Compilation.SymbolLookup.GetExtensionContainers(
                    ns,
                    name,
                    ExtensionMemberKinds.StaticMethods))
                {
                    foreach (var method in EnumerateExtensionStaticMethods(typeMember, name, includePartialMatches))
                        yield return method;
                }

                if (ns is SourceNamespaceSymbol)
                    yield break;
            }

            foreach (var member in ns.GetMembers())
            {
                if (member is NamedTypeSymbol typeMember)
                {
                    foreach (var method in EnumerateExtensionStaticMethods(typeMember, name, includePartialMatches))
                        yield return method;
                }
            }

            yield break;
        }

        if (scope is not NamedTypeSymbol type)
            yield break;

        if (!type.HasStaticExtensionMembers)
            yield break;

        var typeMembers = GetDeclaredMembersForExtensionLookup(type, includePartialMatches ? null : name);
        var members = typeMembers.OfType<MethodSymbol>();

        foreach (var member in members)
        {
            if (!member.IsStatic || !member.IsStaticExtensionMember)
                continue;

            if (!ExtensionMemberNameMatches(member.Name, name, includePartialMatches))
                continue;

            yield return member;
        }

        foreach (var nested in typeMembers.OfType<NamedTypeSymbol>())
        {
            foreach (var method in EnumerateExtensionStaticMethods(nested, name, includePartialMatches))
                yield return method;
        }
    }

    private IEnumerable<PropertySymbol> EnumerateExtensionProperties(
        NamespaceOrTypeSymbol scope,
        string? name,
        bool includePartialMatches)
    {
        if (scope is NamespaceSymbol ns)
        {
            Compilation.EnsureSourceExtensionScopeDeclared(ns);
            if (!string.IsNullOrEmpty(name) && !includePartialMatches)
            {
                foreach (var typeMember in Compilation.SymbolLookup.GetExtensionContainers(
                    ns,
                    name,
                    ExtensionMemberKinds.InstanceProperties))
                {
                    foreach (var property in EnumerateExtensionProperties(typeMember, name, includePartialMatches))
                        yield return property;
                }

                if (ns is SourceNamespaceSymbol)
                    yield break;
            }

            foreach (var member in ns.GetMembers())
            {
                if (member is NamedTypeSymbol typeMember)
                {
                    foreach (var property in EnumerateExtensionProperties(typeMember, name, includePartialMatches))
                        yield return property;
                }
            }

            yield break;
        }

        if (scope is not NamedTypeSymbol type)
            yield break;

        var typeMembers = GetDeclaredMembersForExtensionLookup(type, includePartialMatches ? null : name);
        var members = typeMembers.OfType<PropertySymbol>();

        foreach (var property in members)
        {
            if (!property.IsExtensionProperty)
                continue;

            if (!ExtensionMemberNameMatches(property.Name, name, includePartialMatches))
                continue;

            yield return property;
        }

        foreach (var nested in typeMembers.OfType<NamedTypeSymbol>())
        {
            foreach (var property in EnumerateExtensionProperties(nested, name, includePartialMatches))
                yield return property;
        }
    }

    private IEnumerable<PropertySymbol> EnumerateExtensionStaticProperties(
        NamespaceOrTypeSymbol scope,
        string? name,
        bool includePartialMatches)
    {
        if (scope is NamespaceSymbol ns)
        {
            Compilation.EnsureSourceExtensionScopeDeclared(ns);
            if (!string.IsNullOrEmpty(name) && !includePartialMatches)
            {
                foreach (var typeMember in Compilation.SymbolLookup.GetExtensionContainers(
                    ns,
                    name,
                    ExtensionMemberKinds.StaticProperties))
                {
                    foreach (var property in EnumerateExtensionStaticProperties(typeMember, name, includePartialMatches))
                        yield return property;
                }

                if (ns is SourceNamespaceSymbol)
                    yield break;
            }

            foreach (var member in ns.GetMembers())
            {
                if (member is NamedTypeSymbol typeMember)
                {
                    foreach (var property in EnumerateExtensionStaticProperties(typeMember, name, includePartialMatches))
                        yield return property;
                }
            }

            yield break;
        }

        if (scope is not NamedTypeSymbol type)
            yield break;

        if (!type.HasStaticExtensionMembers)
            yield break;

        var typeMembers = GetDeclaredMembersForExtensionLookup(type, includePartialMatches ? null : name);
        var members = typeMembers.OfType<PropertySymbol>();

        foreach (var property in members)
        {
            if (property.IsExtensionProperty || !property.IsStatic)
                continue;

            if (property.GetExtensionReceiverType() is null)
                continue;

            if (!ExtensionMemberNameMatches(property.Name, name, includePartialMatches))
                continue;

            yield return property;
        }

        foreach (var nested in typeMembers.OfType<NamedTypeSymbol>())
        {
            foreach (var property in EnumerateExtensionStaticProperties(nested, name, includePartialMatches))
                yield return property;
        }
    }

    private static ImmutableArray<Symbol> GetDeclaredMembersForExtensionLookup(NamedTypeSymbol type, string? name)
    {
        if (type is Symbols.SourceNamedTypeSymbol sourceType)
        {
            var compilation = sourceType.ContainingAssembly is SourceAssemblySymbol sourceAssembly
                ? sourceAssembly.Compilation
                : null;
            if (compilation?.IsSourceNamespaceLookupDeclarationCompletionSuppressed != true)
            {
                return string.IsNullOrEmpty(name)
                    ? sourceType.GetMembers()
                    : sourceType.GetMembers(name!);
            }

            return string.IsNullOrEmpty(name)
                ? sourceType.GetDeclaredMembersWithoutEnsuring()
                : sourceType.GetDeclaredMembersWithoutEnsuring(name!);
        }

        return string.IsNullOrEmpty(name)
            ? type.GetMembers()
            : type.GetMembers(name!);
    }

    private static bool ExtensionMemberNameMatches(string memberName, string? requestedName, bool includePartialMatches)
    {
        if (string.IsNullOrEmpty(requestedName))
            return true;

        return includePartialMatches
            ? memberName.StartsWith(requestedName, StringComparison.Ordinal)
            : string.Equals(memberName, requestedName, StringComparison.Ordinal);
    }

    private bool IsExtensionCandidateForReceiver(
        MethodSymbol method,
        TypeSymbol receiverType,
        bool includePartialMatches)
    {
        if (!TryIsInstanceExtensionMember(method))
            return false;

        if (includePartialMatches)
            return true;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return true;

        if (!TryGetMethodParameterType(method, 0, out var parameterType))
            return false;

        if (parameterType is null || parameterType.TypeKind == TypeKind.Error)
            return true;

        if (ContainsTypeParameters(parameterType))
        {
            var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);
            return TryUnifyExtensionReceiver(parameterType, receiverType, substitutions) &&
                   SatisfiesInferredTypeParameterConstraints(method, substitutions);
        }

        if (SymbolEqualityComparer.Default.Equals(parameterType, receiverType))
            return true;

        var conversion = Compilation.ClassifyConversion(receiverType, parameterType, includeUserDefined: false);
        return conversion.Exists && conversion.IsImplicit;
    }

    private static bool TryIsInstanceExtensionMember(MethodSymbol method)
    {
        try
        {
            if (method.IsExtensionMethod)
                return true;

            var receiverType = method.GetExtensionReceiverType();
            if (receiverType is null)
                return false;

            return TryGetMethodParameterType(method, 0, out var parameterType) &&
                   SymbolEqualityComparer.Default.Equals(receiverType, parameterType);
        }
        catch (InvalidOperationException)
        {
            return false;
        }
    }

    private static bool TryGetMethodParameterType(
        MethodSymbol method,
        int index,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out TypeSymbol? type)
    {
        if (method is PEMethodSymbol peMethod)
            return peMethod.TryGetParameterType(index, out type);

        if (index < 0 || index >= method.Parameters.Length)
        {
            type = null;
            return false;
        }

        type = method.Parameters[index].Type;
        return type is not null;
    }

    private bool SatisfiesInferredTypeParameterConstraints(
        MethodSymbol? method,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
    {
        if (method is null || substitutions.Count == 0)
            return true;

        var methodDefinition = method.OriginalDefinition ?? method;

        if (methodDefinition.ContainingType is NamedTypeSymbol containingType &&
            !SatisfiesInferredTypeParameterConstraints(containingType.TypeParameters, substitutions))
        {
            return false;
        }

        return SatisfiesInferredTypeParameterConstraints(methodDefinition.TypeParameters, substitutions);
    }

    private bool SatisfiesInferredTypeParameterConstraints(
        ImmutableArray<TypeParameterSymbol> typeParameters,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
    {
        if (typeParameters.IsDefaultOrEmpty || substitutions.Count == 0)
            return true;

        EnsureTypeParameterConstraintTypesResolved(typeParameters);

        foreach (var typeParameter in typeParameters)
        {
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                continue;

            var constraintKind = typeParameter.ConstraintKind;

            if (SemanticFacts.MayBeRefLike(typeArgument) &&
                (constraintKind & TypeParameterConstraintKind.AllowByRefLike) == 0)
            {
                return false;
            }

            if (constraintKind.HasFlag(TypeParameterConstraintKind.FunctionType) && !SemanticFacts.SatisfiesFunctionTypeConstraint(typeArgument))
                return false;

            if ((constraintKind & TypeParameterConstraintKind.ReferenceType) != 0 &&
                !SemanticFacts.SatisfiesReferenceTypeConstraint(typeArgument))
            {
                return false;
            }

            if ((constraintKind & TypeParameterConstraintKind.ValueType) != 0 &&
                !SemanticFacts.SatisfiesValueTypeConstraint(typeArgument))
            {
                return false;
            }

            if ((constraintKind & TypeParameterConstraintKind.NotNull) != 0 &&
                !SemanticFacts.SatisfiesNotNullConstraint(typeArgument))
            {
                return false;
            }

            if ((constraintKind & TypeParameterConstraintKind.Constructor) != 0 &&
                !SemanticFacts.SatisfiesConstructorConstraint(typeArgument))
            {
                return false;
            }

            if ((constraintKind & TypeParameterConstraintKind.TypeConstraint) == 0)
                continue;

            foreach (var constraintType in typeParameter.ConstraintTypes)
            {
                var substitutedConstraint = SubstituteConstraintType(constraintType, substitutions);

                if (substitutedConstraint is TypeParameterSymbol unsubstituted &&
                    !substitutions.ContainsKey(unsubstituted))
                {
                    continue;
                }

                if (substitutedConstraint is ErrorTypeSymbol)
                    continue;

                if (substitutedConstraint is NamedTypeSymbol namedConstraint)
                {
                    if (!RuntimeSelfTypes.SatisfiesConstraint(Compilation, typeArgument, namedConstraint))
                        return false;

                    continue;
                }

                if (!RuntimeSelfTypes.SatisfiesConstraint(Compilation, typeArgument, substitutedConstraint))
                    return false;
            }
        }

        return true;
    }

    private static bool ContainsTypeParameters(TypeSymbol type)
        => ContainsTypeParameters(type, new HashSet<TypeSymbol>(ReferenceEqualityComparer.Instance));

    private static bool ContainsTypeParameters(TypeSymbol type, HashSet<TypeSymbol> visited)
    {
        if (!visited.Add(type))
            return false;

        switch (type)
        {
            case TypeParameterSymbol:
                return true;
            case ConstructedNamedTypeSymbol constructed:
                foreach (var argument in constructed.GetExplicitTypeArgumentsForInference())
                {
                    if (ContainsTypeParameters(argument, visited))
                        return true;
                }

                return false;
            case NamedTypeSymbol named when !named.TypeArguments.IsDefaultOrEmpty:
                foreach (var argument in named.TypeArguments)
                {
                    if (ContainsTypeParameters(argument, visited))
                        return true;
                }

                return false;
            case ArrayTypeSymbol array:
                return ContainsTypeParameters(array.ElementType, visited);
            case NullableTypeSymbol nullable:
                return ContainsTypeParameters(nullable.UnderlyingType, visited);
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

    internal TypeSymbol BindTypeSyntaxAndReport(
        TypeSyntax typeSyntax,
        TypeResolutionOptions? options = null,
        RefKind? refKindHint = null,
        DiagnosticBag? additionalDiagnostics = null)
    {
        var result = BindTypeSyntax(typeSyntax, options);
        if (result.Success)
            return ApplyBoundTypeContract(result.ResolvedType, typeSyntax.GetLocation(), refKindHint);

        foreach (var diagnostic in MapResolveResultToDiagnostics(result, typeSyntax))
        {
            _diagnostics.Report(diagnostic);
            if (additionalDiagnostics is not null && !ReferenceEquals(additionalDiagnostics, _diagnostics))
                additionalDiagnostics.Report(diagnostic);
        }

        return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
    }

    private TypeSymbol ApplyBoundTypeContract(TypeSymbol type, Location location, RefKind? refKindHint)
    {
        if (type is NamedTypeSymbol or TypeParameterSymbol)
            return ApplyAccessibilityAndRefKind(type, location, refKindHint);

        return ApplyRefKindHint(type, refKindHint);
    }

    internal void ReportResolveTypeResultDiagnostics(ResolveTypeResult result, TypeSyntax? rootSyntax = null)
    {
        foreach (var diagnostic in MapResolveResultToDiagnostics(result, rootSyntax))
            _diagnostics.Report(diagnostic);
    }

    internal IEnumerable<Diagnostic> MapResolveResultToDiagnostics(ResolveTypeResult result, TypeSyntax? rootSyntax = null)
    {
        var fallbackSyntax = rootSyntax ?? (result.Issues.IsDefaultOrEmpty ? null : result.Issues[0].Syntax);
        var fallbackLocation = fallbackSyntax?.GetLocation() ?? Location.None;
        var fallbackName = fallbackSyntax?.ToString() ?? "type";

        if (result.IsAmbiguous)
        {
            var candidates = result.AmbiguousCandidates;
            var left = candidates.Length > 0 ? candidates[0].ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) : fallbackName;
            var right = candidates.Length > 1 ? candidates[1].ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) : left;
            yield return Diagnostic.Create(CompilerDiagnostics.CallIsAmbiguous, fallbackLocation, left, right);
            yield break;
        }

        var issues = result.Issues;

        if (issues.IsDefaultOrEmpty)
        {
            yield return Diagnostic.Create(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, fallbackLocation, fallbackName);
            yield break;
        }

        var primaryIssue = SelectPrimaryResolutionIssue(issues);
        if (primaryIssue is null)
        {
            yield return Diagnostic.Create(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, fallbackLocation, fallbackName);
            yield break;
        }

        var issueSyntax = primaryIssue.Syntax ?? fallbackSyntax;
        var location = issueSyntax?.GetLocation() ?? fallbackLocation;
        switch (primaryIssue.Kind)
        {
            case TypeResolutionFailureKind.IntersectionTypeNotSupported:
                yield return Diagnostic.Create(CompilerDiagnostics.IntersectionTypeNotSupported, location);
                break;
            case TypeResolutionFailureKind.ArityMismatch:
                {
                    var typeName = issueSyntax?.ToString() ?? fallbackName;
                    var expectedArity = result.ResolvedNamedDefinition?.TypeParameters.Length;
                    if (expectedArity is int arity)
                        yield return Diagnostic.Create(CompilerDiagnostics.TypeRequiresTypeArguments, location, typeName, arity);
                    else
                        yield return Diagnostic.Create(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, location, typeName);
                    break;
                }
            case TypeResolutionFailureKind.Ambiguous:
                {
                    var amb = primaryIssue.Candidates;
                    var first = amb.Length > 0 ? amb[0].ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) : issueSyntax?.ToString() ?? fallbackName;
                    var second = amb.Length > 1 ? amb[1].ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) : first;
                    yield return Diagnostic.Create(CompilerDiagnostics.CallIsAmbiguous, location, first, second);
                    break;
                }
            case TypeResolutionFailureKind.NullableValueTypesNotAllowed:
                yield return Diagnostic.Create(CompilerDiagnostics.NullableValueTypesNotAllowed, location);
                break;
            case TypeResolutionFailureKind.PointerTypeRequiresUnsafe:
                yield return Diagnostic.Create(CompilerDiagnostics.PointerTypeRequiresUnsafe, location);
                break;
            default:
                yield return Diagnostic.Create(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, location, issueSyntax?.ToString() ?? fallbackName);
                break;
        }
    }

    private static ResolveTypeResult.ResolutionIssue? SelectPrimaryResolutionIssue(
        ImmutableArray<ResolveTypeResult.ResolutionIssue> issues)
    {
        if (issues.IsDefaultOrEmpty)
            return null;

        foreach (var issue in issues)
        {
            if (!IsWrapperResolutionIssue(issue.Kind))
                return issue;
        }

        return issues[0];
    }

    private static bool IsWrapperResolutionIssue(TypeResolutionFailureKind kind)
    {
        return kind is TypeResolutionFailureKind.TypeArgumentFailed
            or TypeResolutionFailureKind.TupleElementFailed
            or TypeResolutionFailureKind.FunctionParameterFailed
            or TypeResolutionFailureKind.FunctionReturnFailed
            or TypeResolutionFailureKind.ArrayElementFailed
            or TypeResolutionFailureKind.ByRefElementFailed
            or TypeResolutionFailureKind.NullableUnderlyingFailed;
    }


    private static TypeSymbol ApplyRefKindHint(TypeSymbol type, RefKind? refKindHint)
    {
        if (refKindHint is not RefKind.Ref and not RefKind.Out and not RefKind.In and not RefKind.RefReadOnly and not RefKind.RefReadOnlyParameter)
            return type;

        if (type is RefTypeSymbol existing)
            return existing;

        return new RefTypeSymbol(type);
    }

    private TypeSymbol ApplyAccessibilityAndRefKind(
        TypeSymbol type,
        Location location,
        RefKind? refKindHint)
    {
        if (IsSymbolAccessible(type))
            return ApplyRefKindHint(type, refKindHint);

        return ReportInaccessibleType(type, location, refKindHint);
    }

    private TypeSymbol ReportInaccessibleType(
        TypeSymbol type,
        Location location,
        RefKind? refKindHint)
    {
        var display = type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        _diagnostics.ReportSymbolIsInaccessible("type", display, location);
        return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
    }

    internal TypeSymbol EnsureTypeValidForStorageLocation(TypeSymbol type, Location location)
    {
        if (type.TypeKind == TypeKind.Error)
            return type;

        if (!ValidateStorageTypeArgumentConstraints(type, location))
            return Compilation.ErrorTypeSymbol;

        if (TryFindRefLikeArrayElement(type, out var elementType))
        {
            var elementDisplay = elementType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            _diagnostics.ReportRefLikeTypeCannotBeUsedAsArrayElement(elementDisplay, location);
            return Compilation.ErrorTypeSymbol;
        }

        if (!TryFindStaticStorageType(type, out var staticType))
            return type;

        var display = staticType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        _diagnostics.ReportStaticTypeCannotBeUsedAsStorageType(display, location);
        return Compilation.ErrorTypeSymbol;
    }

    private bool ValidateStorageTypeArgumentConstraints(TypeSymbol type, Location location)
    {
        switch (type)
        {
            case ArrayTypeSymbol array:
                return ValidateStorageTypeArgumentConstraints(array.ElementType, location);
            case NullableTypeSymbol nullable:
                return ValidateStorageTypeArgumentConstraints(nullable.UnderlyingType, location);
            case RefTypeSymbol reference:
                return ValidateStorageTypeArgumentConstraints(reference.ElementType, location);
            case NamedTypeSymbol named when !named.IsUnboundGenericType:
                var valid = true;
                if (named.ContainingType is { } containingType)
                    valid &= ValidateStorageTypeArgumentConstraints(containingType, location);

                foreach (var argument in named.TypeArguments)
                    valid &= ValidateStorageTypeArgumentConstraints(argument, location);

                if (!named.TypeArguments.IsDefaultOrEmpty)
                    valid &= ValidateTypeArgumentConstraints(named, named.TypeArguments, _ => location);

                return valid;
            default:
                return true;
        }
    }

    internal TypeSymbol EnsureTypeValidForField(TypeSymbol type, Location location)
    {
        type = EnsureTypeValidForStorageLocation(type, location);
        if (type.TypeKind == TypeKind.Error)
            return type;

        var containingType = ContainingSymbol as NamedTypeSymbol ?? ContainingSymbol?.ContainingType;
        if (SemanticFacts.MayBeRefLike(type) &&
            containingType is not { IsRefLikeType: true })
        {
            var display = type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            var containingDisplay = containingType?.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<global>";
            _diagnostics.ReportRefLikeTypeCannotBeUsedAsField(display, containingDisplay, location);
            return Compilation.ErrorTypeSymbol;
        }

        return type;
    }

    private static bool TryFindRefLikeArrayElement(TypeSymbol type, out TypeSymbol refLikeType)
    {
        if (type is ArrayTypeSymbol arrayType)
        {
            if (SemanticFacts.MayBeRefLike(arrayType.ElementType))
            {
                refLikeType = arrayType.ElementType;
                return true;
            }

            return TryFindRefLikeArrayElement(arrayType.ElementType, out refLikeType);
        }

        refLikeType = null!;
        return false;
    }


    private static bool TryFindStaticStorageType(TypeSymbol type, out NamedTypeSymbol staticType)
    {
        switch (type)
        {
            case NamedTypeSymbol namedType:
                if (namedType.IsStatic)
                {
                    staticType = namedType;
                    return true;
                }

                foreach (var typeArgument in namedType.TypeArguments)
                {
                    if (TryFindStaticStorageType(typeArgument, out staticType))
                        return true;
                }

                break;

            case ArrayTypeSymbol arrayType:
                return TryFindStaticStorageType(arrayType.ElementType, out staticType);

            case NullableTypeSymbol nullableType:
                return TryFindStaticStorageType(nullableType.UnderlyingType, out staticType);

            case RefTypeSymbol refType:
                return TryFindStaticStorageType(refType.ElementType, out staticType);

            case AddressTypeSymbolImplementation addressType:
                return TryFindStaticStorageType(addressType.ReferencedType, out staticType);
        }

        staticType = null!;
        return false;
    }

    protected static bool RequiresExplicitTypeArguments(NamedTypeSymbol named)
        => named.Arity > 0 &&
           ReferenceEquals(named, NormalizeDefinition(named)) &&
           HasImplicitDefinitionTypeArguments(named);

    private static bool HasImplicitDefinitionTypeArguments(NamedTypeSymbol named)
    {
        if (named.TypeArguments.IsDefaultOrEmpty)
            return true;

        if (named.TypeArguments.Length != named.TypeParameters.Length)
            return false;

        for (var i = 0; i < named.TypeParameters.Length; i++)
        {
            if (!SymbolEqualityComparer.Default.Equals(named.TypeArguments[i], named.TypeParameters[i]))
                return false;
        }

        return true;
    }


    public virtual IEnumerable<Symbol> LookupAvailableSymbols()
    {
        // Declaration contexts (for example a parameter type) have no block
        // binder, but still have the same imported namespace and type scopes.
        for (var current = this; current is not null; current = current.ParentBinder)
        {
            if (current is not ImportBinder imports)
                continue;

            foreach (var type in imports.GetImportedTypes())
                yield return type;

            foreach (var scope in imports.GetImportedNamespacesOrTypeScopes())
            {
                foreach (var member in scope.GetMembers().OfType<NamespaceOrTypeSymbol>())
                    yield return member;
            }
        }

        if (CurrentNamespace is { IsGlobalNamespace: false } currentNamespace)
        {
            foreach (var member in currentNamespace.GetMembers().OfType<NamespaceOrTypeSymbol>())
                yield return member;
        }

        foreach (var member in Compilation.SymbolLookup.GetGlobalMembers())
            yield return member;
    }

    public Symbol? LookupLocalSymbol(string name)
    {
        return LookupSymbol(name);
    }

    public virtual BoundFunctionStatement BindFunction(FunctionStatementSyntax function)
    {
        return ParentBinder?.BindFunction(function)
             ?? throw new InvalidOperationException("Function statements require an enclosing executable block binder.");
    }

    protected BoundNode? TryGetCachedBoundNode(SyntaxNode node)
        => SemanticModel?.TryGetCachedBoundNode(node);

    protected BoundNode? TryGetCachedBoundNode(SyntaxNode node, TypeSymbol? targetType)
        => SemanticModel?.TryGetCachedBoundNode(node, targetType);

    protected void CacheBoundNode(SyntaxNode node, BoundNode bound)
    {
        SemanticModel?.CacheBoundNode(node, bound, this);
    }

    protected void CacheBoundNode(SyntaxNode node, BoundNode bound, TypeSymbol? targetType)
    {
        SemanticModel?.CacheBoundNode(node, bound, this, targetType);
    }

    protected void RemoveCachedBoundNode(SyntaxNode node)
    {
        SemanticModel?.RemoveCachedBoundNode(node);
    }

    protected void RemoveCachedBinder(SyntaxNode node)
    {
        SemanticModel?.RemoveCachedBinder(node);
    }

    protected void CacheBinder(SyntaxNode node)
    {
        SemanticModel?.CacheBinderForNode(node, this);
    }

    public virtual BoundNode GetOrBind(SyntaxNode node)
    {
        SemanticModel?.Compilation.PerformanceInstrumentation.BinderReentry.RecordInvocation(node);

        if (TryGetCachedBoundNode(node) is BoundNode cached)
        {
            SemanticModel?.Compilation.PerformanceInstrumentation.BinderReentry.RecordCacheHit(node);
            return cached;
        }

        if (node is FunctionExpressionSyntax lambdaSyntax &&
            lambdaSyntax.Parent is ArgumentSyntax argument &&
            argument.Parent is ArgumentListSyntax argumentList &&
            argumentList.Parent is InvocationExpressionSyntax invocation)
        {
            _ = BindExpression(invocation);

            if (TryGetCachedBoundNode(node) is BoundNode rebound)
            {
                SemanticModel?.Compilation.PerformanceInstrumentation.BinderReentry.RecordCacheHit(node);
                return rebound;
            }
        }

        SemanticModel?.Compilation.PerformanceInstrumentation.BinderReentry.RecordBindExecution(node);

        BoundNode result = node switch
        {
            ExpressionSyntax expr => BindExpression(expr),
            StatementSyntax stmt => BindStatement(stmt),
            ArrowExpressionClauseSyntax arrow => BindArrowExpressionClause(arrow),

            _ => throw new NotSupportedException($"Unsupported node kind: {node.Kind}")
        };

        CacheBoundNode(node, result);
        return result;
    }

    public virtual BoundNode GetOrBindForSemanticQuery(SyntaxNode node)
    {
        using var _ = _diagnostics.CreateNonReportingScope();
        return GetOrBind(node);
    }

    protected virtual BoundNode BindArrowExpressionClause(ArrowExpressionClauseSyntax clause)
    {
        var expression = BindExpression(clause.Expression);
        return expression is BoundBlockExpression blockExpression
            ? new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose)
            : new BoundBlockStatement([new BoundExpressionStatement(expression)]);
    }

    private static int ComputeGenericArity(GenericNameSyntax generic)
    {
        var argumentCount = generic.TypeArgumentList.Arguments.Count;
        var separators = generic.TypeArgumentList.Arguments.SeparatorCount + 1;

        if (argumentCount == 0)
            return Math.Max(1, separators);

        return Math.Max(argumentCount, separators);
    }

    protected NamedTypeSymbol? FindAccessibleNamedType(string name, int arity)
    {
        return TypeLookupUtilities.SelectBestNamedTypeByArity(LookupNamedTypeCandidates(name), arity);
    }

    protected static NamedTypeSymbol NormalizeDefinition(NamedTypeSymbol named)
        => named.ConstructedFrom as NamedTypeSymbol ?? named;

    protected TypeSymbol? TryConstructGeneric(NamedTypeSymbol definition, ImmutableArray<TypeSymbol> typeArguments, int arity)
    {
        if (typeArguments.Length != arity)
            return null;

        if (typeArguments.Any(t => t == Compilation.ErrorTypeSymbol))
            return Compilation.ErrorTypeSymbol;

        return Compilation.ConstructGenericType(definition, typeArguments.ToArray());
    }

    protected bool ValidateTypeArgumentConstraints(
        NamedTypeSymbol definition,
        ImmutableArray<TypeSymbol> typeArguments,
        Func<int, Location> getArgumentLocation,
        string? genericDisplayName = null)
    {
        var typeParameters = definition.TypeParameters;
        if (typeParameters.Length != typeArguments.Length)
            return true;

        bool allSatisfied = true;
        var displayName = genericDisplayName ?? definition.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);

        for (int i = 0; i < typeParameters.Length; i++)
            substitutions[typeParameters[i]] = typeArguments[i];

        EnsureTypeParameterConstraintTypesResolved(typeParameters);

        for (int i = 0; i < typeParameters.Length; i++)
        {
            var typeParameter = typeParameters[i];
            var typeArgument = typeArguments[i];
            var constraintKind = typeParameter.ConstraintKind;

            if (SemanticFacts.MayBeRefLike(typeArgument) &&
                (constraintKind & TypeParameterConstraintKind.AllowByRefLike) == 0)
            {
                ReportConstraintViolation(typeArgument, "non-ref-like", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if (constraintKind.HasFlag(TypeParameterConstraintKind.FunctionType) && !SemanticFacts.SatisfiesFunctionTypeConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "func", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

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
                    var substitutedConstraint = SubstituteConstraintType(constraintType, substitutions);

                    if (substitutedConstraint is TypeParameterSymbol unsubstituted &&
                        !substitutions.ContainsKey(unsubstituted))
                    {
                        continue;
                    }

                    if (substitutedConstraint is ErrorTypeSymbol)
                        continue;

                    if (substitutedConstraint is NamedTypeSymbol namedConstraint)
                    {
                        if (RuntimeSelfTypes.SatisfiesConstraint(Compilation, typeArgument, namedConstraint))
                            continue;

                        var constraintDisplay = namedConstraint.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                        ReportConstraintViolation(typeArgument, constraintDisplay, typeParameter, displayName, getArgumentLocation(i));
                        allSatisfied = false;
                        continue;
                    }

                    if (RuntimeSelfTypes.SatisfiesConstraint(Compilation, typeArgument, substitutedConstraint))
                        continue;

                    var display = substitutedConstraint.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    ReportConstraintViolation(typeArgument, display, typeParameter, displayName, getArgumentLocation(i));
                    allSatisfied = false;
                }
            }
        }

        return allSatisfied;
    }

    internal void EnsureTypeParameterConstraintTypesResolved(ImmutableArray<TypeParameterSymbol> typeParameters)
    {
        foreach (var parameter in typeParameters)
            EnsureTypeParameterConstraintTypesResolved(parameter);
    }

    private void EnsureTypeParameterConstraintTypesResolved(TypeParameterSymbol typeParameter)
    {
        if (typeParameter is not SourceTypeParameterSymbol source)
            return;

        if (source.HasResolvedConstraintTypes)
        {
            if (!ReferenceEquals(source.ContainingSymbol, ContainingSymbol))
                return;

            // Resolution may have happened in a non-reporting query or an earlier
            // declaration binder. The active binder must own failures as well.
            for (var i = 0; i < source.ConstraintTypes.Length; i++)
            {
                if (source.ConstraintTypes[i].TypeKind == TypeKind.Error &&
                    source.ConstraintTypeReferences[i].GetSyntax() is TypeSyntax failedConstraint)
                    _ = BindTypeSyntaxAndReport(failedConstraint);
            }
            return;
        }

        if (source.ConstraintTypeReferences.IsDefaultOrEmpty)
        {
            source.SetConstraintTypes(ImmutableArray<TypeSymbol>.Empty);
            return;
        }

        // Constraint type parameters belong to the declaration being checked,
        // not to a same-named parameter in the binder that first uses that type.
        var declaredParameters = new Dictionary<string, TypeSymbol>(StringComparer.Ordinal);
        for (Symbol? owner = source.ContainingSymbol; owner is not null; owner = owner.ContainingSymbol)
        {
            var parameters = owner switch
            {
                MethodSymbol method => method.TypeParameters,
                NamedTypeSymbol type => type.TypeParameters,
                _ => ImmutableArray<TypeParameterSymbol>.Empty
            };
            foreach (var parameter in parameters)
                declaredParameters.TryAdd(parameter.Name, parameter);
        }
        var options = new TypeResolutionOptions { TypeParameterSubstitutions = declaredParameters };
        var builder = ImmutableArray.CreateBuilder<TypeSymbol>(source.ConstraintTypeReferences.Length);
        foreach (var reference in source.ConstraintTypeReferences)
        {
            if (reference.GetSyntax() is TypeSyntax constraintType)
            {
                var resolved = BindTypeSyntaxAndReport(constraintType, options);
                builder.Add(resolved);
            }
        }

        source.SetConstraintTypes(builder.MoveToImmutable());
    }

    private void ReportConstraintViolation(TypeSymbol typeArgument, string constraintDisplay, TypeParameterSymbol typeParameter, string genericDisplayName, Location location)
    {
        var argumentDisplay = typeArgument.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat);
        _diagnostics.ReportTypeArgumentDoesNotSatisfyConstraint(argumentDisplay, constraintDisplay, typeParameter.Name, genericDisplayName, location);
    }

    private void ReportNotNullConstraintViolation(TypeParameterSymbol typeParameter, Location location)
    {
        var argumentDisplay = $"{typeParameter.Name}?";
        var genericDisplayName = typeParameter.ContainingSymbol?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)
            ?? typeParameter.Name;
        _diagnostics.ReportTypeArgumentDoesNotSatisfyConstraint(argumentDisplay, "notnull", typeParameter.Name, genericDisplayName, location);
    }

    protected bool ReportTypeArgumentConstraintFailureIfPresent(OverloadResolutionResult resolution, Location location)
    {
        if (resolution.ConstraintFailure is not { } failure)
            return false;

        _diagnostics.ReportTypeArgumentDoesNotSatisfyConstraint(
            failure.TypeArgument,
            failure.Constraint,
            failure.TypeParameter,
            failure.GenericName,
            location);

        return true;
    }

    protected static Location GetTypeArgumentLocation<TNode>(SeparatedSyntaxList<TNode> arguments, Location fallback, int index)
        where TNode : SyntaxNode
    {
        if (index >= 0 && index < arguments.Count)
            return arguments[index].GetLocation();

        return fallback;
    }

    protected bool ValidateMethodTypeArgumentConstraints(
        MethodSymbol method,
        ImmutableArray<TypeSymbol> typeArguments,
        Func<int, Location> getArgumentLocation)
    {
        var typeParameters = method.TypeParameters;
        if (typeParameters.Length != typeArguments.Length)
            return true;

        bool allSatisfied = true;
        var displayName = method.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);

        for (int i = 0; i < typeParameters.Length; i++)
            substitutions[typeParameters[i]] = typeArguments[i];

        for (int i = 0; i < typeParameters.Length; i++)
        {
            var typeParameter = typeParameters[i];
            var typeArgument = typeArguments[i];
            var constraintKind = typeParameter.ConstraintKind;

            if (SemanticFacts.MayBeRefLike(typeArgument) &&
                (constraintKind & TypeParameterConstraintKind.AllowByRefLike) == 0)
            {
                ReportConstraintViolation(typeArgument, "non-ref-like", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

            if (constraintKind.HasFlag(TypeParameterConstraintKind.FunctionType) && !SemanticFacts.SatisfiesFunctionTypeConstraint(typeArgument))
            {
                ReportConstraintViolation(typeArgument, "func", typeParameter, displayName, getArgumentLocation(i));
                allSatisfied = false;
            }

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
                    var substitutedConstraint = SubstituteConstraintType(constraintType, substitutions);
                    if (substitutedConstraint is ErrorTypeSymbol)
                        continue;

                    if (substitutedConstraint is NamedTypeSymbol namedConstraint)
                    {
                        if (RuntimeSelfTypes.SatisfiesConstraint(Compilation, typeArgument, namedConstraint))
                            continue;

                        var constraintDisplay = namedConstraint.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                        ReportConstraintViolation(typeArgument, constraintDisplay, typeParameter, displayName, getArgumentLocation(i));
                        allSatisfied = false;
                        continue;
                    }

                    if (RuntimeSelfTypes.SatisfiesConstraint(Compilation, typeArgument, substitutedConstraint))
                        continue;

                    var display = substitutedConstraint.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    ReportConstraintViolation(typeArgument, display, typeParameter, displayName, getArgumentLocation(i));
                    allSatisfied = false;
                }
            }
        }

        return allSatisfied;
    }

    private static TypeSymbol SubstituteConstraintType(
        TypeSymbol type,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
    {
        switch (type)
        {
            case FunctionReturnTypeSymbolImplementation projection:
                return projection.Substitute(t => SubstituteConstraintType(t, substitutions));

            case StructuralFunctionTypeSymbol function:
                return function.Substitute(t => SubstituteConstraintType(t, substitutions));

            case StructuralUnionTypeSymbol union:
                return TypeSubstitution.SubstituteStructuralUnion(union, t => SubstituteConstraintType(t, substitutions));
            case IntersectionTypeSymbol intersection:
                return TypeSubstitution.SubstituteIntersection(intersection, t => SubstituteConstraintType(t, substitutions));
            case TypeParameterSymbol typeParameter:
                return substitutions.TryGetValue(typeParameter, out var substitutedType)
                    ? substitutedType
                    : type;

            case NamedTypeSymbol named when !named.TypeArguments.IsDefaultOrEmpty:
                {
                    var typeArguments = named.TypeArguments;
                    var rewritten = new TypeSymbol[typeArguments.Length];
                    var changed = false;

                    for (int i = 0; i < typeArguments.Length; i++)
                    {
                        var substitutedArg = SubstituteConstraintType(typeArguments[i], substitutions);
                        rewritten[i] = substitutedArg;
                        changed |= !ReferenceEquals(substitutedArg, typeArguments[i]);
                    }

                    if (!changed)
                        return type;

                    var definition = named.ConstructedFrom as NamedTypeSymbol ?? named;
                    if (definition.Arity == rewritten.Length)
                        return definition.Construct(rewritten);

                    return type;
                }

            case NullableTypeSymbol nullable:
                {
                    var substitutedUnderlying = SubstituteConstraintType(nullable.UnderlyingType, substitutions);
                    if (ReferenceEquals(substitutedUnderlying, nullable.UnderlyingType))
                        return type;

                    return new NullableTypeSymbol(
                        substitutedUnderlying,
                        nullable.ContainingSymbol,
                        nullable.ContainingType,
                        nullable.ContainingNamespace,
                        nullable.Locations.ToArray(),
                        nullable.AbiProjection);
                }
        }

        return type;
    }

    private static NamedTypeSymbol? SelectByArity(IEnumerable<NamedTypeSymbol> candidates, int arity)
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

    private NamedTypeSymbol? FindNamedTypeForGeneric(GenericNameSyntax generic, int arity)
    {
        return FindAccessibleNamedType(generic.Identifier.ValueText, arity);
    }

    protected bool IsValidAsyncReturnType(TypeSymbol? type)
        => AsyncReturnTypeUtilities.IsValidAsyncReturnType(type) ||
            AsyncReturnTypeUtilities.TryGetRuntimeIterator(Compilation, type, out _, out _);
}
