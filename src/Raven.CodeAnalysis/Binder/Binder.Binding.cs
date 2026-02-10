using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal abstract partial class Binder
{
    // ---------------------------------
    // Helpers: interpret type/name syntax as expression access and vice versa
    // ---------------------------------

    /// <summary>
    /// Attempts to interpret a <see cref="TypeSyntax"/> (typically a QualifiedName) as an expression chain,
    /// producing either a namespace expression, a type expression, a member access expression, or a method group.
    /// Useful when the parser produces type-like syntax in expression position (e.g. System.Console.WriteLine).
    /// </summary>
    public bool TryBindTypeSyntaxAsMemberAccessExpression(TypeSyntax syntax, out BoundExpression expression)
    {
        expression = null!;

        // We only support identifier/qualified/generic names here.
        if (syntax is not IdentifierNameSyntax &&
            syntax is not QualifiedNameSyntax &&
            syntax is not GenericNameSyntax)
        {
            return false;
        }

        var parts = FlattenTypeLikeName(syntax);
        if (parts.Length == 0)
            return false;

        // Bind first segment as either namespace or type.
        BoundExpression? current;

        var first = parts[0];
        var ns = LookupNamespace(first);
        if (ns is not null)
        {
            current = new BoundNamespaceExpression(ns);
        }
        else
        {
            var t = LookupType(first);
            if (t is null)
                return false;

            current = new BoundTypeExpression(t);
        }

        // Walk remaining segments.
        for (int i = 1; i < parts.Length; i++)
        {
            var name = parts[i];

            if (current is BoundNamespaceExpression nsReceiver)
            {
                // Prefer nested namespace.
                var nextNs = nsReceiver.Namespace.LookupNamespace(name);
                if (nextNs is not null)
                {
                    current = new BoundNamespaceExpression(nextNs);
                    continue;
                }

                // Then a type in that namespace.
                var typeInNs = nsReceiver.Namespace
                    .GetMembers(name)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault();

                if (typeInNs is null)
                    return false;

                current = new BoundTypeExpression(typeInNs);
                continue;
            }

            if (current is BoundTypeExpression typeReceiver)
            {
                // Prefer nested type.
                var nestedCandidates = typeReceiver.Type.GetTypeMembers(name);
                if (nestedCandidates.Length == 1)
                {
                    current = new BoundTypeExpression(nestedCandidates[0]);
                    continue;
                }

                if (nestedCandidates.Length > 1)
                    return false;

                // Static members.
                var members = typeReceiver.Type.GetMembers(name);

                // Methods -> method group.
                var methodCandidates = members
                    .OfType<IMethodSymbol>()
                    .Where(m => m.IsStatic)
                    .ToImmutableArray();

                if (!methodCandidates.IsDefaultOrEmpty)
                {
                    current = BindMethodGroup(typeReceiver, methodCandidates, syntax.GetLocation());
                    continue;
                }

                // Fields/properties/events -> single member access.
                ISymbol? selected = null;

                var field = members.OfType<IFieldSymbol>().FirstOrDefault(f => f.IsStatic);
                if (field is not null)
                    selected = field;

                var prop = members.OfType<IPropertySymbol>().FirstOrDefault(p => p.IsStatic);
                if (prop is not null)
                {
                    if (selected is not null)
                        return false;
                    selected = prop;
                }

                var evt = members.OfType<IEventSymbol>().FirstOrDefault(e => e.IsStatic);
                if (evt is not null)
                {
                    if (selected is not null)
                        return false;
                    selected = evt;
                }

                if (selected is null)
                    return false;

                current = new BoundMemberAccessExpression(typeReceiver, selected);
                continue;
            }

            // If we got here, we can't continue the chain.
            return false;
        }

        expression = current!;
        return expression is not null;

        static string[] FlattenTypeLikeName(TypeSyntax node)
        {
            // Supports IdentifierName, GenericName, QualifiedName.
            // For GenericName we only keep the identifier portion; type args are not represented
            // in the resulting access chain.
            var parts = new List<string>();

            void Walk(TypeSyntax n)
            {
                switch (n)
                {
                    case IdentifierNameSyntax id:
                        parts.Add(id.Identifier.ValueText);
                        break;

                    case GenericNameSyntax g:
                        parts.Add(g.Identifier.ValueText);
                        break;

                    case QualifiedNameSyntax q:
                        Walk(q.Left);
                        Walk((TypeSyntax)q.Right);
                        break;
                }
            }

            Walk(node);
            return parts.ToArray();
        }
    }

    private BoundExpression BindMethodGroup(BoundExpression? receiver, ImmutableArray<IMethodSymbol> methods, Location location)
    {
        var accessibleMethods = GetAccessibleMethods(methods, location);

        if (accessibleMethods.IsDefaultOrEmpty)
            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

        return CreateMethodGroup(receiver, accessibleMethods);
    }

    private BoundMethodGroupExpression CreateMethodGroup(BoundExpression? receiver, ImmutableArray<IMethodSymbol> methods)
    {
        Func<ITypeSymbol?>? delegateFactory = null;

        if (!methods.IsDefaultOrEmpty && methods.Length == 1)
        {
            var method = methods[0];
            delegateFactory = () => Compilation.GetMethodReferenceDelegate(method);
        }

        return BoundFactory.MethodGroupExpression(receiver, methods, delegateFactory);
    }

    protected ImmutableArray<IMethodSymbol> GetAccessibleMethods(
        ImmutableArray<IMethodSymbol> methods,
        Location location,
        bool reportIfInaccessible = true)
    {
        if (methods.IsDefaultOrEmpty)
            return methods;

        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in methods)
        {
            if (IsSymbolAccessible(method))
                builder.Add(method);
        }

        if (builder.Count > 0)
            return builder.ToImmutable();

        if (reportIfInaccessible)
            EnsureMemberAccessible(methods[0], location, "method");
        return ImmutableArray<IMethodSymbol>.Empty;
    }

    protected virtual bool EnsureMemberAccessible(ISymbol symbol, Location location, string symbolKind)
    {
        if (symbol is null)
            return true;

        if (symbol.DeclaredAccessibility == Accessibility.NotApplicable)
            return true;

        if (IsSymbolAccessible(symbol))
            return true;

        return false;
    }

    protected void ReportObsoleteIfNeeded(ISymbol symbol, Location location)
    {
        if (!TryGetObsoleteInfo(symbol, out var message, out var isError))
            return;

        var symbolKind = symbol.Kind.ToString().ToLowerInvariant();
        var display = symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var text = string.IsNullOrWhiteSpace(message)
            ? "No additional details provided."
            : message;

        _diagnostics.ReportObsoleteMember(
            symbolKind,
            display,
            text,
            location,
            isError ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning);
    }

    private static bool TryGetObsoleteInfo(ISymbol symbol, out string? message, out bool isError)
    {
        message = null;
        isError = false;

        foreach (var attribute in symbol.GetAttributes())
        {
            var attributeClass = attribute.AttributeClass;
            if (attributeClass is null)
                continue;

            if (!string.Equals(attributeClass.Name, "ObsoleteAttribute", StringComparison.Ordinal) &&
                !string.Equals(attributeClass.Name, "Obsolete", StringComparison.Ordinal))
            {
                continue;
            }

            if (attribute.ConstructorArguments.Length >= 1 &&
                attribute.ConstructorArguments[0].Value is string constructorMessage)
            {
                message = constructorMessage;
            }

            if (attribute.ConstructorArguments.Length >= 2 &&
                attribute.ConstructorArguments[1].Value is bool constructorIsError)
            {
                isError = constructorIsError;
            }

            foreach (var (name, value) in attribute.NamedArguments)
            {
                if (string.Equals(name, "Message", StringComparison.Ordinal) &&
                    value.Value is string namedMessage)
                {
                    message = namedMessage;
                    continue;
                }

                if (string.Equals(name, "IsError", StringComparison.Ordinal) &&
                    value.Value is bool namedIsError)
                {
                    isError = namedIsError;
                }
            }

            return true;
        }

        return false;
    }

    /// <summary>
    /// Attempts to interpret a member access expression like <c>System.Console</c> as a type name.
    /// If successful, returns the resolved <see cref="ITypeSymbol"/>.
    /// </summary>
    public bool TryBindMemberAccessExpressionAsType(MemberAccessExpressionSyntax syntax, out ITypeSymbol type)
    {
        type = Compilation.ErrorTypeSymbol;

        var parts = FlattenMemberAccess(syntax);
        if (parts.Length == 0)
            return false;

        // Binder-scope type parameters and imported scopes (same as BindType).
        var typeParams = GetInScopeTypeParameters();
        var importedScopes = GetImportedScopesForTypeResolution();

        // First segment: namespace or type.
        INamespaceSymbol? ns = null;
        INamedTypeSymbol? currentType = null;

        var first = parts[0];
        if (first.TypeArguments is not null)
        {
            var lookup = LookupNamedTypeByParts(new[] { first.Name }, importedScopes);
            if (lookup.IsAmbiguous || lookup.Definition is null)
                return false;

            var constructed = ConstructFrom(lookup.Definition, first.TypeArguments);
            if (!constructed.Success)
                return false;

            currentType = constructed.ResolvedType as INamedTypeSymbol;
            if (currentType is null)
                return false;
        }
        else
        {
            ns = LookupNamespace(first.Name);
            if (ns is null)
            {
                var lookup = LookupNamedTypeByParts(new[] { first.Name }, importedScopes);
                if (lookup.IsAmbiguous || lookup.Definition is null)
                    return false;

                currentType = lookup.Definition;
                if (currentType is null)
                    return false;
            }
        }

        // Walk remaining segments.
        for (int i = 1; i < parts.Length; i++)
        {
            var part = parts[i];
            var name = part.Name;

            if (currentType is null)
            {
                // Still in namespace chain.
                if (part.TypeArguments is null)
                {
                    var nextNs = ns!.LookupNamespace(name);
                    if (nextNs is not null)
                    {
                        ns = nextNs;
                        continue;
                    }
                }

                // Then a type in that namespace.
                var t = ns!.LookupType(name) as INamedTypeSymbol;
                if (t is null)
                    return false;

                var constructed = ConstructFrom(t, part.TypeArguments);
                if (!constructed.Success)
                    return false;

                currentType = constructed.ResolvedType as INamedTypeSymbol;
                if (currentType is null)
                    return false;
                continue;
            }

            // Nested type chain.
            var nested = currentType.GetTypeMembers(name);
            if (nested.Length != 1)
                return false;

            var nestedDef = nested[0];
            var nestedConstructed = ConstructFrom(nestedDef, part.TypeArguments);
            if (!nestedConstructed.Success)
                return false;

            currentType = nestedConstructed.ResolvedType as INamedTypeSymbol;
            if (currentType is null)
                return false;
        }

        if (currentType is null)
            return false;

        type = currentType;
        return type.TypeKind != TypeKind.Error;

        ResolveTypeResult ConstructFrom(INamedTypeSymbol definition, TypeArgumentListSyntax? typeArguments)
        {
            if (typeArguments is null)
            {
                return new ResolveTypeResult
                {
                    ResolvedType = definition,
                    ResolvedNamedDefinition = definition
                };
            }

            var args = BindTypeArguments(typeArguments, typeParams, importedScopes);
            if (!args.Success)
                return args;

            return Construct(definition, args.ResolvedTypeArguments);
        }

        static NamePart[] FlattenMemberAccess(MemberAccessExpressionSyntax node)
        {
            var parts = new List<NamePart>();

            void Walk(ExpressionSyntax expr)
            {
                switch (expr)
                {
                    case IdentifierNameSyntax id:
                        parts.Add(new NamePart(id.Identifier.ValueText, typeArguments: null));
                        break;

                    case GenericNameSyntax g:
                        parts.Add(new NamePart(g.Identifier.ValueText, g.TypeArgumentList));
                        break;

                    case MemberAccessExpressionSyntax ma:
                        Walk(ma.Expression);
                        switch (ma.Name)
                        {
                            case IdentifierNameSyntax mid:
                                parts.Add(new NamePart(mid.Identifier.ValueText, typeArguments: null));
                                break;
                            case GenericNameSyntax mg:
                                parts.Add(new NamePart(mg.Identifier.ValueText, mg.TypeArgumentList));
                                break;
                        }
                        break;
                }
            }

            Walk(node);
            return parts.ToArray();
        }
    }

    private readonly struct NamePart
    {
        public NamePart(string name, TypeArgumentListSyntax? typeArguments)
        {
            Name = name;
            TypeArguments = typeArguments;
        }

        public string Name { get; }
        public TypeArgumentListSyntax? TypeArguments { get; }
    }
}
