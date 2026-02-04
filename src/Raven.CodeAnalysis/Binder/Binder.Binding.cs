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

        // First segment: namespace or type.
        INamespaceSymbol? ns = LookupNamespace(parts[0]);
        INamedTypeSymbol? currentType = null;

        if (ns is null)
        {
            currentType = LookupType(parts[0]) as INamedTypeSymbol;
            if (currentType is null)
                return false;
        }

        // Walk remaining segments.
        for (int i = 1; i < parts.Length; i++)
        {
            var name = parts[i];

            if (currentType is null)
            {
                // Still in namespace chain.
                var nextNs = ns!.LookupNamespace(name);
                if (nextNs is not null)
                {
                    ns = nextNs;
                    continue;
                }

                // Then a type in that namespace.
                var t = ns!.LookupType(name) as INamedTypeSymbol;
                if (t is null)
                    return false;

                currentType = t;
                continue;
            }

            // Nested type chain.
            var nested = currentType.GetTypeMembers(name);
            if (nested.Length != 1)
                return false;

            currentType = nested[0];
        }

        if (currentType is null)
            return false;

        type = currentType;
        return type.TypeKind != TypeKind.Error;

        static string[] FlattenMemberAccess(MemberAccessExpressionSyntax node)
        {
            var parts = new List<string>();

            void Walk(ExpressionSyntax expr)
            {
                switch (expr)
                {
                    case IdentifierNameSyntax id:
                        parts.Add(id.Identifier.ValueText);
                        break;

                    case MemberAccessExpressionSyntax ma:
                        Walk(ma.Expression);
                        parts.Add(ma.Name.Identifier.ValueText);
                        break;
                }
            }

            Walk(node);
            return parts.ToArray();
        }
    }
}
