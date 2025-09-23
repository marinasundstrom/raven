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

        var name = node.Identifier.Text;
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
        var ns = CurrentNamespace?.LookupNamespace(name);
        if (ns != null)
            return ns;

        ns = Compilation.GlobalNamespace.LookupNamespace(name);
        if (ns != null)
            return ns;

        return ParentBinder?.LookupNamespace(name);
    }

    public virtual ISymbol? LookupSymbol(string name)
    {
        return ParentBinder?.LookupSymbol(name);
    }

    public virtual IEnumerable<ISymbol> LookupSymbols(string name)
    {
        return ParentBinder?.LookupSymbols(name) ?? Enumerable.Empty<ISymbol>();
    }

    public virtual BoundExpression BindExpression(ExpressionSyntax expression)
    {
        if (TryGetCachedBoundNode(expression) is BoundExpression cached)
            return cached;

        var result = ParentBinder?.BindExpression(expression)
                     ?? new BoundErrorExpression(Compilation.ErrorTypeSymbol);

        CacheBoundNode(expression, result);

        return result;
    }

    public virtual BoundStatement BindStatement(StatementSyntax statement)
    {
        if (TryGetCachedBoundNode(statement) is BoundStatement cached)
            return cached;

        var result = ParentBinder?.BindStatement(statement)
                     ?? new BoundExpressionStatement(new BoundErrorExpression(Compilation.ErrorTypeSymbol));

        CacheBoundNode(statement, result);

        return result;
    }

    public virtual ITypeSymbol ResolveType(TypeSyntax typeSyntax)
    {
        if (typeSyntax is NullTypeSyntax)
        {
            return Compilation.NullTypeSymbol;
        }

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
            return new LiteralTypeSymbol(underlying, value, Compilation);
        }

        if (typeSyntax is ByRefTypeSyntax byRef)
        {
            return ResolveType(byRef.ElementType);
        }

        if (typeSyntax is NullableTypeSyntax nb)
        {
            var elementType = ResolveType(nb.ElementType);
            return new NullableTypeSymbol(elementType, null, null, null, []);
        }

        if (typeSyntax is UnionTypeSyntax ut)
        {
            var types = new List<ITypeSymbol>();
            foreach (var t in ut.Types)
            {
                if (t is NullableTypeSyntax nt)
                {
                    _diagnostics.ReportNullableTypeInUnion(nt.GetLocation());
                    return Compilation.ErrorTypeSymbol;
                }

                types.Add(ResolveType(t));
            }

            return new UnionTypeSymbol(types, null, null, null, []);
        }

        if (typeSyntax is TupleTypeSyntax tupleTypeSyntax)
        {
            var elements = tupleTypeSyntax.Elements
                .Select(e => (e.NameColon?.Name.ToString(), ResolveType(e.Type)))
                .ToArray();
            return Compilation.CreateTupleTypeSymbol(elements);
        }

        if (typeSyntax is PredefinedTypeSyntax predefinedTypeSyntax)
        {
            return Compilation.ResolvePredefinedType(predefinedTypeSyntax);
        }

        if (typeSyntax is UnitTypeSyntax)
        {
            return Compilation.GetSpecialType(SpecialType.System_Unit);
        }

        if (typeSyntax is IdentifierNameSyntax ident)
        {
            var type = LookupType(ident.Identifier.Text);
            if (type is INamedTypeSymbol named)
            {
                if (named.Arity > 0 && named.IsUnboundGenericType)
                {
                    var zeroArity = FindAccessibleNamedType(ident.Identifier.Text, 0);
                    if (zeroArity is null)
                    {
                        _diagnostics.ReportTypeRequiresTypeArguments(named.Name, named.Arity, ident.Identifier.GetLocation());
                        return Compilation.ErrorTypeSymbol;
                    }

                    return zeroArity;
                }

                return NormalizeDefinition(named);
            }

            if (type is not null)
                return type;
        }

        if (typeSyntax is GenericNameSyntax generic)
        {
            var arity = ComputeGenericArity(generic);
            var typeArgs = ResolveGenericTypeArguments(generic);

            var symbol = LookupType(generic.Identifier.Text) as INamedTypeSymbol;
            if (symbol is not null)
            {
                symbol = NormalizeDefinition(symbol);
                if (symbol.Arity != arity)
                    symbol = FindAccessibleNamedType(generic.Identifier.Text, arity);
            }
            else
            {
                symbol = FindAccessibleNamedType(generic.Identifier.Text, arity);
            }

            if (symbol is null)
            {
                _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(generic.Identifier.Text, generic.GetLocation());
                return Compilation.ErrorTypeSymbol;
            }

            var constructed = TryConstructGeneric(symbol, typeArgs, arity);
            if (constructed is not null)
                return constructed;

            return symbol;
        }

        if (typeSyntax is QualifiedNameSyntax qualified)
        {
            var symbol = ResolveQualifiedType(qualified);
            if (symbol is not null)
                return symbol;
        }

        var name = typeSyntax switch
        {
            IdentifierNameSyntax id => id.Identifier.Text,
            _ => typeSyntax.ToString()
        };

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, typeSyntax.GetLocation());
        return Compilation.ErrorTypeSymbol;
    }

    private ITypeSymbol? ResolveQualifiedType(QualifiedNameSyntax qualified)
    {
        var left = ResolveQualifiedLeft(qualified.Left);

        if (left is INamespaceSymbol ns)
        {
            if (qualified.Right is IdentifierNameSyntax id)
            {
                var type = SelectByArity(ns.GetMembers(id.Identifier.Text)
                        .OfType<INamedTypeSymbol>(), 0)
                    ?? ns.LookupType(id.Identifier.Text);

                if (type is INamedTypeSymbol named && NormalizeDefinition(named).Arity > 0)
                {
                    _diagnostics.ReportTypeRequiresTypeArguments(named.Name, named.Arity, id.Identifier.GetLocation());
                    return Compilation.ErrorTypeSymbol;
                }

                return type;
            }

            if (qualified.Right is GenericNameSyntax gen)
            {
                return ResolveGenericMember(ns, gen);
            }

            return null;
        }

        if (left is ITypeSymbol leftType)
        {
            if (qualified.Right is IdentifierNameSyntax id)
            {
                return SelectByArity(leftType.GetMembers(id.Identifier.Text)
                    .OfType<INamedTypeSymbol>(), 0);
            }

            if (qualified.Right is GenericNameSyntax gen)
            {
                return ResolveGenericMember(leftType, gen);
            }

            return null;
        }

        return null;
    }

    private object? ResolveQualifiedLeft(TypeSyntax left)
    {
        if (left is IdentifierNameSyntax id)
        {
            var ns = LookupNamespace(id.Identifier.Text);
            if (ns is not null)
                return ns;

            var type = LookupType(id.Identifier.Text);
            if (type is INamedTypeSymbol named)
            {
                var definition = NormalizeDefinition(named);
                if (definition.Arity > 0)
                {
                    var zeroArity = FindAccessibleNamedType(id.Identifier.Text, 0);
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

            return null;
        }

        if (left is GenericNameSyntax gen)
        {
            var arity = ComputeGenericArity(gen);
            var typeArgs = ResolveGenericTypeArguments(gen);

            var symbol = LookupType(gen.Identifier.Text) as INamedTypeSymbol;
            if (symbol is not null)
            {
                symbol = NormalizeDefinition(symbol);
                if (symbol.Arity != arity)
                    symbol = FindAccessibleNamedType(gen.Identifier.Text, arity);
            }
            else
            {
                symbol = FindAccessibleNamedType(gen.Identifier.Text, arity);
            }

            if (symbol is null)
                return null;

            var constructed = TryConstructGeneric(symbol, typeArgs, arity);
            return constructed ?? symbol;
        }

        if (left is QualifiedNameSyntax qualified)
        {
            return ResolveQualifiedType(qualified);
        }

        return null;
    }

    protected ISymbol? ResolveName(NameSyntax name)
    {
        return name switch
        {
            IdentifierNameSyntax id => LookupSymbol(id.Identifier.Text)
                ?? (ISymbol?)LookupNamespace(id.Identifier.Text)
                ?? LookupType(id.Identifier.Text),
            GenericNameSyntax gen => ResolveGenericName(gen),
            QualifiedNameSyntax qn => ResolveQualifiedName(qn),
            _ => null
        };
    }

    private ISymbol? ResolveGenericName(GenericNameSyntax gen)
    {
        var arity = ComputeGenericArity(gen);
        var typeArgs = ResolveGenericTypeArguments(gen);

        var symbol = LookupType(gen.Identifier.Text) as INamedTypeSymbol;
        if (symbol is not null)
        {
            symbol = NormalizeDefinition(symbol);
            if (symbol.Arity != arity)
                symbol = FindAccessibleNamedType(gen.Identifier.Text, arity);
        }
        else
        {
            symbol = FindAccessibleNamedType(gen.Identifier.Text, arity);
        }

        if (symbol is null)
            return null;

        return TryConstructGeneric(symbol, typeArgs, arity) ?? symbol;
    }

    private ISymbol? ResolveQualifiedName(QualifiedNameSyntax qn)
    {
        var left = ResolveName(qn.Left);

        if (left is INamespaceSymbol ns)
        {
            if (qn.Right is IdentifierNameSyntax id)
                return (ISymbol?)ns.LookupNamespace(id.Identifier.Text)
                    ?? SelectByArity(ns.GetMembers(id.Identifier.Text)
                        .OfType<INamedTypeSymbol>(), 0)
                    ?? ns.LookupType(id.Identifier.Text);

            if (qn.Right is GenericNameSyntax gen)
            {
                return ResolveGenericMember(ns, gen);
            }

            return null;
        }

        if (left is ITypeSymbol type)
        {
            if (qn.Right is IdentifierNameSyntax id)
                return SelectByArity(type.GetMembers(id.Identifier.Text)
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
        SemanticModel?.CacheBoundNode(node, bound);
    }

    public virtual BoundNode GetOrBind(SyntaxNode node)
    {
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
                if (candidate.Arity == arity)
                    return candidate;
            }
        }

        if (LookupType(name) is INamedTypeSymbol fallback)
        {
            var candidate = NormalizeDefinition(fallback);
            if (candidate.Arity == arity)
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

    private ITypeSymbol? ResolveGenericMember(INamespaceOrTypeSymbol container, GenericNameSyntax generic)
    {
        var arity = ComputeGenericArity(generic);
        var definition = SelectByArity(container.GetMembers(generic.Identifier.Text)
            .OfType<INamedTypeSymbol>(), arity);

        if (definition is null)
            return null;

        var typeArguments = ResolveGenericTypeArguments(generic);
        var constructed = TryConstructGeneric(definition, typeArguments, arity);

        return constructed ?? definition;
    }

    private static INamedTypeSymbol? SelectByArity(IEnumerable<INamedTypeSymbol> candidates, int arity)
    {
        foreach (var candidate in candidates)
        {
            var definition = NormalizeDefinition(candidate);
            if (definition.Arity == arity)
                return definition;
        }

        return null;
    }
}
