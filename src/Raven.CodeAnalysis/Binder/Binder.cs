using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal abstract class Binder
{
    protected readonly Dictionary<string, ISymbol> SymbolTable = new();
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

        return ParentBinder?.LookupNamespace(name);
    }

    public void DeclareSymbol(string name, ISymbol symbol)
    {
        if (SymbolTable.ContainsKey(name))
            throw new Exception($"Symbol '{name}' is already declared in this scope.");
        SymbolTable[name] = symbol;
    }

    public virtual ISymbol? LookupSymbol(string name)
    {
        return SymbolTable.TryGetValue(name, out var symbol) ? symbol : ParentBinder?.LookupSymbol(name);
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
        if (typeSyntax is NullableTypeSyntax nb)
        {
            // INFO: Temporary workaround
            typeSyntax = nb.ElementType;
        }

        if (typeSyntax is UnionTypeSyntax ut)
        {
            var types = ut.Types.Select(x => ResolveType(x)).ToArray();
            return new UnionTypeSymbol(types, null, null, null, []);
        }

        if (typeSyntax is PredefinedTypeSyntax predefinedTypeSyntax)
        {
            return Compilation.ResolvePredefinedType(predefinedTypeSyntax);
        }

        if (typeSyntax is IdentifierNameSyntax ident)
        {
            var type = LookupType(ident.Identifier.Text);
            if (type is not null)
                return type;
        }

        if (typeSyntax is GenericNameSyntax generic)
        {
            var typeArgs = generic.TypeArgumentList.Arguments
                .Select(arg => ResolveType(arg.Type))
                .ToArray();

            var symbol = LookupType(generic.Identifier.Text) as INamedTypeSymbol;
            if (symbol is not null && symbol.Arity == typeArgs.Length)
            {
                return Compilation.ConstructGenericType(symbol, typeArgs);
            }
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

        _diagnostics.ReportUndefinedName(name, typeSyntax.GetLocation());
        return Compilation.ErrorTypeSymbol;
    }

    private ITypeSymbol? ResolveQualifiedType(QualifiedNameSyntax qualified)
    {
        var left = ResolveQualifiedLeft(qualified.Left);

        if (left is INamespaceSymbol ns)
        {
            if (qualified.Right is IdentifierNameSyntax id)
            {
                return ns.LookupType(id.Identifier.Text);
            }

            if (qualified.Right is GenericNameSyntax gen)
            {
                var unconstructed = ns.LookupType(gen.Identifier.Text) as INamedTypeSymbol;
                if (unconstructed is null)
                    return null;

                var args = gen.TypeArgumentList.Arguments
                    .Select(a => ResolveType(a.Type))
                    .ToArray();

                if (unconstructed.Arity != args.Length)
                    return null;

                return Compilation.ConstructGenericType(unconstructed, args);
            }

            return null;
        }

        if (left is ITypeSymbol leftType)
        {
            if (qualified.Right is IdentifierNameSyntax id)
            {
                return leftType.GetMembers(id.Identifier.Text)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault(t => t.Arity == 0);
            }

            if (qualified.Right is GenericNameSyntax gen)
            {
                var unconstructed = leftType.GetMembers(gen.Identifier.Text)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault(t => t.Arity == gen.TypeArgumentList.Arguments.Count);

                if (unconstructed is null)
                    return null;

                var args = gen.TypeArgumentList.Arguments
                    .Select(a => ResolveType(a.Type))
                    .ToArray();

                return Compilation.ConstructGenericType(unconstructed, args);
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
            if (type is not null)
                return type;

            return null;
        }

        if (left is GenericNameSyntax gen)
        {
            var args = gen.TypeArgumentList.Arguments
                .Select(a => ResolveType(a.Type))
                .ToArray();

            var symbol = LookupType(gen.Identifier.Text) as INamedTypeSymbol;
            if (symbol is not null && symbol.Arity == args.Length)
            {
                return Compilation.ConstructGenericType(symbol, args);
            }

            return null;
        }

        if (left is QualifiedNameSyntax qualified)
        {
            return ResolveQualifiedType(qualified);
        }

        return null;
    }

    public virtual IEnumerable<ISymbol> LookupAvailableSymbols()
    {
        throw new NotImplementedException();
    }

    public ISymbol? LookupLocalSymbol(string name)
    {
        return LookupSymbol(name);
    }

    public virtual BoundLocalFunctionStatement BindLocalFunction(LocalFunctionStatementSyntax localFunction)
    {
        return ParentBinder?.BindLocalFunction(localFunction)
             ?? throw new NotImplementedException("BindLocalFunction not implemented in root binder.");
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
}