using System;
using System.Collections.Generic;

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
        var symbol = LookupSymbol(node.Identifier.Text);
        return symbol != null ? new SymbolInfo(symbol) : SymbolInfo.None;
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
                     ?? throw new NotImplementedException("BindExpression not implemented in root binder.");

        CacheBoundNode(expression, result);

        return result;
    }

    public virtual BoundStatement BindStatement(StatementSyntax statement)
    {
        if (TryGetCachedBoundNode(statement) is BoundStatement cached)
            return cached;

        var result = ParentBinder?.BindStatement(statement)
                     ?? throw new NotImplementedException("BindStatement not implemented in root binder.");

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

        throw new Exception($"Type '{typeSyntax}' could not be resolved.");
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
        => SemanticModel.TryGetCachedBoundNode(node);

    protected void CacheBoundNode(SyntaxNode node, BoundNode bound)
        => SemanticModel.CacheBoundNode(node, bound);

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