using System.Collections.Immutable;
using System.Linq.Expressions;

namespace Raven.CodeAnalysis;

abstract partial class BoundTreeRewriter : BoundTreeVisitor<BoundNode?>
{
    public override BoundNode? Visit(BoundNode? node)
    {
        return node?.Accept(this);
    }

    public virtual IEnumerable<T> VisitList<T>(IEnumerable<T> nodes)
        where T : BoundNode
    {
        foreach (var node in nodes)
            yield return (T)node.Accept(this)!;
    }

    public virtual ImmutableArray<T> VisitSymbolList<T>(IEnumerable<ISymbol?> symbols)
        where T : ISymbol
    {
        var builder = ImmutableArray.CreateBuilder<T>();

        foreach (var symbol in symbols)
        {
            if (symbol is null)
            {
                builder.Add((T)symbol!);
                continue;
            }

            builder.Add((T)VisitSymbol(symbol)!);
        }

        return builder.MoveToImmutable();
    }

    public virtual BoundStatement VisitStatement(BoundStatement statement)
    {
        return statement switch
        {
            BoundReturnStatement ret => (BoundStatement)VisitReturnStatement(ret)!,
            BoundExpressionStatement expr => (BoundStatement)VisitExpressionStatement(expr)!,
            BoundLocalDeclarationStatement localDecl => (BoundStatement)VisitLocalDeclarationStatement(localDecl)!,
            BoundFunctionStatement func => (BoundStatement)VisitFunctionStatement(func)!,
            BoundIfStatement ifStmt => (BoundStatement)VisitIfStatement(ifStmt)!,
            BoundTryStatement tryStmt => (BoundStatement)VisitTryStatement(tryStmt)!,
            BoundWhileStatement whileStmt => (BoundStatement)VisitWhileStatement(whileStmt)!,
            BoundForStatement forStmt => (BoundStatement)VisitForStatement(forStmt)!,
            BoundBlockStatement blockStmt => (BoundStatement)VisitBlockStatement(blockStmt)!,
            BoundAssignmentStatement assignmentStmt => (BoundStatement)VisitAssignmentStatement(assignmentStmt)!,
            _ => statement,
        };
    }

    public virtual BoundExpression? VisitExpression(BoundExpression? node)
    {
        if (node is null)
            return null;

        return node switch
        {
            BoundLiteralExpression lit => (BoundExpression)VisitLiteralExpression(lit)!,
            BoundLocalAccess local => (BoundExpression)VisitLocalAccess(local)!,
            BoundParameterAccess par => (BoundExpression)VisitParameterAccess(par)!,
            BoundBinaryExpression bin => (BoundExpression)VisitBinaryExpression(bin)!,
            BoundInvocationExpression call => (BoundExpression)VisitInvocationExpression(call)!,
            BoundObjectCreationExpression objectCreation => (BoundExpression)VisitObjectCreationExpression(objectCreation)!,
            BoundLambdaExpression lambda => (BoundExpression)VisitLambdaExpression(lambda)!,
            BoundBlockExpression block => (BoundExpression)VisitBlockExpression(block)!,
            BoundAssignmentExpression assignment => (BoundExpression)VisitAssignmentExpression(assignment)!,
            BoundCastExpression cast => (BoundExpression)VisitCastExpression(cast)!,
            BoundAsExpression asExpr => (BoundExpression)VisitAsExpression(asExpr)!,
            BoundDelegateCreationExpression delegateCreation => (BoundExpression)VisitDelegateCreationExpression(delegateCreation)!,
            BoundMethodGroupExpression methodGroup => (BoundExpression)VisitMethodGroupExpression(methodGroup)!,
            BoundTypeOfExpression typeOfExpression => (BoundExpression)VisitTypeOfExpression(typeOfExpression)!,
            _ => throw new NotImplementedException($"Unhandled expression: {node.GetType().Name}"),
        };
    }

    public virtual BoundNode? VisitAssignmentExpression(BoundAssignmentExpression node) => node;

    public virtual INamespaceSymbol VisitNamespace(INamespaceSymbol @namespace)
    {
        return @namespace;
    }

    public virtual ISymbol? VisitSymbol(ISymbol? symbol)
    {
        return symbol switch
        {
            null => null,
            INamespaceSymbol ns => VisitNamespace(ns),
            ITypeSymbol type => VisitType(type),
            IMethodSymbol method => VisitMethod(method),
            IParameterSymbol parameter => VisitParameter(parameter),
            IPropertySymbol prop => VisitProperty(prop),
            IFieldSymbol field => VisitField(field),
            ILocalSymbol local => VisitLocal(local),
            _ => throw new Exception($"Unhandled symbol type: {symbol.GetType()}")
        };
    }

    public virtual ITypeSymbol VisitType(ITypeSymbol type)
    {
        return type;
    }

    public virtual IMethodSymbol VisitMethod(IMethodSymbol method)
    {
        return method;
    }

    public virtual IParameterSymbol VisitParameter(IParameterSymbol param)
    {
        return param;
    }

    public virtual IPropertySymbol VisitProperty(IPropertySymbol property)
    {
        return property;
    }

    public virtual IFieldSymbol VisitField(IFieldSymbol field)
    {
        return field;
    }

    public virtual ILocalSymbol VisitLocal(ILocalSymbol local)
    {
        return local;
    }

    public virtual BoundNode VisitPattern(BoundPattern pattern)
    {
        return pattern.Accept(this);
    }

    public virtual BoundNode VisitDesignator(BoundDesignator designator)
    {
        return designator.Accept(this);
    }

}
