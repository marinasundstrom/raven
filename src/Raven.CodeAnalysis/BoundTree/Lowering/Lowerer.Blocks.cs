using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
    {
        var statements = new List<BoundStatement>();

        foreach (var statement in node.Statements)
        {
            if (statement is BoundLocalDeclarationStatement localDeclaration
                && TryRewritePropagateLocalDeclaration(localDeclaration, out var rewrittenLocalStatements))
            {
                statements.AddRange(rewrittenLocalStatements);
                continue;
            }

            if (statement is BoundExpressionStatement expressionStatement
                && TryRewritePropagateExpressionStatement(expressionStatement, out var rewrittenExpressionStatements))
            {
                statements.AddRange(rewrittenExpressionStatements);
                continue;
            }

            statements.Add((BoundStatement)VisitStatement(statement));
        }

        var loweredStatements = statements.ToImmutableArray();
        var handledUsingLocals = new HashSet<ILocalSymbol>(SymbolEqualityComparer.Default);
        var rewritten = RewriteUsingDeclarations(loweredStatements, handledUsingLocals);

        if (handledUsingLocals.Count == 0)
            return new BoundBlockStatement(rewritten, node.LocalsToDispose);

        var localsBuilder = ImmutableArray.CreateBuilder<ILocalSymbol>();
        foreach (var local in node.LocalsToDispose)
        {
            if (!handledUsingLocals.Contains(local))
                localsBuilder.Add(local);
        }

        return new BoundBlockStatement(rewritten, localsBuilder.ToImmutable());
    }

    private ImmutableArray<BoundStatement> RewriteUsingDeclarations(
        ImmutableArray<BoundStatement> statements,
        HashSet<ILocalSymbol> handledUsingLocals)
    {
        if (statements.IsDefaultOrEmpty)
            return statements;

        var builder = ImmutableArray.CreateBuilder<BoundStatement>();

        for (var i = 0; i < statements.Length; i++)
        {
            var statement = statements[i];

            if (statement is BoundLocalDeclarationStatement { IsUsing: true } usingDeclaration)
            {
                var declarators = usingDeclaration.Declarators.ToArray();

                foreach (var declarator in declarators)
                    handledUsingLocals.Add(declarator.Local);

                var loweredUsing = new BoundLocalDeclarationStatement(declarators);

                var remaining = statements.RemoveRange(0, i + 1);
                var tryBlockStatements = RewriteUsingDeclarations(remaining, handledUsingLocals);
                var tryBlock = new BoundBlockStatement(tryBlockStatements, ImmutableArray<ILocalSymbol>.Empty);

                var finallyStatements = CreateDisposeStatements(declarators);
                var finallyBlock = new BoundBlockStatement(finallyStatements, ImmutableArray<ILocalSymbol>.Empty);
                var tryStatement = new BoundTryStatement(tryBlock, ImmutableArray<BoundCatchClause>.Empty, finallyBlock);

                builder.Add(loweredUsing);
                builder.Add(tryStatement);

                return builder.ToImmutable();
            }

            builder.Add(statement);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<BoundStatement> CreateDisposeStatements(BoundVariableDeclarator[] declarators)
    {
        if (declarators.Length == 0)
            return ImmutableArray<BoundStatement>.Empty;

        var compilation = GetCompilation();
        var disposableType = compilation.GetSpecialType(SpecialType.System_IDisposable);
        if (disposableType.TypeKind == TypeKind.Error)
            return ImmutableArray<BoundStatement>.Empty;

        var disposeMethod = disposableType
            .GetMembers(nameof(IDisposable.Dispose))
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Parameters.Length == 0);

        if (disposeMethod is null)
            return ImmutableArray<BoundStatement>.Empty;

        var builder = ImmutableArray.CreateBuilder<BoundStatement>(declarators.Length);

        for (var i = declarators.Length - 1; i >= 0; i--)
        {
            var local = declarators[i].Local;
            if (local.Type is null || local.Type.TypeKind == TypeKind.Error)
                continue;

            var disposeStatement = CreateDisposeStatement(local, disposeMethod, compilation);
            if (disposeStatement is not null)
                builder.Add(disposeStatement);
        }

        return builder.ToImmutable();
    }

    private BoundStatement? CreateDisposeStatement(ILocalSymbol local, IMethodSymbol disposeMethod, Compilation compilation)
    {
        if (local.Type is null || local.Type.TypeKind == TypeKind.Error)
            return null;

        var disposeCall = new BoundExpressionStatement(
            new BoundInvocationExpression(disposeMethod, Array.Empty<BoundExpression>(), new BoundLocalAccess(local)));

        if (local.Type.IsReferenceType || local.Type.TypeKind == TypeKind.Null)
        {
            var nullLiteral = new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, local.Type);

            if (BoundBinaryOperator.TryLookup(compilation, SyntaxKind.NotEqualsToken, local.Type, local.Type, out var notEquals))
            {
                var condition2 = new BoundBinaryExpression(new BoundLocalAccess(local), notEquals, nullLiteral);
                return new BoundIfStatement(condition2, new BoundBlockStatement(new[] { disposeCall }));
            }

            var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
            var objectType = compilation.GetSpecialType(SpecialType.System_Object);
            if (booleanType.TypeKind == TypeKind.Error || objectType.TypeKind == TypeKind.Error)
                return disposeCall;

            var nullLiteralType = new LiteralTypeSymbol(objectType, constantValue: null!, compilation);
            var nullPattern = new BoundConstantPattern(nullLiteralType);
            var notNullPattern = new BoundNotPattern(nullPattern);
            var condition = new BoundIsPatternExpression(new BoundLocalAccess(local), notNullPattern, booleanType);
            return new BoundIfStatement(condition, new BoundBlockStatement(new[] { disposeCall }));
        }

        return disposeCall;
    }

    public override BoundNode? VisitIfStatement(BoundIfStatement node)
    {
        var condition = (BoundExpression)VisitExpression(node.Condition)!;
        var thenStatement = (BoundStatement)VisitStatement(node.ThenNode);
        var elseStatement = node.ElseNode is null ? null : (BoundStatement)VisitStatement(node.ElseNode);

        if (elseStatement is null)
        {
            var endLabel = CreateLabel("if_end");
            return new BoundBlockStatement([
                new BoundConditionalGotoStatement(endLabel, condition, jumpIfTrue: false),
                thenStatement,
                CreateLabelStatement(endLabel),
            ]);
        }
        else
        {
            var elseLabel = CreateLabel("if_else");
            var endLabel = CreateLabel("if_end");
            return new BoundBlockStatement([
                new BoundConditionalGotoStatement(elseLabel, condition, jumpIfTrue: false),
                thenStatement,
                new BoundGotoStatement(endLabel),
                new BoundLabeledStatement(elseLabel, elseStatement),
                CreateLabelStatement(endLabel),
            ]);
        }
    }
}
