using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
    {
        var statements = new List<BoundStatement>();

        foreach (var statement in node.Statements)
        {
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

        return new BoundBlockStatement(rewritten, localsBuilder.MoveToImmutable());
    }

    private ImmutableArray<BoundStatement> RewriteUsingDeclarations(
        ImmutableArray<BoundStatement> statements,
        HashSet<ILocalSymbol> handledUsingLocals)
    {
        if (statements.IsDefaultOrEmpty)
            return statements;

        var builder = ImmutableArray.CreateBuilder<BoundStatement>(statements.Length);

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

                var finallyLocalsBuilder = ImmutableArray.CreateBuilder<ILocalSymbol>(declarators.Length);
                foreach (var declarator in declarators)
                    finallyLocalsBuilder.Add(declarator.Local);

                var finallyBlock = new BoundBlockStatement(ImmutableArray<BoundStatement>.Empty, finallyLocalsBuilder.MoveToImmutable());
                var tryStatement = new BoundTryStatement(tryBlock, ImmutableArray<BoundCatchClause>.Empty, finallyBlock);

                builder.Add(loweredUsing);
                builder.Add(tryStatement);

                return builder.MoveToImmutable();
            }

            builder.Add(statement);
        }

        return builder.MoveToImmutable();
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

