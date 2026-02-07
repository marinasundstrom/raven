using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports 'var' locals that are never reassigned and suggests using 'val' instead.
/// Also determines whether a local is rebound (written after declaration).
/// </summary>
public sealed class VarCanBeValAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9004";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Variable can be immutable",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Variable '{0}' is never reassigned; consider 'val {0}'",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(
            AnalyzeBodyOwner,
            SyntaxKind.MethodDeclaration,
            SyntaxKind.FunctionStatement);
    }

    private static void AnalyzeBodyOwner(SyntaxNodeAnalysisContext context)
    {
        var body = context.Node switch
        {
            MethodDeclarationSyntax m => (SyntaxNode?)m.Body ?? m.ExpressionBody,
            FunctionStatementSyntax f => (SyntaxNode?)f.Body ?? f.ExpressionBody,
            _ => null
        };

        if (body is null)
            return;

        var collector = new VarRebindingCollector(context.SemanticModel);
        collector.Visit(body);

        foreach (var candidate in collector.GetVarThatCanBeVal())
        {
            var diagnostic = Diagnostic.Create(
                Descriptor,
                candidate.IdentifierToken.GetLocation(),
                candidate.Name);

            context.ReportDiagnostic(diagnostic);
        }
    }

    private readonly struct VarCandidate
    {
        public readonly ILocalSymbol Local;
        public readonly string Name;
        public readonly SyntaxToken IdentifierToken;

        public VarCandidate(ILocalSymbol local, string name, SyntaxToken identifierToken)
        {
            Local = local;
            Name = name;
            IdentifierToken = identifierToken;
        }
    }

    private sealed class VarRebindingCollector : SyntaxWalker
    {
        private readonly SemanticModel _semanticModel;

        private readonly Dictionary<ILocalSymbol, VarCandidate> _varLocals =
            new(SymbolEqualityComparer.Default);

        private readonly HashSet<ILocalSymbol> _writtenAfterDeclaration =
            new(SymbolEqualityComparer.Default);

        public VarRebindingCollector(SemanticModel semanticModel)
        {
            _semanticModel = semanticModel;
        }

        public IEnumerable<VarCandidate> GetVarThatCanBeVal()
        {
            foreach (var kvp in _varLocals)
            {
                var local = kvp.Key;
                var candidate = kvp.Value;

                if (!_writtenAfterDeclaration.Contains(local))
                    yield return candidate;
            }
        }

        // --- Core traversal helpers ---------------------------------------------

        private void VisitMaybe(SyntaxNode? node)
        {
            if (node is not null)
                node.Accept(this);
        }

        private void VisitMaybe(ExpressionSyntax? expr)
        {
            if (expr is not null)
                Visit(expr);
        }

        private void VisitMaybe(StatementSyntax? stmt)
        {
            if (stmt is not null)
                stmt.Accept(this);
        }

        // --- Blocks --------------------------------------------------------------

        public override void VisitBlockStatement(BlockStatementSyntax node)
        {
            foreach (var statement in node.Statements)
                statement.Accept(this);
        }

        // --- Decls / writes ------------------------------------------------------

        public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            var isVar = IsVarDeclaration(node);

            if (isVar)
            {
                foreach (var declarator in node.Declaration.Declarators)
                {
                    if (_semanticModel.GetDeclaredSymbol(declarator) is ILocalSymbol local)
                    {
                        var name = local.Name;
                        var idToken = declarator.Identifier;
                        _varLocals[local] = new VarCandidate(local, name, idToken);
                    }
                }
            }

            // Keep walking initializer expressions etc (if your base walker does it).
            // If it doesn't, you should explicitly visit them here.
            base.VisitLocalDeclarationStatement(node);
        }

        public override void VisitAssignmentStatement(AssignmentStatementSyntax node)
        {
            MarkWritten(node.Left);
            Visit(node.Right);
        }

        public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
        {
            MarkWritten(node.Left);
            Visit(node.Right);
        }

        public override void VisitUnaryExpression(UnaryExpressionSyntax node)
        {
            if (IsIncrementOrDecrement(node.OperatorToken))
                MarkWrittenFromExpression(node.Expression);

            // If your base walker does nothing, replace with explicit Visit(...) calls.
            base.VisitUnaryExpression(node);
        }

        public override void VisitPostfixUnaryExpression(PostfixUnaryExpressionSyntax node)
        {
            if (IsIncrementOrDecrement(node.OperatorToken))
                MarkWrittenFromExpression(node.Expression);

            base.VisitPostfixUnaryExpression(node);
        }

        public override void VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            // Optional: ref/out handling (as you already sketched)
            // ...

            Visit(node.Expression);
            foreach (var arg in node.ArgumentList.Arguments)
                Visit(arg.Expression);
        }

        // --- CONTROL FLOW: add these --------------------------------------------

        public override void VisitIfStatement(IfStatementSyntax node)
        {
            Visit(node.Condition);

            // adjust names: ThenStatement/Then, ElseClause/Else, etc.
            VisitMaybe(node.ThenStatement);

            if (node.ElseClause is not null)
                VisitMaybe(node.ElseClause.Statement);
        }

        public override void VisitWhileStatement(WhileStatementSyntax node)
        {
            Visit(node.Condition);
            VisitMaybe(node.Statement);
        }

        public override void VisitForStatement(ForStatementSyntax node)
        {
            VisitMaybe(node.Expression);

            VisitMaybe(node.Body);
        }

        public override void VisitReturnStatement(ReturnStatementSyntax node)
        {
            VisitMaybe(node.Expression);
        }

        public override void VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            Visit(node.Expression);
        }

        // If you have match/switch-like constructs, add them too:
        public override void VisitMatchExpression(MatchExpressionSyntax node)
        {
            Visit(node.Expression);
            foreach (var arm in node.Arms)
                arm.Accept(this);
        }

        public override void VisitMatchArm(MatchArmSyntax node)
        {
            // arm pattern + optional guard + expression/body
            // adjust names
            VisitMaybe(node.Pattern);
            VisitMaybe(node.Expression);
        }

        // --- write marking -------------------------------------------------------

        private void MarkWritten(ExpressionOrPatternSyntax left)
        {
            foreach (var local in DataFlowAnalysisHelpers.GetAssignedLocals(_semanticModel, left))
                _writtenAfterDeclaration.Add(local);
        }

        private void MarkWrittenFromExpression(ExpressionSyntax expression)
        {
            foreach (var local in DataFlowAnalysisHelpers.GetAssignedLocals(_semanticModel, expression))
                _writtenAfterDeclaration.Add(local);
        }

        private static bool IsIncrementOrDecrement(SyntaxToken operatorToken)
            => operatorToken.Kind is SyntaxKind.PlusPlusToken or SyntaxKind.MinusMinusToken;

        private bool IsVarDeclaration(LocalDeclarationStatementSyntax node)
        {
            if (node.Declaration.BindingKeyword.Kind == SyntaxKind.VarKeyword)
                return true;

            return false;
        }
    }
}
