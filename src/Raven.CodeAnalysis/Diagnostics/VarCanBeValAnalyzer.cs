using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

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
        context.RegisterSyntaxTreeAction(AnalyzeTree);
    }

    private static void AnalyzeTree(SyntaxTreeAnalysisContext context)
    {
        var semanticModel = context.Compilation.GetSemanticModel(context.SyntaxTree);
        var root = context.SyntaxTree.GetRoot();

        // Analyze each body separately so locals from different bodies don't mix.
        foreach (var owner in root.DescendantNodes().Where(IsBodyOwner))
        {
            var body = GetBodySyntax(owner);
            if (body is null)
                continue;

            var collector = new VarRebindingCollector(semanticModel);
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
    }

    private static bool IsBodyOwner(SyntaxNode node)
        => node is MethodDeclarationSyntax
        || node is FunctionStatementSyntax
        // Add other “body owners” as you introduce them:
        // || node is AccessorDeclarationSyntax
        // || node is LambdaExpressionSyntax
        ;

    private static SyntaxNode? GetBodySyntax(SyntaxNode owner)
    {
        return owner switch
        {
            MethodDeclarationSyntax m => (SyntaxNode?)m.Body ?? m.ExpressionBody,
            FunctionStatementSyntax f => (SyntaxNode?)f.Body ?? f.ExpressionBody,
            _ => null
        };
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

        // var locals declared in this body (and their diagnostic location)
        private readonly Dictionary<ILocalSymbol, VarCandidate> _varLocals =
            new(SymbolEqualityComparer.Default);

        // locals that are written via assignment/compound assignment/etc (excluding declaration)
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

        public override void VisitBlockStatement(BlockStatementSyntax node)
        {
            foreach (var statement in node.Statements)
            {
                statement.Accept(this);
            }
        }

        public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            // Detect "var" declarations. Adjust this to your syntax model.
            // Common shapes:
            // - node.Declaration.Keyword.Kind == SyntaxKind.VarKeyword
            // - node.Declaration.IsVar
            // - node.Declaration.Kind == LocalDeclarationKind.Var
            var isVar = IsVarDeclaration(node);

            if (isVar)
            {
                foreach (var declarator in node.Declaration.Declarators)
                {
                    // Use declarator symbol as the ground truth for the local.
                    if (_semanticModel.GetDeclaredSymbol(declarator) is ILocalSymbol local)
                    {
                        // Prefer declarator.Identifier; fall back to local.Name if needed.
                        var name = local.Name;

                        // Again, adjust if your declarator token differs.
                        var idToken = declarator.Identifier;

                        _varLocals[local] = new VarCandidate(local, name, idToken);
                    }
                }
            }

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

        public override void VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            // If you can reach bound invocation info, treat ref/out arguments as writes.
            // If not, this safely does nothing beyond normal traversal.
            var bound = _semanticModel.GetBoundNode(node) as BoundInvocationExpression;
            if (bound is not null)
            {
                foreach (var arg in bound.Arguments)
                {
                    /*
                    if (arg.RefKind is RefKind.Ref or RefKind.Out)
                    {
                        // If the argument expression is (or contains) a local lvalue, mark it written.
                        // We can use the syntax-side expression if available; otherwise you can extend this.
                        if (arg.Syntax is ArgumentSyntax argSyntax)
                            MarkWrittenFromExpression(argSyntax.Expression);
                    }
                    */
                }
            }

            // Regular traversal
            Visit(node.Expression);
            foreach (var arg in node.ArgumentList.Arguments)
                Visit(arg.Expression);
        }

        // If Raven has ++/-- syntax nodes, add these overrides.
        // If you don’t, you can delete them; they’re here for completeness.
        public override void VisitUnaryExpression(UnaryExpressionSyntax node)
        {
            if (IsIncrementOrDecrement(node.OperatorToken))
                MarkWrittenFromExpression(node.Expression);

            base.VisitUnaryExpression(node);
        }

        public override void VisitPostfixUnaryExpression(PostfixUnaryExpressionSyntax node)
        {
            if (IsIncrementOrDecrement(node.OperatorToken))
                MarkWrittenFromExpression(node.Expression);

            base.VisitPostfixUnaryExpression(node);
        }

        private void MarkWritten(ExpressionOrPatternSyntax left)
        {
            foreach (var local in DataFlowAnalysisHelpers.GetAssignedLocals(_semanticModel, left))
                _writtenAfterDeclaration.Add(local);
        }

        private void MarkWrittenFromExpression(ExpressionSyntax expression)
        {
            // This covers cases where we only have ExpressionSyntax (e.g. ref/out arguments, ++x operand).
            foreach (var local in DataFlowAnalysisHelpers.GetAssignedLocals(_semanticModel, expression))
                _writtenAfterDeclaration.Add(local);
        }

        private static bool IsIncrementOrDecrement(SyntaxToken operatorToken)
        {
            // Adjust these kinds to your SyntaxKind names.
            return operatorToken.Kind is SyntaxKind.PlusPlusToken or SyntaxKind.MinusMinusToken;
        }

        private bool IsVarDeclaration(LocalDeclarationStatementSyntax node)
        {
            // Adjust to your Raven syntax surface.
            // Here are a few common patterns; pick the one that matches your AST.

            // Pattern A: keyword token on the declaration (recommended)
            if (node.Declaration.BindingKeyword.Kind == SyntaxKind.VarKeyword)
                return true;

            // Pattern B: explicit property
            // if (node.Declaration.IsVar)
            //     return true;

            // Pattern C: enum kind
            // if (node.Declaration.DeclarationKind == LocalDeclarationKind.Var)
            //     return true;

            return false;
        }
    }
}
