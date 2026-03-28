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
        messageFormat: "'{0}' is never reassigned. Use val {0}.",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(
            AnalyzeBodyOwner,
            SyntaxKind.MethodDeclaration,
            SyntaxKind.FunctionStatement);
        context.RegisterSyntaxNodeAction(
            AnalyzeCompilationUnit,
            SyntaxKind.CompilationUnit);
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

        ReportCandidates(context, body);
    }

    private static void AnalyzeCompilationUnit(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not CompilationUnitSyntax compilationUnit)
            return;

        var collector = new VarRebindingCollector(context.SemanticModel);

        foreach (var member in compilationUnit.Members)
        {
            if (member is not GlobalStatementSyntax globalStatement)
                continue;

            collector.Visit(globalStatement.Statement);
        }

        ReportCandidates(context, collector);
    }

    private static void ReportCandidates(SyntaxNodeAnalysisContext context, SyntaxNode node)
    {
        var collector = new VarRebindingCollector(context.SemanticModel);
        collector.Visit(node);
        ReportCandidates(context, collector);
    }

    private static void ReportCandidates(SyntaxNodeAnalysisContext context, VarRebindingCollector collector)
    {
        foreach (var candidate in collector.GetVarThatCanBeVal())
        {
            var diagnostic = Diagnostic.Create(
                Descriptor,
                candidate.BindingKeywordToken.GetLocation(),
                candidate.Name);

            context.ReportDiagnostic(diagnostic);
        }
    }

    private readonly struct VarCandidate
    {
        public readonly ILocalSymbol Local;
        public readonly string Name;
        public readonly SyntaxToken BindingKeywordToken;

        public VarCandidate(ILocalSymbol local, string name, SyntaxToken bindingKeywordToken)
        {
            Local = local;
            Name = name;
            BindingKeywordToken = bindingKeywordToken;
        }
    }

    private sealed class VarRebindingCollector : SyntaxWalker
    {
        private readonly SemanticModel _semanticModel;

        private readonly Dictionary<ILocalSymbol, VarCandidate> _varLocals =
            new(SymbolEqualityComparer.Default);

        private readonly HashSet<ILocalSymbol> _writtenAfterDeclaration =
            new(SymbolEqualityComparer.Default);

        private readonly HashSet<ILocalSymbol> _capturedByClosure =
            new(SymbolEqualityComparer.Default);

        private int _closureDepth;

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

                if (!_writtenAfterDeclaration.Contains(local) && !_capturedByClosure.Contains(local))
                    yield return candidate;
            }
        }

        private void EnterClosure() => _closureDepth++;

        private void ExitClosure() => _closureDepth--;

        private bool IsInClosure => _closureDepth > 0;

        public override void DefaultVisit(SyntaxNode node)
        {
            foreach (var child in node.ChildNodes())
                Visit(child);
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

        // --- Closures ------------------------------------------------------------

        public override void VisitFunctionStatement(FunctionStatementSyntax node)
        {
            // A nested function forms a closure boundary.
            EnterClosure();

            // Visit body/expression body explicitly to ensure we walk in closure mode.
            VisitMaybe(node.Body);
            VisitMaybe(node.ExpressionBody);

            ExitClosure();
        }

        public override void VisitSimpleFunctionExpression(SimpleFunctionExpressionSyntax node)
        {
            VisitFunctionExpressionCore(node);
        }

        public override void VisitParenthesizedFunctionExpression(ParenthesizedFunctionExpressionSyntax node)
        {
            VisitFunctionExpressionCore(node);
        }

        private void VisitFunctionExpressionCore(FunctionExpressionSyntax node)
        {
            // Lambda forms a closure boundary.
            EnterClosure();

            VisitMaybe(node.Body);
            VisitMaybe(node.ExpressionBody);

            ExitClosure();
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
                        _varLocals[local] = new VarCandidate(local, name, node.Declaration.BindingKeyword);
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

        public override void VisitPrefixOperatorExpression(PrefixOperatorExpressionSyntax node)
        {
            if (IsIncrementOrDecrement(node.OperatorToken))
                MarkWrittenFromExpression(node.Expression);

            // If your base walker does nothing, replace with explicit Visit(...) calls.
            base.VisitPrefixOperatorExpression(node);
        }

        public override void VisitPostfixOperatorExpression(PostfixOperatorExpressionSyntax node)
        {
            if (IsIncrementOrDecrement(node.OperatorToken))
                MarkWrittenFromExpression(node.Expression);

            base.VisitPostfixOperatorExpression(node);
        }

        public override void VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            Visit(node.Expression);

            var needsParameterResolution = false;
            for (var i = 0; i < node.ArgumentList.Arguments.Count; i++)
            {
                var argumentRefKind = node.ArgumentList.Arguments[i].RefKindKeyword.Kind;
                if (argumentRefKind is SyntaxKind.RefKeyword or SyntaxKind.OutKeyword or SyntaxKind.InKeyword)
                {
                    needsParameterResolution = true;
                    break;
                }
            }

            IMethodSymbol? method = null;
            if (needsParameterResolution)
            {
                var symbolInfo = _semanticModel.GetSymbolInfo(node);
                method = symbolInfo.Symbol as IMethodSymbol
                    ?? symbolInfo.CandidateSymbols.OfType<IMethodSymbol>().FirstOrDefault();
            }

            for (var i = 0; i < node.ArgumentList.Arguments.Count; i++)
            {
                var argument = node.ArgumentList.Arguments[i];
                var parameter = TryGetParameterForArgument(method, argument, i);

                if (parameter is not null &&
                    parameter.RefKind is RefKind.Out or RefKind.Ref or RefKind.RefReadOnlyParameter)
                {
                    MarkWrittenFromExpression(argument.Expression);
                }

                Visit(argument.Expression);
            }
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
            VisitMaybe(node.Target);
            VisitMaybe(node.Expression);
            VisitMaybe(node.StepExpression);

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

        // --- Capture detection ----------------------------------------------------

        public override void VisitIdentifierName(IdentifierNameSyntax node)
        {
            // Only treat name references inside closures as captures.
            if (IsInClosure)
            {
                var symbol = _semanticModel.GetSymbolInfo(node).Symbol;
                if (symbol is ILocalSymbol local)
                    _capturedByClosure.Add(local);
            }

            base.VisitIdentifierName(node);
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

        private static IParameterSymbol? TryGetParameterForArgument(
            IMethodSymbol? method,
            ArgumentSyntax argument,
            int position)
        {
            if (method is null || method.Parameters.IsDefaultOrEmpty)
                return null;

            if (argument.NameColon?.Name is IdentifierNameSyntax namedArgument)
            {
                var parameterName = namedArgument.Identifier.ValueText;
                foreach (var parameter in method.Parameters)
                {
                    if (parameter.Name == parameterName)
                        return parameter;
                }

                return null;
            }

            return position < method.Parameters.Length
                ? method.Parameters[position]
                : null;
        }

        private bool IsVarDeclaration(LocalDeclarationStatementSyntax node)
        {
            if (node.Declaration.BindingKeyword.Kind == SyntaxKind.VarKeyword)
                return true;

            return false;
        }
    }
}
