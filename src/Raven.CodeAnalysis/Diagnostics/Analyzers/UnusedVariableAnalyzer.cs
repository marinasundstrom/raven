using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedVariableAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9027";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Variable is never used",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Variable '{0}' is never used.",
        category: "Usage",
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
        if (context.SemanticModel.GetDiagnostics(context.CancellationToken).Any(d => d.Severity == DiagnosticSeverity.Error))
            return;

        var body = context.Node switch
        {
            MethodDeclarationSyntax m => (SyntaxNode?)m.Body ?? m.ExpressionBody?.Expression,
            FunctionStatementSyntax f => (SyntaxNode?)f.Body ?? f.ExpressionBody?.Expression,
            _ => null
        };

        if (body is null)
            return;

        var collector = new CandidateCollector(context.SemanticModel);
        collector.Visit(body);
        ReportDiagnostics(context, collector.GetCandidates(), GetUsedLocals(context.SemanticModel, body));
    }

    private static void AnalyzeCompilationUnit(SyntaxNodeAnalysisContext context)
    {
        if (context.SemanticModel.GetDiagnostics(context.CancellationToken).Any(d => d.Severity == DiagnosticSeverity.Error))
            return;

        if (context.Node is not CompilationUnitSyntax compilationUnit)
            return;

        var collector = new CandidateCollector(context.SemanticModel);
        var usedLocals = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

        foreach (var member in compilationUnit.Members.OfType<GlobalStatementSyntax>())
        {
            collector.Visit(member.Statement);
            UnionWith(usedLocals, GetUsedLocals(context.SemanticModel, member.Statement));
        }

        ReportDiagnostics(context, collector.GetCandidates(), usedLocals);
    }

    private static void ReportDiagnostics(
        SyntaxNodeAnalysisContext context,
        IEnumerable<Candidate> candidates,
        HashSet<ISymbol> usedLocals)
    {
        foreach (var candidate in candidates)
        {
            if (usedLocals.Contains(candidate.Symbol) ||
                context.SemanticModel.IsCapturedVariable(candidate.Symbol))
            {
                continue;
            }

            context.ReportDiagnostic(Diagnostic.Create(
                Descriptor,
                candidate.Location,
                candidate.Name));
        }
    }

    private readonly struct Candidate
    {
        public Candidate(ISymbol symbol, string name, Location location)
        {
            Symbol = symbol;
            Name = name;
            Location = location;
        }

        public ISymbol Symbol { get; }
        public string Name { get; }
        public Location Location { get; }
    }

    private sealed class CandidateCollector : SyntaxWalker
    {
        private readonly SemanticModel _semanticModel;
        private readonly Dictionary<ISymbol, Candidate> _candidates = new(SymbolEqualityComparer.Default);
        private int _closureDepth;

        public CandidateCollector(SemanticModel semanticModel)
        {
            _semanticModel = semanticModel;
        }

        public IEnumerable<Candidate> GetCandidates() => _candidates.Values;

        public override void DefaultVisit(SyntaxNode node)
        {
            foreach (var child in node.ChildNodes())
                Visit(child);
        }

        public override void VisitFunctionStatement(FunctionStatementSyntax node)
        {
            EnterClosure();
            VisitMaybe(node.Body);
            VisitMaybe(node.ExpressionBody);
            ExitClosure();
        }

        public override void VisitFunctionExpression(FunctionExpressionSyntax node)
        {
            EnterClosure();
            VisitMaybe(node.ExpressionBody);
            ExitClosure();
        }

        public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            if (!IsInClosure)
            {
                foreach (var declarator in node.Declaration.Declarators)
                {
                    if (_semanticModel.GetDeclaredSymbol(declarator) is not ILocalSymbol local)
                        continue;

                    if (string.IsNullOrEmpty(local.Name) || local.Name == "_")
                        continue;

                    _candidates[local.UnderlyingSymbol] = new Candidate(local.UnderlyingSymbol, local.Name, declarator.Identifier.GetLocation());
                }
            }

            base.VisitLocalDeclarationStatement(node);
        }

        public override void VisitSingleVariableDesignation(SingleVariableDesignationSyntax node)
        {
            if (!IsInClosure &&
                _semanticModel.GetDeclaredSymbol(node) is ILocalSymbol local &&
                !string.IsNullOrEmpty(local.Name) &&
                local.Name != "_")
            {
                _candidates[local.UnderlyingSymbol] = new Candidate(local.UnderlyingSymbol, local.Name, node.Identifier.GetLocation());
            }

            base.VisitSingleVariableDesignation(node);
        }

        private bool IsInClosure => _closureDepth > 0;

        private void EnterClosure() => _closureDepth++;

        private void ExitClosure() => _closureDepth--;

        private void VisitMaybe(SyntaxNode? node)
        {
            if (node is not null)
                Visit(node);
        }
    }

    private static HashSet<ISymbol> GetUsedLocals(SemanticModel semanticModel, SyntaxNode body)
    {
        var collector = new UsageCollector(semanticModel);
        collector.Visit(body);
        return collector.GetUsedLocals();
    }

    private static void UnionWith(HashSet<ISymbol> target, IEnumerable<ISymbol> source)
    {
        foreach (var symbol in source)
            target.Add(symbol);
    }

    private sealed class UsageCollector : SyntaxWalker
    {
        private readonly SemanticModel _semanticModel;
        private readonly HashSet<ISymbol> _usedLocals = new(SymbolEqualityComparer.Default);

        public UsageCollector(SemanticModel semanticModel)
        {
            _semanticModel = semanticModel;
        }

        public HashSet<ISymbol> GetUsedLocals() => _usedLocals;

        public override void DefaultVisit(SyntaxNode node)
        {
            foreach (var child in node.ChildNodes())
                Visit(child);
        }

        public override void VisitIdentifierName(IdentifierNameSyntax node)
        {
            if (!CanReferenceLocal(node))
                return;

            TryMarkUsedLocal(node);
        }

        public override void VisitDeclarationPattern(DeclarationPatternSyntax node)
        {
            TryMarkUsedLocal(node);
            base.VisitDeclarationPattern(node);
        }

        public override void VisitVariablePattern(VariablePatternSyntax node)
        {
            TryMarkUsedLocal(node);
            base.VisitVariablePattern(node);
        }

        private void TryMarkUsedLocal(SyntaxNode node)
        {
            if (IsWriteTarget(node))
                return;

            if (_semanticModel.GetSymbolInfo(node).Symbol?.UnderlyingSymbol is ILocalSymbol local &&
                !string.IsNullOrEmpty(local.Name))
            {
                _usedLocals.Add(local.UnderlyingSymbol);
            }
        }

        private static bool IsWriteTarget(SyntaxNode node)
        {
            for (SyntaxNode? current = node; current is not null; current = current.Parent)
            {
                switch (current.Parent)
                {
                    case AssignmentStatementSyntax assignmentStatement when assignmentStatement.Left == current:
                    case AssignmentExpressionSyntax assignmentExpression when assignmentExpression.Left == current:
                        return true;
                }
            }

            return false;
        }

        private static bool CanReferenceLocal(IdentifierNameSyntax node)
        {
            return node.Parent switch
            {
                MemberAccessExpressionSyntax memberAccess when ReferenceEquals(memberAccess.Name, node) => false,
                QualifiedNameSyntax => false,
                TypeAnnotationClauseSyntax => false,
                TypeArgumentSyntax => false,
                ImportDirectiveSyntax => false,
                AttributeSyntax => false,
                NameColonSyntax => false,
                _ => true,
            };
        }
    }
}
