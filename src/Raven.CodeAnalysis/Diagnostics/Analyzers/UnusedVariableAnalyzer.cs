using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedVariableAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9027";
    public const string UnusedParameterDiagnosticId = "RAV9030";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Value is never used",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Value '{0}' is never used.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    private static readonly DiagnosticDescriptor ParameterDescriptor = DiagnosticDescriptor.Create(
        id: UnusedParameterDiagnosticId,
        title: "Parameter is never used",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Parameter '{0}' is never used.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(
            AnalyzeBodyOwner,
            SyntaxKind.MethodDeclaration,
            SyntaxKind.FunctionStatement,
            SyntaxKind.ConstructorDeclaration,
            SyntaxKind.OperatorDeclaration,
            SyntaxKind.ConversionOperatorDeclaration,
            SyntaxKind.SimpleFunctionExpression,
            SyntaxKind.ParenthesizedFunctionExpression);
        context.RegisterSyntaxNodeAction(
            AnalyzeCompilationUnit,
            SyntaxKind.CompilationUnit);
    }

    private static void AnalyzeBodyOwner(SyntaxNodeAnalysisContext context)
    {
        if (context.SemanticModel.GetDocumentDiagnostics(context.CancellationToken).Any(d => d.Severity == DiagnosticSeverity.Error))
            return;

        var body = context.Node switch
        {
            MethodDeclarationSyntax m => (SyntaxNode?)m.Body ?? m.ExpressionBody?.Expression,
            FunctionStatementSyntax f => (SyntaxNode?)f.Body ?? f.ExpressionBody?.Expression,
            BaseMethodDeclarationSyntax m => (SyntaxNode?)m.Body ?? m.ExpressionBody?.Expression,
            FunctionExpressionSyntax f => (SyntaxNode?)f.Body ?? f.ExpressionBody?.Expression,
            _ => null
        };

        if (body is null)
            return;

        var collector = new CandidateCollector(context.SemanticModel);
        collector.CollectParameters(context.Node);
        collector.Visit(body);
        ReportDiagnostics(context, collector.GetCandidates(), GetUsedSymbols(context.SemanticModel, body));
    }

    private static void AnalyzeCompilationUnit(SyntaxNodeAnalysisContext context)
    {
        if (context.SemanticModel.GetDocumentDiagnostics(context.CancellationToken).Any(d => d.Severity == DiagnosticSeverity.Error))
            return;

        if (context.Node is not CompilationUnitSyntax compilationUnit)
            return;

        var collector = new CandidateCollector(context.SemanticModel);
        var usedSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

        foreach (var member in compilationUnit.Members.OfType<GlobalStatementSyntax>())
        {
            collector.Visit(member.Statement);
            UnionWith(usedSymbols, GetUsedSymbols(context.SemanticModel, member.Statement));
        }

        ReportDiagnostics(context, collector.GetCandidates(), usedSymbols);
    }

    private static void ReportDiagnostics(
        SyntaxNodeAnalysisContext context,
        IEnumerable<Candidate> candidates,
        HashSet<ISymbol> usedSymbols)
    {
        foreach (var candidate in candidates)
        {
            if (usedSymbols.Contains(candidate.Symbol) ||
                context.SemanticModel.IsCapturedVariable(candidate.Symbol))
            {
                continue;
            }

            context.ReportDiagnostic(Diagnostic.Create(
                candidate.Descriptor,
                candidate.Location,
                candidate.Name));
        }
    }

    private readonly struct Candidate
    {
        public Candidate(ISymbol symbol, string name, Location location, DiagnosticDescriptor descriptor)
        {
            Symbol = symbol;
            Name = name;
            Location = location;
            Descriptor = descriptor;
        }

        public ISymbol Symbol { get; }
        public string Name { get; }
        public Location Location { get; }
        public DiagnosticDescriptor Descriptor { get; }
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

        public void CollectParameters(SyntaxNode node)
        {
            IEnumerable<ParameterSyntax> parameters = node switch
            {
                BaseMethodDeclarationSyntax method => method.ParameterList.Parameters,
                FunctionStatementSyntax function => function.ParameterList.Parameters,
                SimpleFunctionExpressionSyntax function => [function.Parameter],
                ParenthesizedFunctionExpressionSyntax function => function.ParameterList.Parameters,
                _ => []
            };

            foreach (var parameter in parameters)
                CollectParameter(parameter);
        }

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
                    if (declarator.Initializer?.Value is FunctionExpressionSyntax)
                        continue;

                    if (_semanticModel.GetDeclaredSymbol(declarator) is not ILocalSymbol local)
                        continue;

                    if (string.IsNullOrEmpty(local.Name) || local.Name == "_")
                        continue;

                    _candidates[local.UnderlyingSymbol] = new Candidate(local.UnderlyingSymbol, local.Name, declarator.Identifier.GetLocation(), Descriptor);
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
                _candidates[local.UnderlyingSymbol] = new Candidate(local.UnderlyingSymbol, local.Name, node.Identifier.GetLocation(), Descriptor);
            }

            base.VisitSingleVariableDesignation(node);
        }

        private void CollectParameter(ParameterSyntax parameter)
        {
            if (_semanticModel.GetDeclaredSymbol(parameter) is not IParameterSymbol parameterSymbol)
                return;

            if (string.IsNullOrEmpty(parameterSymbol.Name) || parameterSymbol.Name == "_")
                return;

            _candidates[parameterSymbol.UnderlyingSymbol] = new Candidate(
                parameterSymbol.UnderlyingSymbol,
                parameterSymbol.Name,
                parameter.Identifier.GetLocation(),
                ParameterDescriptor);
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

    private static HashSet<ISymbol> GetUsedSymbols(SemanticModel semanticModel, SyntaxNode body)
    {
        var collector = new UsageCollector(semanticModel);
        collector.Visit(body);
        return collector.GetUsedSymbols();
    }

    private static void UnionWith(HashSet<ISymbol> target, IEnumerable<ISymbol> source)
    {
        foreach (var symbol in source)
            target.Add(symbol);
    }

    private sealed class UsageCollector : SyntaxWalker
    {
        private readonly SemanticModel _semanticModel;
        private readonly HashSet<ISymbol> _usedSymbols = new(SymbolEqualityComparer.Default);

        public UsageCollector(SemanticModel semanticModel)
        {
            _semanticModel = semanticModel;
        }

        public HashSet<ISymbol> GetUsedSymbols() => _usedSymbols;

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

        public override void VisitInterpolatedStringExpression(InterpolatedStringExpressionSyntax node)
        {
            foreach (var content in node.Contents)
                Visit(content);
        }

        public override void VisitInterpolation(InterpolationSyntax node)
        {
            TryMarkUsedLocal(node.Expression);
            base.VisitInterpolation(node);
        }

        private void TryMarkUsedLocal(SyntaxNode node)
        {
            if (IsWriteTarget(node))
                return;

            if (node is IdentifierNameSyntax identifier &&
                CanReferenceLocal(identifier))
            {
                if (_semanticModel.TryLookupVisibleValueSymbol(identifier) is { } visibleSymbol &&
                    TryGetLocalOrParameter(visibleSymbol, out var visibleUsedSymbol))
                {
                    _usedSymbols.Add(visibleUsedSymbol.UnderlyingSymbol);
                    return;
                }

                if (_semanticModel.GetBinder(identifier).LookupSymbol(identifier.Identifier.ValueText) is { } lookedUpSymbol &&
                    TryGetLocalOrParameter(lookedUpSymbol, out var lookedUpUsedSymbol))
                {
                    _usedSymbols.Add(lookedUpUsedSymbol.UnderlyingSymbol);
                    return;
                }
            }

            if (_semanticModel.GetSymbolInfo(node).Symbol?.UnderlyingSymbol is { } symbol &&
                TryGetLocalOrParameter(symbol, out var usedSymbol))
            {
                _usedSymbols.Add(usedSymbol.UnderlyingSymbol);
            }
        }

        private static bool TryGetLocalOrParameter(ISymbol symbol, out ISymbol usedSymbol)
        {
            usedSymbol = symbol.UnderlyingSymbol;
            return usedSymbol switch
            {
                ILocalSymbol local => !string.IsNullOrEmpty(local.Name),
                IParameterSymbol parameter => !string.IsNullOrEmpty(parameter.Name),
                _ => false
            };
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
