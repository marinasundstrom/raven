using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedLocalAnalyzer : UnusedVariableAnalyzerBase
{
    public const string DiagnosticId = UnusedVariableAnalyzer.DiagnosticId;

    public UnusedLocalAnalyzer()
        : base(reportLocals: true, reportParameters: false)
    {
    }
}

public sealed class UnusedParameterAnalyzer : UnusedVariableAnalyzerBase
{
    public const string DiagnosticId = UnusedVariableAnalyzer.UnusedParameterDiagnosticId;

    public UnusedParameterAnalyzer()
        : base(reportLocals: false, reportParameters: true)
    {
    }
}

public sealed class UnusedVariableAnalyzer : UnusedVariableAnalyzerBase
{
    public const string DiagnosticId = "RAV9027";
    public const string UnusedParameterDiagnosticId = "RAV9030";

    public UnusedVariableAnalyzer()
        : base(reportLocals: true, reportParameters: true)
    {
    }
}

public abstract class UnusedVariableAnalyzerBase : DiagnosticAnalyzer
{
    protected static readonly DiagnosticDescriptor LocalDescriptor = DiagnosticDescriptor.Create(
        id: UnusedVariableAnalyzer.DiagnosticId,
        title: "Value is never used",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Value '{0}' is never used.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    protected static readonly DiagnosticDescriptor ParameterDescriptor = DiagnosticDescriptor.Create(
        id: UnusedVariableAnalyzer.UnusedParameterDiagnosticId,
        title: "Parameter is never used",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Parameter '{0}' is never used.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    private readonly bool _reportLocals;
    private readonly bool _reportParameters;

    protected UnusedVariableAnalyzerBase(bool reportLocals, bool reportParameters)
    {
        _reportLocals = reportLocals;
        _reportParameters = reportParameters;
    }

    public override void Initialize(AnalysisContext context)
    {
        context.EnableConcurrentExecution();

        if (_reportLocals)
        {
            context.RegisterSyntaxNodeAction(
                AnalyzeCompilationUnit,
                SyntaxKind.CompilationUnit);

            context.RegisterSyntaxNodeAction(
                AnalyzeLocalOwner,
                SyntaxKind.MethodDeclaration,
                SyntaxKind.FunctionStatement,
                SyntaxKind.ConstructorDeclaration,
                SyntaxKind.OperatorDeclaration,
                SyntaxKind.ConversionOperatorDeclaration,
                SyntaxKind.SimpleFunctionExpression,
                SyntaxKind.ParenthesizedFunctionExpression);
        }

        if (_reportParameters)
            context.RegisterSymbolAction(AnalyzeMethodSymbol, SymbolKind.Method);
    }

    private void AnalyzeLocalOwner(SyntaxNodeAnalysisContext context)
        => AnalyzeLocalOwner(context, context.Node);

    private void AnalyzeLocalOwner(SyntaxNodeAnalysisContext context, SyntaxNode owner)
    {
        if (HasBlockingSyntaxDiagnostics(context))
            return;

        var bodyRoots = GetBodyRoots(owner).ToArray();
        var usageRoots = GetUsageRoots(owner).ToArray();

        if (bodyRoots.Length == 0 && usageRoots.Length == 0)
            return;

        var facts = OwnerUsageCollector.Collect(context.SemanticModel, owner, collectLocals: true, collectParameters: false);
        if (!facts.IsSemanticallyComplete)
            return;

        var candidates = facts.Candidates.ToArray();
        var usedSymbols = facts.UsedSymbols;

        if (owner is ConstructorDeclarationSyntax { Initializer: not null } constructor)
            MarkConstructorInitializerParameterReferences(candidates, constructor.Initializer, usedSymbols);

        ReportDiagnostics(context.ReportDiagnostic, context.SemanticModel, candidates, usedSymbols);
    }

    private void AnalyzeMethodSymbol(SymbolAnalysisContext context)
    {
        if (context.Symbol is not IMethodSymbol method ||
            AnalyzerContractFacts.IsContractMethod(method))
        {
            return;
        }

        foreach (var reference in method.DeclaringSyntaxReferences)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            var owner = reference.GetSyntax(context.CancellationToken);
            var syntaxTree = reference.SyntaxTree;
            if (syntaxTree is null)
                continue;

            if (HasBlockingSyntaxDiagnostics(syntaxTree, context.CancellationToken))
                continue;

            var semanticModel = context.Compilation.GetSemanticModel(syntaxTree);
            var bodyRoots = GetBodyRoots(owner).ToArray();
            var usageRoots = GetUsageRoots(owner).ToArray();
            if (bodyRoots.Length == 0 && usageRoots.Length == 0)
                continue;

            var facts = OwnerUsageCollector.Collect(semanticModel, owner, collectLocals: false, collectParameters: true);
            if (!facts.IsSemanticallyComplete)
                continue;

            var candidates = facts.Candidates.ToArray();
            var usedSymbols = facts.UsedSymbols;
            if (owner is ConstructorDeclarationSyntax { Initializer: not null } constructor)
                MarkConstructorInitializerParameterReferences(candidates, constructor.Initializer, usedSymbols);

            ReportDiagnostics(context.ReportDiagnostic, semanticModel, candidates, usedSymbols);
        }
    }

    private static void MarkConstructorInitializerParameterReferences(
        IEnumerable<Candidate> candidates,
        ConstructorInitializerSyntax initializer,
        HashSet<ISymbol> usedSymbols)
    {
        var parametersByName = candidates
            .Where(candidate => candidate.Descriptor.Id == UnusedVariableAnalyzer.UnusedParameterDiagnosticId)
            .ToDictionary(candidate => candidate.Name, candidate => candidate.Symbol, StringComparer.Ordinal);

        if (parametersByName.Count == 0)
            return;

        foreach (var identifier in initializer.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>())
        {
            if (parametersByName.TryGetValue(identifier.Identifier.ValueText, out var symbol))
                usedSymbols.Add(symbol.UnderlyingSymbol);
        }

        foreach (var argument in initializer.ArgumentList.Arguments)
        {
            if (parametersByName.TryGetValue(argument.Expression.ToString(), out var symbol))
                usedSymbols.Add(symbol.UnderlyingSymbol);
        }
    }

    private static IEnumerable<SyntaxNode> GetBodyRoots(SyntaxNode node)
    {
        var body = node switch
        {
            MethodDeclarationSyntax method => (SyntaxNode?)method.Body ?? method.ExpressionBody?.Expression,
            FunctionStatementSyntax function => (SyntaxNode?)function.Body ?? function.ExpressionBody?.Expression,
            BaseMethodDeclarationSyntax method => (SyntaxNode?)method.Body ?? method.ExpressionBody?.Expression,
            FunctionExpressionSyntax function => (SyntaxNode?)function.Body ?? function.ExpressionBody?.Expression,
            _ => null
        };

        if (body is not null)
            yield return body;
    }

    private static IEnumerable<SyntaxNode> GetUsageRoots(SyntaxNode node)
    {
        if (node is ConstructorDeclarationSyntax { Initializer: not null } constructor)
            yield return constructor.Initializer;

        foreach (var bodyRoot in GetBodyRoots(node))
            yield return bodyRoot;
    }

    private void AnalyzeCompilationUnit(SyntaxNodeAnalysisContext context)
    {
        if (HasBlockingSyntaxDiagnostics(context))
            return;

        if (context.Node is not CompilationUnitSyntax compilationUnit)
            return;

        if (_reportLocals)
        {
            var facts = OwnerUsageCollector.CollectGlobalStatements(
                context.SemanticModel,
                compilationUnit.Members.OfType<GlobalStatementSyntax>());

            if (facts.IsSemanticallyComplete)
                ReportDiagnostics(context.ReportDiagnostic, context.SemanticModel, facts.Candidates, facts.UsedSymbols);
        }
    }

    private static bool HasBlockingSyntaxDiagnostics(SyntaxNodeAnalysisContext context)
    {
        var syntaxTree = context.Node.SyntaxTree;
        if (syntaxTree is null)
            return false;

        return HasBlockingSyntaxDiagnostics(syntaxTree, context.CancellationToken);
    }

    private static bool HasBlockingSyntaxDiagnostics(SyntaxTree syntaxTree, CancellationToken cancellationToken)
    {
        return syntaxTree.GetDiagnostics(cancellationToken)
            .Any(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    private static void ReportDiagnostics(
        Action<Diagnostic> reportDiagnostic,
        SemanticModel semanticModel,
        IEnumerable<Candidate> candidates,
        HashSet<ISymbol> usedSymbols)
    {
        foreach (var candidate in candidates)
        {
            if (ContainsSymbol(usedSymbols, candidate.Symbol) ||
                semanticModel.IsCapturedVariable(candidate.Symbol))
            {
                continue;
            }

            reportDiagnostic(Diagnostic.Create(
                candidate.Descriptor,
                candidate.Location,
                candidate.Name));
        }
    }

    private static bool ContainsSymbol(IEnumerable<ISymbol> symbols, ISymbol candidate)
        => symbols.Any(symbol => SymbolEqualityComparer.Default.Equals(symbol, candidate));

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

    private sealed class OwnerUsageCollector : SyntaxWalker
    {
        private readonly SemanticModel _semanticModel;
        private readonly bool _collectLocals;
        private readonly Dictionary<ISymbol, Candidate> _candidates = new(SymbolEqualityComparer.Default);
        private readonly HashSet<ISymbol> _usedSymbols = new(SymbolEqualityComparer.Default);
        private int _nestedOwnerDepth;
        private bool _isSemanticallyComplete = true;

        private OwnerUsageCollector(SemanticModel semanticModel, bool collectLocals)
        {
            _semanticModel = semanticModel;
            _collectLocals = collectLocals;
        }

        public static OwnerUsageFacts Collect(
            SemanticModel semanticModel,
            SyntaxNode owner,
            bool collectLocals,
            bool collectParameters)
        {
            var collector = new OwnerUsageCollector(semanticModel, collectLocals);

            if (collectParameters)
                collector.CollectParameters(owner);

            foreach (var bodyRoot in GetUsageRoots(owner))
                collector.Visit(bodyRoot);

            return collector.ToFacts();
        }

        public static OwnerUsageFacts CollectGlobalStatements(
            SemanticModel semanticModel,
            IEnumerable<GlobalStatementSyntax> globalStatements)
        {
            var collector = new OwnerUsageCollector(semanticModel, collectLocals: true);

            foreach (var member in globalStatements)
                collector.Visit(member.Statement);

            return collector.ToFacts();
        }

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

        private OwnerUsageFacts ToFacts()
            => new(_candidates.Values.ToArray(), _usedSymbols, _isSemanticallyComplete);

        public override void DefaultVisit(SyntaxNode node)
        {
            foreach (var child in node.ChildNodes())
                Visit(child);
        }

        public override void VisitFunctionStatement(FunctionStatementSyntax node)
        {
            EnterNestedOwner();
            VisitMaybe(node.Body);
            VisitMaybe(node.ExpressionBody);
            ExitNestedOwner();
        }

        public override void VisitFunctionExpression(FunctionExpressionSyntax node)
        {
            EnterNestedOwner();
            VisitMaybe(node.ExpressionBody);
            ExitNestedOwner();
        }

        public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            if (_collectLocals && !IsInNestedOwner)
            {
                foreach (var declarator in node.Declaration.Declarators)
                {
                    if (declarator.Initializer?.Value is FunctionExpressionSyntax)
                        continue;

                    if (_semanticModel.GetDeclaredSymbol(declarator) is not ILocalSymbol local)
                        continue;

                    if (string.IsNullOrEmpty(local.Name) || local.Name == "_")
                        continue;

                    AddCandidate(local, local.Name, declarator.Identifier.GetLocation(), LocalDescriptor);
                }
            }

            base.VisitLocalDeclarationStatement(node);
        }

        public override void VisitSingleVariableDesignation(SingleVariableDesignationSyntax node)
        {
            if (_collectLocals &&
                !IsInNestedOwner &&
                _semanticModel.GetDeclaredSymbol(node) is ILocalSymbol local &&
                !string.IsNullOrEmpty(local.Name) &&
                local.Name != "_")
            {
                AddCandidate(local, local.Name, node.Identifier.GetLocation(), LocalDescriptor);
            }

            base.VisitSingleVariableDesignation(node);
        }

        public override void VisitIdentifierName(IdentifierNameSyntax node)
        {
            if (!CanReferenceLocal(node))
                return;

            TryMarkUsedLocal(node);
        }

        public override void VisitAssignmentStatement(AssignmentStatementSyntax node)
        {
            VisitAssignmentTarget(node.Left);
            Visit(node.Right);
        }

        public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
        {
            VisitAssignmentTarget(node.Left);
            Visit(node.Right);
        }

        public override void VisitForStatement(ForStatementSyntax node)
        {
            VisitMaybe(node.Target);
            Visit(node.Expression);
            VisitMaybe(node.StepExpression);
            Visit(node.Body);
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

        private void CollectParameter(ParameterSyntax parameter)
        {
            if (_semanticModel.GetDeclaredSymbol(parameter) is not IParameterSymbol parameterSymbol)
                return;

            if (string.IsNullOrEmpty(parameterSymbol.Name) || parameterSymbol.Name == "_")
                return;

            if (parameterSymbol.ContainingSymbol is IMethodSymbol method &&
                AnalyzerContractFacts.IsContractMethod(method))
            {
                return;
            }

            AddCandidate(parameterSymbol, parameterSymbol.Name, parameter.Identifier.GetLocation(), ParameterDescriptor);
        }

        private void TryMarkUsedLocal(SyntaxNode node)
        {
            if (IsWriteTarget(node))
                return;

            var symbolInfo = _semanticModel.GetSymbolInfo(node);
            if (symbolInfo.Symbol?.UnderlyingSymbol is { } symbol)
            {
                if (symbol is IErrorSymbol)
                {
                    MarkIncompleteIfCandidateReference(node);
                    return;
                }

                if (TryGetLocalOrParameter(symbol, out var usedSymbol))
                {
                    AddUsedSymbol(usedSymbol);
                    TryMarkEquivalentCandidateByNameAndKind(node, usedSymbol);
                }
                else
                {
                    TryMarkCandidateByName(node);
                }

                return;
            }

            if (symbolInfo.CandidateReason != CandidateReason.None ||
                symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                MarkIncompleteIfCandidateReference(node);
            }
        }

        private void MarkIncompleteIfCandidateReference(SyntaxNode node)
        {
            if (node is IdentifierNameSyntax identifier &&
                _candidates.Values.Any(candidate => string.Equals(candidate.Name, identifier.Identifier.ValueText, StringComparison.Ordinal)))
            {
                _isSemanticallyComplete = false;
            }
        }

        private void TryMarkCandidateByName(SyntaxNode node)
        {
            if (node is not IdentifierNameSyntax identifier)
                return;

            foreach (var candidate in _candidates.Values)
            {
                if (string.Equals(candidate.Name, identifier.Identifier.ValueText, StringComparison.Ordinal))
                    AddUsedSymbol(candidate.Symbol);
            }
        }

        private void TryMarkEquivalentCandidateByNameAndKind(SyntaxNode node, ISymbol usedSymbol)
        {
            if (node is not IdentifierNameSyntax identifier)
                return;

            foreach (var candidate in _candidates.Values)
            {
                if (candidate.Symbol.Kind == usedSymbol.Kind &&
                    string.Equals(candidate.Name, identifier.Identifier.ValueText, StringComparison.Ordinal))
                {
                    AddUsedSymbol(candidate.Symbol);
                }
            }
        }

        private void AddCandidate(ISymbol symbol, string name, Location location, DiagnosticDescriptor descriptor)
        {
            var key = symbol.UnderlyingSymbol;
            _candidates[key] = new Candidate(key, name, location, descriptor);
        }

        private void AddUsedSymbol(ISymbol symbol)
        {
            // Semantic queries can return different symbol instances for the same declaration
            // after diagnostics or lazy binding. Keep all usage matching comparer-based.
            _usedSymbols.Add(symbol.UnderlyingSymbol);
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

        private bool IsInNestedOwner => _nestedOwnerDepth > 0;

        private void EnterNestedOwner() => _nestedOwnerDepth++;

        private void ExitNestedOwner() => _nestedOwnerDepth--;

        private void VisitMaybe(SyntaxNode? node)
        {
            if (node is not null)
                Visit(node);
        }

        private void VisitAssignmentTarget(ExpressionOrPatternSyntax target)
        {
            if (target is MemberAccessExpressionSyntax memberAccess)
                Visit(memberAccess.Expression);
        }

        private static bool IsWriteTarget(SyntaxNode node)
        {
            for (SyntaxNode? current = node; current is not null; current = current.Parent)
            {
                switch (current.Parent)
                {
                    case AssignmentStatementSyntax assignmentStatement when assignmentStatement.Left == current:
                        return !IsReceiverWithinAssignmentTarget(node, current);

                    case AssignmentExpressionSyntax assignmentExpression when assignmentExpression.Left == current:
                        return !IsReceiverWithinAssignmentTarget(node, current);
                }
            }

            return false;
        }

        private static bool IsReceiverWithinAssignmentTarget(SyntaxNode node, SyntaxNode assignmentTarget)
        {
            if (assignmentTarget is MemberAccessExpressionSyntax memberAccess &&
                ContainsNode(memberAccess.Expression, node))
            {
                return true;
            }

            return false;
        }

        private static bool ContainsNode(SyntaxNode root, SyntaxNode node)
        {
            return node.Span.Start >= root.Span.Start &&
                   node.Span.End <= root.Span.End;
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

    private readonly record struct OwnerUsageFacts(
        IReadOnlyCollection<Candidate> Candidates,
        HashSet<ISymbol> UsedSymbols,
        bool IsSemanticallyComplete);
}
