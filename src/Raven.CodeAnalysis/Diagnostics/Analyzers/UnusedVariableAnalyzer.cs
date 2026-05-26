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
        context.EnableConcurrentExecution();

        context.RegisterSyntaxNodeAction(
            AnalyzeCompilationUnit,
            SyntaxKind.CompilationUnit);
    }

    private static void AnalyzeBodyOwner(SyntaxNodeAnalysisContext context, SyntaxNode owner)
    {
        var bodyRoots = GetBodyRoots(owner).ToArray();
        var usageRoots = GetUsageRoots(owner).ToArray();

        if (bodyRoots.Length == 0 && usageRoots.Length == 0)
            return;

        var facts = OwnerUsageCollector.Collect(context.SemanticModel, owner);
        var candidates = facts.Candidates.ToArray();
        var usedSymbols = facts.UsedSymbols;

        if (owner is ConstructorDeclarationSyntax { Initializer: not null } constructor)
            MarkConstructorInitializerParameterReferences(candidates, constructor.Initializer, usedSymbols);

        ReportDiagnostics(context, candidates, usedSymbols);
    }

    private static void MarkConstructorInitializerParameterReferences(
        IEnumerable<Candidate> candidates,
        ConstructorInitializerSyntax initializer,
        HashSet<ISymbol> usedSymbols)
    {
        var parametersByName = candidates
            .Where(candidate => candidate.Descriptor.Id == UnusedParameterDiagnosticId)
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

        var facts = OwnerUsageCollector.CollectGlobalStatements(
            context.SemanticModel,
            compilationUnit.Members.OfType<GlobalStatementSyntax>());

        ReportDiagnostics(context, facts.Candidates, facts.UsedSymbols);

        foreach (var owner in compilationUnit.DescendantNodes().Where(IsBodyOwner))
            AnalyzeBodyOwner(context, owner);
    }

    private static bool IsBodyOwner(SyntaxNode node)
        => node.Kind is
            SyntaxKind.MethodDeclaration or
            SyntaxKind.FunctionStatement or
            SyntaxKind.ConstructorDeclaration or
            SyntaxKind.OperatorDeclaration or
            SyntaxKind.ConversionOperatorDeclaration or
            SyntaxKind.SimpleFunctionExpression or
            SyntaxKind.ParenthesizedFunctionExpression;

    private static bool HasBlockingSyntaxDiagnostics(SyntaxNodeAnalysisContext context)
    {
        var syntaxTree = context.Node.SyntaxTree;
        if (syntaxTree is null)
            return false;

        return syntaxTree.GetDiagnostics(context.CancellationToken)
            .Any(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
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

    private sealed class OwnerUsageCollector : SyntaxWalker
    {
        private readonly SemanticModel _semanticModel;
        private readonly Dictionary<ISymbol, Candidate> _candidates = new(SymbolEqualityComparer.Default);
        private readonly HashSet<ISymbol> _usedSymbols = new(SymbolEqualityComparer.Default);
        private int _nestedOwnerDepth;

        private OwnerUsageCollector(SemanticModel semanticModel)
        {
            _semanticModel = semanticModel;
        }

        public static OwnerUsageFacts Collect(SemanticModel semanticModel, SyntaxNode owner)
        {
            var collector = new OwnerUsageCollector(semanticModel);
            collector.CollectParameters(owner);

            foreach (var bodyRoot in GetUsageRoots(owner))
                collector.Visit(bodyRoot);

            return collector.ToFacts();
        }

        public static OwnerUsageFacts CollectGlobalStatements(
            SemanticModel semanticModel,
            IEnumerable<GlobalStatementSyntax> globalStatements)
        {
            var collector = new OwnerUsageCollector(semanticModel);

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
            => new(_candidates.Values.ToArray(), _usedSymbols);

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
            if (!IsInNestedOwner)
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
            if (!IsInNestedOwner &&
                _semanticModel.GetDeclaredSymbol(node) is ILocalSymbol local &&
                !string.IsNullOrEmpty(local.Name) &&
                local.Name != "_")
            {
                _candidates[local.UnderlyingSymbol] = new Candidate(local.UnderlyingSymbol, local.Name, node.Identifier.GetLocation(), Descriptor);
            }

            base.VisitSingleVariableDesignation(node);
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

            _candidates[parameterSymbol.UnderlyingSymbol] = new Candidate(
                parameterSymbol.UnderlyingSymbol,
                parameterSymbol.Name,
                parameter.Identifier.GetLocation(),
                ParameterDescriptor);
        }

        private void TryMarkUsedLocal(SyntaxNode node)
        {
            if (IsWriteTarget(node))
                return;

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

        private bool IsInNestedOwner => _nestedOwnerDepth > 0;

        private void EnterNestedOwner() => _nestedOwnerDepth++;

        private void ExitNestedOwner() => _nestedOwnerDepth--;

        private void VisitMaybe(SyntaxNode? node)
        {
            if (node is not null)
                Visit(node);
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
        HashSet<ISymbol> UsedSymbols);
}
