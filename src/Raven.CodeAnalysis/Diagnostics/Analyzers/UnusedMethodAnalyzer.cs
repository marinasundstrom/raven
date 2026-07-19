using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedMethodAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9019";
    public const string UnnecessaryDiagnosticProperty = "Unnecessary";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Method is never invoked",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Method '{0}' is never invoked.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.EnableConcurrentExecution();

        context.RegisterCompilationAction(AnalyzeCompilation);
        context.RegisterSyntaxNodeAction(
            AnalyzeBodyOwner,
            SyntaxKind.MethodDeclaration);
        context.RegisterSyntaxNodeAction(
            AnalyzeCompilationUnit,
            SyntaxKind.CompilationUnit);
    }

    private static void AnalyzeCompilation(CompilationAnalysisContext context)
    {
        var includePublic = context.Compilation.Options.OutputKind != OutputKind.DynamicallyLinkedLibrary;
        var entryPointSymbols = GetEntryPointSymbols(context.Compilation);
        var candidates = CollectCandidates(
            context.Compilation,
            entryPointSymbols,
            includePublic,
            context.SyntaxTree,
            context.CancellationToken);
        if (candidates.Count == 0)
            return;

        var candidateSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var candidateNames = new HashSet<string>(StringComparer.Ordinal);
        foreach (var candidate in candidates)
        {
            candidateSymbols.Add(candidate.Symbol.UnderlyingSymbol);
            candidateNames.Add(candidate.Symbol.Name);
        }

        var invoked = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        MarkInvokedMethods(context.Compilation, candidateSymbols, candidateNames, invoked, context.CancellationToken);

        foreach (var candidate in candidates)
        {
            if (invoked.Contains(candidate.Symbol.UnderlyingSymbol))
                continue;

            context.ReportDiagnostic(Diagnostic.Create(
                Descriptor,
                candidate.Location,
                candidate.Symbol.Name));
        }
    }

    private static void AnalyzeBodyOwner(SyntaxNodeAnalysisContext context)
    {
        var body = context.Node switch
        {
            MethodDeclarationSyntax m => (SyntaxNode?)m.Body ?? m.ExpressionBody?.Expression,
            _ => null
        };

        if (body is null)
            return;

        AnalyzeLocalFunctions(context, body);
    }

    private static void AnalyzeCompilationUnit(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not CompilationUnitSyntax compilationUnit)
            return;

        foreach (var member in compilationUnit.Members.OfType<GlobalStatementSyntax>())
            AnalyzeLocalFunctions(context, member.Statement);
    }

    private static List<Candidate> CollectCandidates(
        Compilation compilation,
        HashSet<ISymbol> entryPointSymbols,
        bool includePublic,
        SyntaxTree? targetTree,
        CancellationToken cancellationToken)
    {
        var candidates = new List<Candidate>();

        var syntaxTrees = targetTree is null ? compilation.SyntaxTrees : [targetTree];
        foreach (var tree in syntaxTrees)
        {
            var semanticModel = compilation.GetSemanticModel(tree);
            var root = tree.GetRoot(cancellationToken);

            foreach (var typeDecl in root.DescendantNodesAndSelf().OfType<TypeDeclarationSyntax>())
            {
                cancellationToken.ThrowIfCancellationRequested();

                if (typeDecl.Kind is not (SyntaxKind.ClassDeclaration or SyntaxKind.StructDeclaration))
                    continue;

                foreach (var member in typeDecl.Members)
                {
                    if (member is not MethodDeclarationSyntax methodDecl)
                        continue;

                    if (semanticModel.GetDeclaredSymbol(methodDecl) is not IMethodSymbol methodSymbol)
                        continue;

                    if (!CanReport(methodSymbol, entryPointSymbols, includePublic))
                        continue;

                    candidates.Add(new Candidate(methodSymbol, methodDecl.Identifier.GetLocation()));
                }
            }
        }

        return candidates;
    }

    private static bool CanReport(IMethodSymbol method, HashSet<ISymbol> entryPointSymbols, bool includePublic)
    {
        var symbol = method.UnderlyingSymbol;

        if (symbol.IsImplicitlyDeclared ||
            entryPointSymbols.Contains(symbol) ||
            (!includePublic && symbol.DeclaredAccessibility == Accessibility.Public) ||
            method.MethodKind != MethodKind.Ordinary ||
            method.IsAbstract ||
            method.IsVirtual ||
            method.IsExtern ||
            AnalyzerContractFacts.IsContractMethod(method))
        {
            return false;
        }

        return true;
    }

    private static HashSet<ISymbol> GetEntryPointSymbols(Compilation compilation)
    {
        var symbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var entryPoint = compilation.GetEntryPoint();
        if (entryPoint is null)
            return symbols;

        symbols.Add(entryPoint.UnderlyingSymbol);

        if (entryPoint is SynthesizedEntryPointBridgeMethodSymbol bridge)
            symbols.Add(bridge.AsyncImplementation.UnderlyingSymbol);

        return symbols;
    }

    private static void AnalyzeLocalFunctions(SyntaxNodeAnalysisContext context, SyntaxNode body)
    {
        var candidates = CollectLocalFunctionCandidates(body, context.SemanticModel);
        if (candidates.Count == 0)
            return;

        var candidateSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        foreach (var candidate in candidates)
            candidateSymbols.Add(candidate.Symbol.UnderlyingSymbol);

        var invoked = GetInvokedMethods(context.SemanticModel, body, candidateSymbols);
        foreach (var candidate in candidates)
        {
            if (invoked.Contains(candidate.Symbol.UnderlyingSymbol))
                continue;

            context.ReportDiagnostic(new Diagnostic(
                Descriptor,
                candidate.Location,
                [candidate.Symbol.Name],
                properties: ImmutableDictionary<string, string?>.Empty.Add(UnnecessaryDiagnosticProperty, bool.TrueString)));
        }
    }

    private static List<Candidate> CollectLocalFunctionCandidates(
        SyntaxNode root,
        SemanticModel semanticModel)
    {
        var candidates = new List<Candidate>();

        foreach (var function in root.DescendantNodesAndSelf().OfType<FunctionStatementSyntax>())
        {
            if (!IsLocalFunction(function))
                continue;

            if (semanticModel.GetDeclaredSymbol(function) is not IMethodSymbol methodSymbol)
                continue;

            if (methodSymbol.MethodKind != MethodKind.Ordinary || methodSymbol.IsExtern)
                continue;

            candidates.Add(new Candidate(methodSymbol, function.Identifier.GetLocation()));
        }

        return candidates;
    }

    private static bool IsLocalFunction(FunctionStatementSyntax function)
    {
        for (var current = function.Parent; current is not null; current = current.Parent)
        {
            if (current is GlobalStatementSyntax)
                return false;

            if (current is MethodDeclarationSyntax or FunctionStatementSyntax)
                return true;
        }

        return false;
    }

    private static void MarkInvokedMethods(
        Compilation compilation,
        HashSet<ISymbol> candidateSymbols,
        HashSet<string> candidateNames,
        HashSet<ISymbol> invoked,
        CancellationToken cancellationToken)
    {
        foreach (var tree in compilation.SyntaxTrees)
        {
            var model = compilation.GetSemanticModel(tree);
            var root = tree.GetRoot(cancellationToken);

            foreach (var invocation in root.DescendantNodesAndSelf().OfType<InvocationExpressionSyntax>())
            {
                if (!TryGetInvokedSourceName(invocation.Expression, out var invokedName) ||
                    !candidateNames.Contains(invokedName))
                {
                    continue;
                }

                ISymbol? symbol = null;
                try
                {
                    symbol = model.GetSymbolInfo(invocation.Expression).Symbol?.UnderlyingSymbol;
                }
                catch (OperationCanceledException)
                {
                    throw;
                }
                catch
                {
                    continue;
                }

                if (symbol is null || !candidateSymbols.Contains(symbol))
                    continue;

                invoked.Add(symbol);
            }
        }
    }

    private static bool TryGetInvokedSourceName(ExpressionSyntax expression, out string name)
    {
        switch (expression)
        {
            case SimpleNameSyntax simpleName:
                name = simpleName.Identifier.ValueText;
                return true;
            case MemberAccessExpressionSyntax memberAccess:
                name = memberAccess.Name.Identifier.ValueText;
                return true;
            default:
                name = string.Empty;
                return false;
        }
    }

    private static HashSet<ISymbol> GetInvokedMethods(
        SemanticModel semanticModel,
        SyntaxNode root,
        HashSet<ISymbol> candidateSymbols)
    {
        var invoked = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

        foreach (var invocation in root.DescendantNodesAndSelf().OfType<InvocationExpressionSyntax>())
        {
            ISymbol? symbol = null;
            try
            {
                symbol = semanticModel.GetSymbolInfo(invocation.Expression).Symbol?.UnderlyingSymbol;
            }
            catch
            {
                continue;
            }

            if (symbol is not null && candidateSymbols.Contains(symbol))
                invoked.Add(symbol);
        }

        return invoked;
    }

    private readonly struct Candidate
    {
        public Candidate(ISymbol symbol, Location location)
        {
            Symbol = symbol;
            Location = location;
        }

        public ISymbol Symbol { get; }
        public Location Location { get; }
    }
}
