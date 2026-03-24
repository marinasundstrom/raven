using System;
using System.Collections.Immutable;
using System.Collections.Generic;
using System.Linq;

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
        context.RegisterSyntaxNodeAction(
            AnalyzeTypeDeclaration,
            SyntaxKind.ClassDeclaration,
            SyntaxKind.StructDeclaration);
        context.RegisterSyntaxNodeAction(
            AnalyzeBodyOwner,
            SyntaxKind.MethodDeclaration);
        context.RegisterSyntaxNodeAction(
            AnalyzeCompilationUnit,
            SyntaxKind.CompilationUnit);
    }

    private static void AnalyzeTypeDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.SemanticModel.GetDiagnostics(context.CancellationToken).Any(d => d.Severity == DiagnosticSeverity.Error))
            return;

        if (context.Node is not TypeDeclarationSyntax typeDecl)
            return;

        var includePublic = context.Compilation.Options.OutputKind == OutputKind.ConsoleApplication;
        var entryPoint = context.Compilation.GetEntryPoint()?.UnderlyingSymbol;
        var candidates = CollectCandidates(typeDecl, context.SemanticModel, entryPoint, includePublic);
        if (candidates.Count == 0)
            return;

        var candidateSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        foreach (var candidate in candidates)
            candidateSymbols.Add(candidate.Symbol.UnderlyingSymbol);

        var invoked = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        MarkInvokedMethods(context.Compilation, candidateSymbols, invoked, context.CancellationToken);

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
        if (context.SemanticModel.GetDiagnostics(context.CancellationToken).Any(d => d.Severity == DiagnosticSeverity.Error))
            return;

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
        if (context.SemanticModel.GetDiagnostics(context.CancellationToken).Any(d => d.Severity == DiagnosticSeverity.Error))
            return;

        if (context.Node is not CompilationUnitSyntax compilationUnit)
            return;

        foreach (var member in compilationUnit.Members.OfType<GlobalStatementSyntax>())
            AnalyzeLocalFunctions(context, member.Statement);
    }

    private static List<Candidate> CollectCandidates(
        TypeDeclarationSyntax typeDecl,
        SemanticModel semanticModel,
        ISymbol? entryPoint,
        bool includePublic)
    {
        var candidates = new List<Candidate>();

        foreach (var member in typeDecl.Members)
        {
            if (member is not MethodDeclarationSyntax methodDecl)
                continue;

            if (semanticModel.GetDeclaredSymbol(methodDecl) is not IMethodSymbol methodSymbol)
                continue;

            if (!CanReport(methodSymbol, entryPoint, includePublic))
                continue;

            candidates.Add(new Candidate(methodSymbol, methodDecl.Identifier.GetLocation()));
        }

        return candidates;
    }

    private static bool CanReport(IMethodSymbol method, ISymbol? entryPoint, bool includePublic)
    {
        var symbol = method.UnderlyingSymbol;

        if (symbol.IsImplicitlyDeclared ||
            SymbolEqualityComparer.Default.Equals(symbol, entryPoint) ||
            (!includePublic && symbol.DeclaredAccessibility == Accessibility.Public) ||
            method.MethodKind != MethodKind.Ordinary ||
            method.IsAbstract ||
            method.IsVirtual ||
            method.IsOverride ||
            method.IsExtern ||
            !method.ExplicitInterfaceImplementations.IsDefaultOrEmpty ||
            ImplementsInterfaceMember(method))
        {
            return false;
        }

        return true;
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

    private static bool ImplementsInterfaceMember(IMethodSymbol method)
    {
        if (method.ContainingType is null)
            return false;

        foreach (var interfaceType in method.ContainingType.AllInterfaces)
        {
            foreach (var interfaceMember in interfaceType.GetMembers(method.Name).OfType<IMethodSymbol>())
            {
                if (interfaceMember.MethodKind != MethodKind.Ordinary)
                    continue;

                if (!HasSameSignature(method, interfaceMember))
                    continue;

                return true;
            }
        }

        return false;
    }

    private static bool HasSameSignature(IMethodSymbol candidate, IMethodSymbol contract)
    {
        if (!SymbolEqualityComparer.Default.Equals(candidate.ReturnType, contract.ReturnType))
            return false;

        return HasSameParameters(candidate.Parameters, contract.Parameters);
    }

    private static bool HasSameParameters(
        ImmutableArray<IParameterSymbol> left,
        ImmutableArray<IParameterSymbol> right)
    {
        if (left.Length != right.Length)
            return false;

        for (var i = 0; i < left.Length; i++)
        {
            if (left[i].RefKind != right[i].RefKind)
                return false;

            if (!SymbolEqualityComparer.Default.Equals(left[i].Type, right[i].Type))
                return false;
        }

        return true;
    }

    private static void MarkInvokedMethods(
        Compilation compilation,
        HashSet<ISymbol> candidateSymbols,
        HashSet<ISymbol> invoked,
        CancellationToken cancellationToken)
    {
        foreach (var tree in compilation.SyntaxTrees)
        {
            var model = compilation.GetSemanticModel(tree);
            var root = tree.GetRoot(cancellationToken);

            foreach (var invocation in root.DescendantNodesAndSelf().OfType<InvocationExpressionSyntax>())
            {
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
