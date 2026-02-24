using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedMethodAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9019";

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
            !method.ExplicitInterfaceImplementations.IsDefaultOrEmpty)
        {
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
