using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedPropertyAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9018";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Property is never used",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Property '{0}' is never used.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterCompilationAction(AnalyzeCompilation);
    }

    private static void AnalyzeCompilation(CompilationAnalysisContext context)
    {
        var includePublic = context.Compilation.Options.OutputKind == OutputKind.ConsoleApplication;
        var candidates = CollectCandidates(context.Compilation, includePublic, context.SyntaxTree, context.CancellationToken);
        if (candidates.Count == 0)
            return;

        var candidateSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        foreach (var candidate in candidates)
            candidateSymbols.Add(candidate.Symbol.UnderlyingSymbol);

        var referenced = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        MarkReferences(context.Compilation, candidateSymbols, referenced, context.CancellationToken);

        foreach (var candidate in candidates)
        {
            if (referenced.Contains(candidate.Symbol.UnderlyingSymbol))
                continue;

            context.ReportDiagnostic(Diagnostic.Create(
                Descriptor,
                candidate.Location,
                candidate.Symbol.Name));
        }
    }

    private static List<Candidate> CollectCandidates(
        Compilation compilation,
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
                    if (member is not PropertyDeclarationSyntax propertyDecl)
                        continue;

                    if (semanticModel.GetDeclaredSymbol(propertyDecl) is not IPropertySymbol propertySymbol)
                        continue;

                    if (propertySymbol.IsImplicitlyDeclared ||
                        propertySymbol.IsIndexer ||
                        (!includePublic && propertySymbol.DeclaredAccessibility == Accessibility.Public) ||
                        AnalyzerContractFacts.IsContractProperty(propertySymbol))
                    {
                        continue;
                    }

                    candidates.Add(new Candidate(propertySymbol, propertyDecl.Identifier.GetLocation()));
                }
            }
        }

        return candidates;
    }

    private static void MarkReferences(
        Compilation compilation,
        HashSet<ISymbol> candidateSymbols,
        HashSet<ISymbol> referenced,
        CancellationToken cancellationToken)
    {
        foreach (var tree in compilation.SyntaxTrees)
        {
            var model = compilation.GetSemanticModel(tree);
            var root = tree.GetRoot(cancellationToken);

            foreach (var node in root.DescendantNodesAndSelf())
            {
                if (node is not (IdentifierNameSyntax or MemberAccessExpressionSyntax or InvocationExpressionSyntax))
                    continue;

                ISymbol? symbol = null;
                try
                {
                    symbol = node switch
                    {
                        InvocationExpressionSyntax invocation => model.GetSymbolInfo(invocation.Expression).Symbol?.UnderlyingSymbol,
                        _ => model.GetSymbolInfo(node).Symbol?.UnderlyingSymbol
                    };
                }
                catch (OperationCanceledException)
                {
                    throw;
                }
                catch
                {
                    continue;
                }

                if (symbol is null)
                    continue;

                // Direct hit: the resolved symbol is itself a candidate property.
                if (candidateSymbols.Contains(symbol))
                {
                    referenced.Add(symbol);
                    continue;
                }
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
