using System.Collections.Generic;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class MemberCanBePrivateAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9016";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Member can be private",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "'{0}' can be made private.",
        category: "Design",
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
        if (context.Node is not TypeDeclarationSyntax typeDecl)
            return;

        if (context.SemanticModel.GetDeclaredSymbol(typeDecl) is not INamedTypeSymbol containingType)
            return;

        var candidates = CollectCandidates(typeDecl, context.SemanticModel);
        if (candidates.Count == 0)
            return;

        var candidateSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        foreach (var candidate in candidates)
            candidateSymbols.Add(candidate.Symbol.UnderlyingSymbol);

        var referencedOutside = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        MarkReferencedOutsideContainingType(
            context.Compilation,
            containingType,
            candidateSymbols,
            referencedOutside,
            context.CancellationToken);

        foreach (var candidate in candidates)
        {
            if (referencedOutside.Contains(candidate.Symbol.UnderlyingSymbol))
                continue;

            context.ReportDiagnostic(Diagnostic.Create(
                Descriptor,
                candidate.Location,
                candidate.Symbol.Name));
        }
    }

    private static List<Candidate> CollectCandidates(TypeDeclarationSyntax typeDecl, SemanticModel semanticModel)
    {
        var candidates = new List<Candidate>();

        foreach (var member in typeDecl.Members)
        {
            switch (member)
            {
                case MethodDeclarationSyntax methodDecl:
                {
                    if (semanticModel.GetDeclaredSymbol(methodDecl) is not IMethodSymbol methodSymbol)
                        break;

                    if (!CanBePrivate(methodSymbol))
                        break;

                    candidates.Add(new Candidate(methodSymbol, methodDecl.Identifier.GetLocation()));
                    break;
                }
                case PropertyDeclarationSyntax propertyDecl:
                {
                    if (semanticModel.GetDeclaredSymbol(propertyDecl) is not IPropertySymbol propertySymbol)
                        break;

                    if (!CanBePrivate(propertySymbol))
                        break;

                    candidates.Add(new Candidate(propertySymbol, propertyDecl.Identifier.GetLocation()));
                    break;
                }
                case EventDeclarationSyntax eventDecl:
                {
                    if (semanticModel.GetDeclaredSymbol(eventDecl) is not IEventSymbol eventSymbol)
                        break;

                    if (!CanBePrivate(eventSymbol))
                        break;

                    candidates.Add(new Candidate(eventSymbol, eventDecl.Identifier.GetLocation()));
                    break;
                }
                case FieldDeclarationSyntax fieldDecl when fieldDecl.Declaration.Declarators.Count == 1:
                {
                    var declarator = fieldDecl.Declaration.Declarators[0];
                    if (semanticModel.GetDeclaredSymbol(declarator) is not IFieldSymbol fieldSymbol)
                        break;

                    if (!CanBePrivate(fieldSymbol))
                        break;

                    candidates.Add(new Candidate(fieldSymbol, declarator.Identifier.GetLocation()));
                    break;
                }
            }
        }

        return candidates;
    }

    private static bool CanBePrivate(ISymbol symbol)
    {
        symbol = symbol.UnderlyingSymbol;

        if (symbol.IsImplicitlyDeclared ||
            symbol.DeclaredAccessibility is Accessibility.Private or Accessibility.NotApplicable ||
            symbol.ContainingType is null ||
            symbol.ContainingType.TypeKind is not (TypeKind.Class or TypeKind.Struct))
        {
            return false;
        }

        return symbol switch
        {
            IMethodSymbol method => method.MethodKind == MethodKind.Ordinary &&
                                    !method.IsAbstract &&
                                    !method.IsVirtual &&
                                    !method.IsOverride &&
                                    method.ExplicitInterfaceImplementations.IsDefaultOrEmpty,
            IPropertySymbol property => !property.IsIndexer &&
                                        property.ExplicitInterfaceImplementations.IsDefaultOrEmpty,
            IEventSymbol @event => @event.ExplicitInterfaceImplementations.IsDefaultOrEmpty,
            IFieldSymbol => true,
            _ => false
        };
    }

    private static void MarkReferencedOutsideContainingType(
        Compilation compilation,
        INamedTypeSymbol containingType,
        HashSet<ISymbol> candidateSymbols,
        HashSet<ISymbol> referencedOutside,
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

                var referenced = model.GetSymbolInfo(node).Symbol?.UnderlyingSymbol;
                if (referenced is null || !candidateSymbols.Contains(referenced))
                    continue;

                if (!IsInsideContainingType(node, model, containingType))
                    referencedOutside.Add(referenced);
            }
        }
    }

    private static bool IsInsideContainingType(SyntaxNode node, SemanticModel model, INamedTypeSymbol containingType)
    {
        var ownerTypeSyntax = node.FirstAncestorOrSelf<TypeDeclarationSyntax>();
        if (ownerTypeSyntax is null)
            return false;

        var ownerType = model.GetDeclaredSymbol(ownerTypeSyntax);
        return ownerType is not null && SymbolEqualityComparer.Default.Equals(ownerType, containingType);
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
