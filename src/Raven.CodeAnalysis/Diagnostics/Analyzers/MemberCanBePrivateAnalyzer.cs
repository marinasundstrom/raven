using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

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

        var isConsoleApplication = context.Compilation.Options.OutputKind == OutputKind.ConsoleApplication;
        var candidates = CollectCandidates(typeDecl, context.SemanticModel, isConsoleApplication);
        if (candidates.Count == 0)
            return;

        var candidateSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        foreach (var candidate in candidates)
            candidateSymbols.Add(candidate.Symbol.UnderlyingSymbol);

        var referencedOutside = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var referencedAnywhere = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        MarkReferences(
            context.Compilation,
            containingType,
            candidateSymbols,
            referencedOutside,
            referencedAnywhere,
            context.CancellationToken);

        foreach (var candidate in candidates)
        {
            var symbol = candidate.Symbol.UnderlyingSymbol;
            if (!referencedAnywhere.Contains(symbol))
                continue;

            if (referencedOutside.Contains(symbol))
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
        bool isConsoleApplication)
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

                        if (!CanBePrivate(methodSymbol, isConsoleApplication))
                            break;

                        candidates.Add(new Candidate(methodSymbol, methodDecl.Identifier.GetLocation()));
                        break;
                    }
                case EventDeclarationSyntax eventDecl:
                    {
                        if (semanticModel.GetDeclaredSymbol(eventDecl) is not IEventSymbol eventSymbol)
                            break;

                        if (!CanBePrivate(eventSymbol, isConsoleApplication))
                            break;

                        candidates.Add(new Candidate(eventSymbol, eventDecl.Identifier.GetLocation()));
                        break;
                    }
                case FieldDeclarationSyntax fieldDecl when fieldDecl.Declaration.Declarators.Count == 1:
                    {
                        var declarator = fieldDecl.Declaration.Declarators[0];
                        if (semanticModel.GetDeclaredSymbol(declarator) is not IFieldSymbol fieldSymbol)
                            break;

                        if (!CanBePrivate(fieldSymbol, isConsoleApplication))
                            break;

                        candidates.Add(new Candidate(fieldSymbol, declarator.Identifier.GetLocation()));
                        break;
                    }
            }
        }

        return candidates;
    }

    private static bool CanBePrivate(ISymbol symbol, bool isConsoleApplication)
    {
        symbol = symbol.UnderlyingSymbol;

        if (symbol.IsImplicitlyDeclared ||
            symbol.DeclaredAccessibility is Accessibility.Private or Accessibility.NotApplicable ||
            symbol.ContainingType is null ||
            symbol.ContainingType.TypeKind is not (TypeKind.Class or TypeKind.Struct))
        {
            return false;
        }

        if (!isConsoleApplication && symbol.DeclaredAccessibility is not Accessibility.Internal)
            return false;

        if (ImplementsInterfaceMember(symbol))
            return false;

        return symbol switch
        {
            IMethodSymbol method => method.MethodKind == MethodKind.Ordinary &&
                                    !method.IsAbstract &&
                                    !method.IsVirtual &&
                                    !method.IsOverride &&
                                    method.ExplicitInterfaceImplementations.IsDefaultOrEmpty,
            IEventSymbol @event => @event.ExplicitInterfaceImplementations.IsDefaultOrEmpty,
            IFieldSymbol => true,
            _ => false
        };
    }

    private static bool ImplementsInterfaceMember(ISymbol symbol)
    {
        if (symbol.ContainingType is null)
            return false;

        foreach (var interfaceType in symbol.ContainingType.AllInterfaces)
        {
            foreach (var interfaceMember in interfaceType.GetMembers(symbol.Name))
            {
                if (IsMatchingInterfaceMember(symbol, interfaceMember))
                    return true;
            }
        }

        return false;
    }

    private static bool IsMatchingInterfaceMember(ISymbol symbol, ISymbol interfaceMember)
    {
        return (symbol, interfaceMember) switch
        {
            (IMethodSymbol method, IMethodSymbol ifaceMethod) => method.MethodKind == MethodKind.Ordinary &&
                ifaceMethod.MethodKind == MethodKind.Ordinary &&
                HasSameParameters(method.Parameters, ifaceMethod.Parameters) &&
                SymbolEqualityComparer.Default.Equals(method.ReturnType, ifaceMethod.ReturnType),

            (IEventSymbol @event, IEventSymbol ifaceEvent) =>
                SymbolEqualityComparer.Default.Equals(@event.Type, ifaceEvent.Type),

            _ => false
        };
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

    private static void MarkReferences(
        Compilation compilation,
        INamedTypeSymbol containingType,
        HashSet<ISymbol> candidateSymbols,
        HashSet<ISymbol> referencedOutside,
        HashSet<ISymbol> referencedAnywhere,
        CancellationToken cancellationToken)
    {
        var candidateNames = candidateSymbols
            .Select(static symbol => symbol.Name)
            .ToHashSet(StringComparer.Ordinal);

        foreach (var tree in compilation.SyntaxTrees)
        {
            var model = compilation.GetSemanticModel(tree);
            var root = tree.GetRoot(cancellationToken);

            foreach (var node in root.DescendantNodesAndSelf())
            {
                if (node is not (IdentifierNameSyntax or MemberAccessExpressionSyntax or InvocationExpressionSyntax))
                    continue;

                if (!CanReferenceCandidate(node, candidateNames))
                    continue;

                ISymbol? referenced = null;
                try
                {
                    referenced = node switch
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

                if (referenced is null || !candidateSymbols.Contains(referenced))
                    continue;

                referencedAnywhere.Add(referenced);

                if (!IsInsideContainingType(node, model, containingType))
                    referencedOutside.Add(referenced);
            }
        }
    }

    private static bool CanReferenceCandidate(SyntaxNode node, HashSet<string> candidateNames)
        => node switch
        {
            IdentifierNameSyntax identifier => candidateNames.Contains(identifier.Identifier.ValueText),
            MemberAccessExpressionSyntax { Name: IdentifierNameSyntax name } => candidateNames.Contains(name.Identifier.ValueText),
            InvocationExpressionSyntax { Expression: IdentifierNameSyntax identifier } => candidateNames.Contains(identifier.Identifier.ValueText),
            InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax { Name: IdentifierNameSyntax name } } => candidateNames.Contains(name.Identifier.ValueText),
            _ => false
        };

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
