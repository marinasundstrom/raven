using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports auto-properties that are never initialized in a constructor or with an initializer.
/// </summary>
public sealed class AutoPropertyInitializationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9006";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Auto-property is not initialized",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Auto-property '{0}' is not initialized; assign it in a constructor or with an initializer",
        category: "Initialization",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxTreeAction(AnalyzeTree);
    }

    private static void AnalyzeTree(SyntaxTreeAnalysisContext context)
    {
        var semanticModel = context.Compilation.GetSemanticModel(context.SyntaxTree);
        var root = context.SyntaxTree.GetRoot();

        foreach (var classDecl in root.DescendantNodes().OfType<ClassDeclarationSyntax>())
        {
            var classSymbol = semanticModel.GetDeclaredSymbol(classDecl) as INamedTypeSymbol;
            if (classSymbol is null || classSymbol.TypeKind != TypeKind.Class)
                continue;

            var autoProperties = new Dictionary<IPropertySymbol, PropertyDeclarationSyntax>(SymbolEqualityComparer.Default);

            foreach (var propertyDecl in classDecl.Members.OfType<PropertyDeclarationSyntax>())
            {
                var propertySymbol = semanticModel.GetDeclaredSymbol(propertyDecl) as SourcePropertySymbol;
                if (propertySymbol is null || !propertySymbol.IsAutoProperty)
                    continue;

                if (propertySymbol.IsRequired || propertySymbol.IsStatic)
                    continue;

                if (propertyDecl.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword))
                    continue;

                if (propertyDecl.Initializer is not null)
                    continue;

                autoProperties[propertySymbol] = propertyDecl;
            }

            if (autoProperties.Count == 0)
                continue;

            var assigned = new HashSet<IPropertySymbol>(SymbolEqualityComparer.Default);

            foreach (var ctor in classDecl.Members.OfType<BaseConstructorDeclarationSyntax>())
            {
                if (ctor.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword))
                    continue;

                CollectAssignedProperties(semanticModel, ctor, autoProperties, assigned);
            }

            foreach (var (propertySymbol, propertyDecl) in autoProperties)
            {
                if (assigned.Contains(propertySymbol))
                    continue;

                var location = propertyDecl.Identifier.GetLocation();
                var diagnostic = Diagnostic.Create(Descriptor, location, propertySymbol.Name);
                context.ReportDiagnostic(diagnostic);
            }
        }
    }

    private static void CollectAssignedProperties(
        SemanticModel semanticModel,
        BaseConstructorDeclarationSyntax ctor,
        IReadOnlyDictionary<IPropertySymbol, PropertyDeclarationSyntax> autoProperties,
        HashSet<IPropertySymbol> assigned)
    {
        if (ctor.Body is not null)
            CollectAssignedProperties(semanticModel, ctor.Body, autoProperties, assigned);

        if (ctor.ExpressionBody is not null)
            CollectAssignedProperties(semanticModel, ctor.ExpressionBody.Expression, autoProperties, assigned);
    }

    private static void CollectAssignedProperties(
        SemanticModel semanticModel,
        SyntaxNode root,
        IReadOnlyDictionary<IPropertySymbol, PropertyDeclarationSyntax> autoProperties,
        HashSet<IPropertySymbol> assigned)
    {
        foreach (var assignment in root.DescendantNodesAndSelf().OfType<AssignmentStatementSyntax>())
            TryMarkAssigned(semanticModel, assignment.Left, autoProperties, assigned);

        foreach (var assignment in root.DescendantNodesAndSelf().OfType<AssignmentExpressionSyntax>())
            TryMarkAssigned(semanticModel, assignment.Left, autoProperties, assigned);
    }

    private static void TryMarkAssigned(
        SemanticModel semanticModel,
        ExpressionOrPatternSyntax left,
        IReadOnlyDictionary<IPropertySymbol, PropertyDeclarationSyntax> autoProperties,
        HashSet<IPropertySymbol> assigned)
    {
        var symbolInfo = semanticModel.GetSymbolInfo(left);
        if (symbolInfo.Symbol is not IPropertySymbol property)
            return;

        if (!autoProperties.ContainsKey(property))
            return;

        assigned.Add(property);
    }
}
