using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports stored properties that are never initialized in a constructor or with an initializer.
/// </summary>
public sealed class UninitializedPropertyAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9006";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Property is not initialized",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "'{0}' needs an initial value. Set it in an initializer or in a constructor.",
        category: "Initialization",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.EnableConcurrentExecution();
        context.RegisterSymbolAction(AnalyzePropertySymbol, SymbolKind.Property);
    }

    private static void AnalyzePropertySymbol(SymbolAnalysisContext context)
    {
        if (context.Symbol is not SourcePropertySymbol propertySymbol ||
            !IsCandidate(propertySymbol))
        {
            return;
        }

        foreach (var reference in propertySymbol.DeclaringSyntaxReferences)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            if (reference.GetSyntax(context.CancellationToken) is not PropertyDeclarationSyntax propertyDecl)
                continue;

            if (HasExplicitGetAccessorImplementation(propertyDecl))
                continue;

            if (propertyDecl.FirstAncestorOrSelf<TypeDeclarationSyntax>() is not { } typeDeclaration)
                continue;

            var semanticModel = context.Compilation.GetSemanticModel(reference.SyntaxTree);
            if (MemberInitializationAnalysis.IsAssignedInInstanceInitialization(
                semanticModel,
                typeDeclaration,
                propertySymbol))
            {
                continue;
            }

            var location = propertyDecl.Identifier.GetLocation();
            context.ReportDiagnostic(Diagnostic.Create(Descriptor, location, propertySymbol.Name));
        }
    }

    private static bool IsCandidate(SourcePropertySymbol propertySymbol)
        => !propertySymbol.IsRequired &&
           !propertySymbol.IsStatic &&
           (propertySymbol.IsAutoProperty || propertySymbol.EmitAsFieldOnly) &&
           propertySymbol.BackingField?.Initializer is null &&
           propertySymbol.DeclaredAccessibility != Accessibility.NotApplicable;

    private static bool HasExplicitGetAccessorImplementation(PropertyDeclarationSyntax propertyDecl)
        => propertyDecl.ExpressionBody is not null ||
           propertyDecl.AccessorList?.Accessors.Any(static accessor =>
               accessor.Kind == SyntaxKind.GetAccessorDeclaration &&
               (accessor.Body is not null || accessor.ExpressionBody is not null)) == true;
}
