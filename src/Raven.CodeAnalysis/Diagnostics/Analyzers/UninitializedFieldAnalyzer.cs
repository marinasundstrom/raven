using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports private explicit fields that are never initialized in a constructor or with an initializer.
/// </summary>
public sealed class UninitializedFieldAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9032";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Field is not initialized",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "'{0}' needs an initial value. Set it in an initializer or in a constructor.",
        category: "Initialization",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.EnableConcurrentExecution();
        context.RegisterSymbolAction(AnalyzeFieldSymbol, SymbolKind.Field);
    }

    private static void AnalyzeFieldSymbol(SymbolAnalysisContext context)
    {
        if (context.Symbol is not SourceFieldSymbol fieldSymbol ||
            !IsCandidate(fieldSymbol))
        {
            return;
        }

        foreach (var reference in fieldSymbol.DeclaringSyntaxReferences)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            if (reference.GetSyntax(context.CancellationToken) is not VariableDeclaratorSyntax declarator ||
                declarator.Parent?.Parent is not FieldDeclarationSyntax)
            {
                continue;
            }

            if (declarator.FirstAncestorOrSelf<TypeDeclarationSyntax>() is not { } typeDeclaration)
                continue;

            var semanticModel = context.Compilation.GetSemanticModel(reference.SyntaxTree);
            if (MemberInitializationAnalysis.IsAssignedInInstanceInitialization(
                semanticModel,
                typeDeclaration,
                fieldSymbol))
            {
                continue;
            }

            context.ReportDiagnostic(Diagnostic.Create(
                Descriptor,
                declarator.Identifier.GetLocation(),
                fieldSymbol.Name));
        }
    }

    private static bool IsCandidate(SourceFieldSymbol fieldSymbol)
        => fieldSymbol.AssociatedSymbol is null &&
           !fieldSymbol.IsRequired &&
           !fieldSymbol.IsStatic &&
           !fieldSymbol.IsConst &&
           fieldSymbol.Initializer is null &&
           fieldSymbol.DeclaredAccessibility == Accessibility.Private;
}
