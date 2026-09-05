using System.Collections.Immutable;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnawaitedTaskAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9038";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Task-returning call is not awaited",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Task-returning call is not awaited; use 'await' or assign the task to '_' to discard it explicitly.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Descriptor];

    public override void Initialize(AnalysisContext context)
        => context.RegisterOperationAction(AnalyzeExpressionStatement, OperationKind.ExpressionStatement);

    private static void AnalyzeExpressionStatement(OperationAnalysisContext context)
    {
        if (context.Operation is not IExpressionStatementOperation
            {
                Syntax: ExpressionStatementSyntax statement,
                Operation: { } operation
            } ||
            !IsTaskType(operation.Type) ||
            !IsInvocation(operation) ||
            UnusedExpressionResultAnalyzer.IsImplicitValueReturnTarget(
                statement, context.SemanticModel, useAsyncResultType: true))
        {
            return;
        }

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, operation.Syntax.GetLocation()));
    }

    private static bool IsInvocation(IOperation operation)
        => operation switch
        {
            IInvocationOperation => true,
            IParenthesizedOperation { Operand: { } operand } => IsInvocation(operand),
            IConversionOperation { Operand: { } operand } => IsInvocation(operand),
            IConditionalAccessOperation { WhenNotNull: { } whenNotNull } => IsInvocation(whenNotNull),
            _ => false,
        };

    private static bool IsTaskType(ITypeSymbol? type)
    {
        type = type?.GetNonNullableType();
        if (type is null || type.TypeKind == TypeKind.Error)
            return false;

        if (AsyncReturnTypeUtilities.IsNonGenericValueTask(type) ||
            AsyncReturnTypeUtilities.IsGenericValueTask(type))
        {
            return true;
        }

        for (var current = type as INamedTypeSymbol; current is not null; current = current.BaseType)
        {
            if (current.OriginalDefinition.SpecialType is
                SpecialType.System_Threading_Tasks_Task or SpecialType.System_Threading_Tasks_Task_T)
            {
                return true;
            }
        }

        return false;
    }
}
