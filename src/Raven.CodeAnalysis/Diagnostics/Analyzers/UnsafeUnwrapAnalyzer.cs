using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnsafeUnwrapAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9037";

    private static readonly HashSet<string> UnsafeMethodNames = new(StringComparer.Ordinal)
    {
        "Expect",
        "Unwrap",
        "UnwrapError",
        "UnwrapOrThrow",
    };

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Unsafe Optional or Result extraction",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Extraction with '{0}' can fail when {1} is {2}. Handle {3} explicitly.",
        category: "ErrorHandling",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Descriptor];

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterOperationAction(AnalyzeInvocation, OperationKind.Invocation);
        context.RegisterSyntaxNodeAction(AnalyzeCarrierUnwrap, SyntaxKind.SuppressNullableWarningExpression);
    }

    private static void AnalyzeCarrierUnwrap(SyntaxNodeAnalysisContext context)
    {
        var expression = (PostfixOperatorExpressionSyntax)context.Node;
        var operandType = context.SemanticModel.GetTypeInfo(expression.Expression).Type;
        if (operandType is null || operandType.IsNullable ||
            !TryGetReceiverKind(operandType, out var receiverKind) ||
            context.SemanticModel.GetOperation(expression)?.Kind != OperationKind.BlockExpression)
        {
            return;
        }

        var failureCase = operandType is INamedTypeSymbol named && ResultFacts.TryFindFailureCaseType(named, out var failureType)
            ? failureType.Name : ResultFacts.FailureCaseName;
        context.ReportDiagnostic(Diagnostic.Create(
            Descriptor,
            expression.OperatorToken.GetLocation(),
            "!",
            receiverKind,
            GetThrowingState("!", receiverKind, failureCase),
            receiverKind == ReceiverKind.Optional ? "Some and None" : $"{ResultFacts.OkCaseName} and {failureCase}"));
    }

    private static void AnalyzeInvocation(OperationAnalysisContext context)
    {
        if (context.Operation is not IInvocationOperation invocation ||
            !UnsafeMethodNames.Contains(invocation.TargetMethod.Name) ||
            !TryGetReceiverKind(invocation.TargetMethod, out var receiverKind))
        {
            return;
        }

        var location = invocation.Syntax switch
        {
            InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax memberAccess }
                => memberAccess.Name.GetLocation(),
            InvocationExpressionSyntax invocationSyntax => invocationSyntax.Expression.GetLocation(),
            _ => invocation.Syntax.GetLocation(),
        };

        var receiverType = invocation.TargetMethod.IsExtensionMethod && invocation.TargetMethod.Parameters.Length > 0
            ? invocation.TargetMethod.Parameters[0].Type : invocation.TargetMethod.ContainingType;
        var failureCase = receiverType is INamedTypeSymbol named && ResultFacts.TryFindFailureCaseType(named, out var failureType)
            ? failureType.Name : ResultFacts.FailureCaseName;

        context.ReportDiagnostic(Diagnostic.Create(
            Descriptor,
            location,
            invocation.TargetMethod.Name,
            receiverKind,
            GetThrowingState(invocation.TargetMethod.Name, receiverKind, failureCase),
            receiverKind == ReceiverKind.Optional ? "Some and None" : $"{ResultFacts.OkCaseName} and {failureCase}"));
    }

    private static string GetThrowingState(string methodName, ReceiverKind receiverKind, string failureCase)
        => receiverKind == ReceiverKind.Result && methodName == "UnwrapError" ? ResultFacts.OkCaseName :
            receiverKind == ReceiverKind.Optional ? OptionalFacts.NoneCaseName : failureCase;

    private static bool TryGetReceiverKind(IMethodSymbol method, out ReceiverKind receiverKind)
    {
        if (!method.IsStatic)
            return TryGetReceiverKind(method.ContainingType, out receiverKind);

        if (method.IsExtensionMethod && method.Parameters.Length > 0)
            return TryGetReceiverKind(method.Parameters[0].Type, out receiverKind);

        receiverKind = ReceiverKind.None;
        return false;
    }

    private static bool TryGetReceiverKind(ITypeSymbol? type, out ReceiverKind receiverKind)
    {
        if (type is not INamedTypeSymbol namedType)
        {
            receiverKind = ReceiverKind.None;
            return false;
        }

        var definition = namedType.OriginalDefinition as INamedTypeSymbol ?? namedType;
        if (!string.Equals(definition.ContainingNamespace?.ToMetadataName(), "System", StringComparison.Ordinal))
        {
            receiverKind = ReceiverKind.None;
            return false;
        }

        receiverKind = (definition.Name, definition.Arity) switch
        {
            (OptionalFacts.TypeName, 1) => ReceiverKind.Optional,
            ("Result", 2) => ReceiverKind.Result,
            _ => ReceiverKind.None,
        };
        return receiverKind != ReceiverKind.None;
    }

    private enum ReceiverKind
    {
        None,
        Optional,
        Result,
    }
}
