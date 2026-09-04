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
        title: "Unsafe Option or Result extraction",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Call to '{0}' can throw when {1} is {2}. Handle {3} explicitly.",
        category: "ErrorHandling",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Descriptor];

    public override void Initialize(AnalysisContext context)
        => context.RegisterOperationAction(AnalyzeInvocation, OperationKind.Invocation);

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

        context.ReportDiagnostic(Diagnostic.Create(
            Descriptor,
            location,
            invocation.TargetMethod.Name,
            receiverKind,
            GetThrowingState(invocation.TargetMethod.Name, receiverKind),
            receiverKind == ReceiverKind.Option ? "Some and None" : "Ok and Error"));
    }

    private static string GetThrowingState(string methodName, ReceiverKind receiverKind)
        => receiverKind == ReceiverKind.Result && methodName == "UnwrapError" ? "Ok" :
            receiverKind == ReceiverKind.Option ? "None" : "Error";

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
            ("Option", 1) => ReceiverKind.Option,
            ("Result", 2) => ReceiverKind.Result,
            _ => ReceiverKind.None,
        };
        return receiverKind != ReceiverKind.None;
    }

    private enum ReceiverKind
    {
        None,
        Option,
        Result,
    }
}
