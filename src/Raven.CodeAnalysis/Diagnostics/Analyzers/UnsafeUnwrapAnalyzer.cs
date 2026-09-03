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
        messageFormat: "Call to '{0}' can throw for a valid Option or Result state. Handle both cases explicitly.",
        category: "ErrorHandling",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Descriptor];

    public override void Initialize(AnalysisContext context)
        => context.RegisterOperationAction(AnalyzeInvocation, OperationKind.Invocation);

    private static void AnalyzeInvocation(OperationAnalysisContext context)
    {
        if (context.Operation is not IInvocationOperation invocation ||
            !UnsafeMethodNames.Contains(invocation.TargetMethod.Name) ||
            !HasOptionOrResultReceiver(invocation.TargetMethod))
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
            invocation.TargetMethod.Name));
    }

    private static bool HasOptionOrResultReceiver(IMethodSymbol method)
    {
        if (!method.IsStatic)
            return IsOptionOrResult(method.ContainingType);

        return method.IsExtensionMethod &&
               method.Parameters.Length > 0 &&
               IsOptionOrResult(method.Parameters[0].Type);
    }

    private static bool IsOptionOrResult(ITypeSymbol? type)
    {
        if (type is not INamedTypeSymbol namedType)
            return false;

        var definition = namedType.OriginalDefinition as INamedTypeSymbol ?? namedType;
        if (!string.Equals(definition.ContainingNamespace?.ToMetadataName(), "System", StringComparison.Ordinal))
            return false;

        return (definition.Name == "Option" && definition.Arity == 1) ||
               (definition.Name == "Result" && definition.Arity == 2);
    }
}
