using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class ImmutableCollectionOperationResultAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9026";

    private static readonly HashSet<string> ImmutableCollectionTypeNames = new(StringComparer.Ordinal)
    {
        "ImmutableArray",
        "ImmutableList",
        "ImmutableDictionary",
        "ImmutableHashSet",
        "ImmutableQueue",
        "ImmutableSortedDictionary",
        "ImmutableSortedSet",
        "ImmutableStack",
        "IImmutableDictionary",
        "IImmutableList",
        "IImmutableQueue",
        "IImmutableSet",
        "IImmutableStack",
    };

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Immutable collection result is ignored",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Result of immutable collection operation '{0}' is ignored. Assign or return the new collection instance.",
        category: "Immutability",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(AnalyzeExpressionStatement, SyntaxKind.ExpressionStatement);

    private static void AnalyzeExpressionStatement(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ExpressionStatementSyntax expressionStatement)
            return;

        if (context.SemanticModel.GetOperation(expressionStatement) is not IExpressionStatementOperation
            {
                Operation: IInvocationOperation invocationOperation
            })
        {
            return;
        }

        if (IsImplicitValueReturnTarget(expressionStatement, context.SemanticModel))
            return;

        var method = invocationOperation.TargetMethod;

        if (!IsImmutableCollectionType(method.ReturnType))
            return;

        if (!HasImmutableCollectionReceiver(method))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, invocationOperation.Syntax.GetLocation(), method.Name));
    }

    private static bool HasImmutableCollectionReceiver(IMethodSymbol method)
    {
        if (!method.IsStatic)
            return IsImmutableCollectionType(method.ContainingType);

        return method.IsExtensionMethod &&
               method.Parameters.Length > 0 &&
               IsImmutableCollectionType(method.Parameters[0].Type);
    }

    private static bool IsImmutableCollectionType(ITypeSymbol? type)
    {
        if (type is not INamedTypeSymbol namedType)
            return false;

        var definition = namedType.OriginalDefinition;
        var ns = definition.ContainingNamespace?.ToMetadataName();
        return string.Equals(ns, "System.Collections.Immutable", StringComparison.Ordinal) &&
               ImmutableCollectionTypeNames.Contains(definition.Name);
    }

    private static bool IsImplicitValueReturnTarget(ExpressionStatementSyntax expressionStatement, SemanticModel semanticModel)
    {
        SyntaxNode blockNode = expressionStatement.Parent;
        SyntaxList<StatementSyntax> statements;

        switch (blockNode)
        {
            case BlockStatementSyntax blockStatement:
                statements = blockStatement.Statements;
                break;
            case BlockSyntax blockExpression:
                statements = blockExpression.Statements;
                break;
            default:
                return false;
        }

        if (statements.Count == 0)
            return false;

        var trailingStatement = statements[^1];
        if (trailingStatement.SyntaxTree != expressionStatement.SyntaxTree ||
            trailingStatement.Span != expressionStatement.Span)
        {
            return false;
        }

        return blockNode.Parent switch
        {
            BaseMethodDeclarationSyntax method => ReturnsValue(semanticModel.GetDeclaredSymbol(method) as IMethodSymbol),
            FunctionStatementSyntax function => ReturnsValue(semanticModel.GetDeclaredSymbol(function) as IMethodSymbol),
            AccessorDeclarationSyntax accessor => AccessorReturnsValue(accessor, semanticModel),
            FunctionExpressionSyntax functionExpression => LambdaReturnsValue(functionExpression, semanticModel),
            _ => false,
        };
    }

    private static bool AccessorReturnsValue(AccessorDeclarationSyntax accessor, SemanticModel semanticModel)
    {
        if (!string.Equals(accessor.Keyword.Text, "get", StringComparison.Ordinal))
            return false;

        return accessor.Parent?.Parent is PropertyDeclarationSyntax property &&
               semanticModel.GetDeclaredSymbol(property) is IPropertySymbol propertySymbol &&
               ReturnsValue(propertySymbol.Type);
    }

    private static bool LambdaReturnsValue(FunctionExpressionSyntax functionExpression, SemanticModel semanticModel)
    {
        var typeInfo = semanticModel.GetTypeInfo(functionExpression);
        var delegateType = typeInfo.ConvertedType as INamedTypeSymbol ?? typeInfo.Type as INamedTypeSymbol;
        return ReturnsValue(delegateType?.GetDelegateInvokeMethod()?.ReturnType);
    }

    private static bool ReturnsValue(IMethodSymbol? method)
        => ReturnsValue(method?.ReturnType);

    private static bool ReturnsValue(ITypeSymbol? type)
        => type is not null && type.SpecialType != SpecialType.System_Unit;
}
