using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class DisposableObjectAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9033";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Disposable object is not disposed",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Disposable value is not disposed before leaving scope.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Descriptor];

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeBlock, SyntaxKind.BlockStatement);
        context.RegisterSyntaxNodeAction(AnalyzeCompilationUnit, SyntaxKind.CompilationUnit);
    }

    private static void AnalyzeBlock(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is BlockStatementSyntax block)
            AnalyzeStatements(block.Statements, context);
    }

    private static void AnalyzeCompilationUnit(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not CompilationUnitSyntax compilationUnit)
            return;

        var statements = compilationUnit.Members
            .OfType<GlobalStatementSyntax>()
            .Select(static global => global.Statement)
            .ToArray();

        if (statements.Length > 0)
            AnalyzeStatements(statements, context);
    }

    private static void AnalyzeStatements(
        IEnumerable<StatementSyntax> statements,
        SyntaxNodeAnalysisContext context)
    {
        var tracked = new Dictionary<ILocalSymbol, TrackedDisposable>(SymbolEqualityComparer.Default);

        foreach (var statement in statements)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            ApplyStatementEffects(statement, context, tracked);
        }

        foreach (var disposable in tracked.Values.OrderBy(static disposable => disposable.Location.SourceSpan.Start))
            Report(context, disposable);
    }

    private static void ApplyStatementEffects(
        StatementSyntax statement,
        SyntaxNodeAnalysisContext context,
        Dictionary<ILocalSymbol, TrackedDisposable> tracked)
    {
        switch (statement)
        {
            case LocalDeclarationStatementSyntax localDeclaration:
                TrackDeclaration(localDeclaration.Declaration, isUseDeclaration: false, context, tracked);
                break;

            case UseDeclarationStatementSyntax useDeclaration:
                TrackDeclaration(useDeclaration.Declaration, isUseDeclaration: true, context, tracked);
                ApplyStatementEffects(useDeclaration.InBlockClause?.Block, context, tracked);
                break;

            case AssignmentStatementSyntax assignment:
                TrackAssignment(assignment, context, tracked);
                break;

            case ExpressionStatementSyntax expressionStatement:
                TrackExpressionStatement(expressionStatement, context, tracked);
                break;

            case ReturnStatementSyntax returnStatement:
                ApplyReturnStatement(returnStatement, context, tracked);
                break;

            case ThrowStatementSyntax:
                ReportAndClear(context, tracked);
                break;

            case BlockStatementSyntax block:
                ApplyStatementEffects(block, context, tracked);
                break;
        }
    }

    private static void ApplyStatementEffects(
        BlockStatementSyntax? block,
        SyntaxNodeAnalysisContext context,
        Dictionary<ILocalSymbol, TrackedDisposable> tracked)
    {
        if (block is null)
            return;

        foreach (var statement in block.Statements)
            ApplyStatementEffects(statement, context, tracked);
    }

    private static void TrackDeclaration(
        VariableDeclarationSyntax declaration,
        bool isUseDeclaration,
        SyntaxNodeAnalysisContext context,
        Dictionary<ILocalSymbol, TrackedDisposable> tracked)
    {
        foreach (var declarator in declaration.Declarators)
        {
            if (context.SemanticModel.GetDeclaredSymbol(declarator) is not ILocalSymbol local)
                continue;

            tracked.Remove(local);

            if (isUseDeclaration ||
                declarator.Initializer?.Value is not { } initializer ||
                !TryGetDisposableProducer(initializer, context))
            {
                continue;
            }

            tracked[local] = new TrackedDisposable(declarator.Identifier.GetLocation());
        }
    }

    private static void TrackAssignment(
        AssignmentStatementSyntax assignment,
        SyntaxNodeAnalysisContext context,
        Dictionary<ILocalSymbol, TrackedDisposable> tracked)
    {
        if (!TryGetDisposableProducer(assignment.Right, context))
            return;

        if (assignment.IsDiscard)
        {
            Report(context, assignment.Right.GetLocation());
            return;
        }

        if (TryGetLocalSymbol(assignment.Left, context.SemanticModel) is not { } local)
            return;

        if (tracked.TryGetValue(local, out var previous))
            Report(context, previous);

        tracked[local] = new TrackedDisposable(assignment.Left.GetLocation());
    }

    private static void TrackExpressionStatement(
        ExpressionStatementSyntax expressionStatement,
        SyntaxNodeAnalysisContext context,
        Dictionary<ILocalSymbol, TrackedDisposable> tracked)
    {
        if (IsImplicitValueReturnTarget(expressionStatement, context.SemanticModel))
            return;

        if (TryGetDisposedLocal(expressionStatement.Expression, context.SemanticModel) is { } disposedLocal)
        {
            tracked.Remove(disposedLocal);
            return;
        }

        if (TryGetDisposableProducer(expressionStatement.Expression, context))
            Report(context, expressionStatement.Expression.GetLocation());
    }

    private static void ApplyReturnStatement(
        ReturnStatementSyntax returnStatement,
        SyntaxNodeAnalysisContext context,
        Dictionary<ILocalSymbol, TrackedDisposable> tracked)
    {
        if (returnStatement.Expression is { } expression &&
            TryGetLocalSymbol(expression, context.SemanticModel) is { } returnedLocal)
        {
            tracked.Remove(returnedLocal);
        }

        ReportAndClear(context, tracked);
    }

    private static void ReportAndClear(
        SyntaxNodeAnalysisContext context,
        Dictionary<ILocalSymbol, TrackedDisposable> tracked)
    {
        foreach (var disposable in tracked.Values.OrderBy(static disposable => disposable.Location.SourceSpan.Start))
            Report(context, disposable);

        tracked.Clear();
    }

    private static bool TryGetDisposableProducer(
        ExpressionSyntax expression,
        SyntaxNodeAnalysisContext context)
    {
        var operation = context.SemanticModel.GetOperation(expression);
        return operation is not null &&
               TryGetDisposableProducer(operation, context.Compilation);
    }

    private static bool TryGetDisposableProducer(
        IOperation operation,
        Compilation compilation)
    {
        operation = UnwrapParenthesized(operation);

        switch (operation)
        {
            case IConversionOperation conversion:
                if (conversion.Operand is not null &&
                    TryGetDisposableProducer(conversion.Operand, compilation) &&
                    SupportsSyncDispose(compilation, conversion.Type ?? conversion.Operand.Type))
                {
                    return true;
                }

                return false;

            case IAwaitOperation awaitOperation:
                if (awaitOperation.Operation is not null &&
                    TryGetDisposableProducer(awaitOperation.Operation, compilation) &&
                    SupportsSyncDispose(compilation, awaitOperation.Type))
                {
                    return true;
                }

                return false;

            case IInvocationOperation invocation:
                if (IsDisposeInvocation(invocation))
                    return false;

                var resultType = invocation.TargetMethod.IsConstructor
                    ? invocation.Type ?? invocation.TargetMethod.ContainingType
                    : invocation.TargetMethod.ReturnType;

                if (!SupportsSyncDispose(compilation, resultType))
                    return false;

                return true;

            case IObjectCreationOperation objectCreation:
                if (!SupportsSyncDispose(compilation, objectCreation.Type))
                    return false;

                return true;

            default:
                return false;
        }
    }

    private static ILocalSymbol? TryGetDisposedLocal(
        ExpressionSyntax expression,
        SemanticModel semanticModel)
    {
        var operation = semanticModel.GetOperation(expression);
        operation = operation is null ? null : UnwrapParenthesized(operation);

        return operation is IInvocationOperation invocation && IsDisposeInvocation(invocation)
            ? TryGetInvocationReceiverLocal(invocation, semanticModel)
            : null;
    }

    private static ILocalSymbol? TryGetInvocationReceiverLocal(
        IInvocationOperation invocation,
        SemanticModel semanticModel)
    {
        if (invocation.Syntax is InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax memberAccess })
            return TryGetLocalSymbol(memberAccess.Expression, semanticModel);

        return TryGetLocalSymbol(invocation.Instance);
    }

    private static bool IsDisposeInvocation(IInvocationOperation invocation)
        => invocation.TargetMethod.Name == nameof(IDisposable.Dispose) &&
           invocation.TargetMethod.Parameters.Length == 0 &&
           !invocation.TargetMethod.IsStatic;

    private static IOperation UnwrapParenthesized(IOperation operation)
    {
        while (operation is IParenthesizedOperation { Operand: { } operand })
            operation = operand;

        return operation;
    }

    private static ILocalSymbol? TryGetLocalSymbol(SyntaxNode syntax, SemanticModel semanticModel)
    {
        var operation = semanticModel.GetOperation(syntax);
        if (operation is not null)
            return TryGetLocalSymbol(operation);

        return semanticModel.GetSymbolInfo(syntax).Symbol?.UnderlyingSymbol as ILocalSymbol;
    }

    private static ILocalSymbol? TryGetLocalSymbol(IOperation? operation)
    {
        operation = operation is null ? null : UnwrapParenthesized(operation);

        while (operation is IConversionOperation { Operand: { } operand })
            operation = UnwrapParenthesized(operand);

        return operation switch
        {
            ILocalReferenceOperation localReference => localReference.Local,
            IVariableReferenceOperation variableReference => variableReference.Variable,
            _ => null
        };
    }

    private static bool SupportsSyncDispose(Compilation compilation, ITypeSymbol? type)
        => type is not null &&
           type.TypeKind != TypeKind.Error &&
           UseDisposalUtilities.SupportsUseDisposal(compilation, type, preferAsync: false);

    private static bool IsImplicitValueReturnTarget(
        ExpressionStatementSyntax expressionStatement,
        SemanticModel semanticModel)
    {
        if (expressionStatement.Parent is not BlockStatementSyntax block ||
            block.Statements.Count == 0)
        {
            return false;
        }

        var trailingStatement = block.Statements[^1];
        if (trailingStatement.SyntaxTree != expressionStatement.SyntaxTree ||
            trailingStatement.Span != expressionStatement.Span)
        {
            return false;
        }

        return block.Parent switch
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
        => type is not null &&
           type.SpecialType is not SpecialType.System_Unit and not SpecialType.System_Void &&
           type.TypeKind is not TypeKind.Error;

    private static void Report(SyntaxNodeAnalysisContext context, TrackedDisposable disposable)
        => Report(context, disposable.Location);

    private static void Report(SyntaxNodeAnalysisContext context, Location location)
        => context.ReportDiagnostic(Diagnostic.Create(Descriptor, location));

    private sealed record TrackedDisposable(Location Location);
}
