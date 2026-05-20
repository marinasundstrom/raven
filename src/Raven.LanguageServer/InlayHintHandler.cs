using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class InlayHintHandler : IInlayHintsHandler
{
    private const int MaxUnboundedDocumentLength = 2_000;
    private const int MaxCachedInlayHintEntries = 256;
    private const double SlowInlayHintThresholdMs = 150;

    private readonly DocumentStore _documents;
    private readonly ILogger<InlayHintHandler> _logger;
    private readonly ConcurrentDictionary<InlayHintDocumentCacheKey, ImmutableArray<InlayHint>> _cache = new();

    public InlayHintHandler(DocumentStore documents, ILogger<InlayHintHandler> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    public InlayHintRegistrationOptions GetRegistrationOptions(InlayHintClientCapabilities capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven"),
            ResolveProvider = false
        };

    public void SetCapability(InlayHintClientCapabilities capability)
    {
    }

    private static IEnumerable<TNode> DescendantNodesInSpan<TNode>(SyntaxNode root, TextSpan span)
        where TNode : SyntaxNode
    {
        foreach (var child in root.ChildNodes())
        {
            if (!ShouldVisitForRequestedSpan(child, span))
                continue;

            if (child is TNode match)
                yield return match;

            foreach (var descendant in DescendantNodesInSpan<TNode>(child, span))
                yield return descendant;
        }
    }

    private static bool ShouldVisitForRequestedSpan(SyntaxNode node, TextSpan span)
    {
        if (span.Length == 0)
            return node.FullSpan.Start <= span.Start && span.Start <= node.FullSpan.End;

        return node.FullSpan.IntersectsWith(span);
    }

    public async Task<InlayHintContainer> Handle(InlayHintParams request, CancellationToken cancellationToken)
    {
        var totalStopwatch = Stopwatch.StartNew();
        var stageStopwatch = Stopwatch.StartNew();
        var gateWaitMs = 0d;
        var analysisContextMs = 0d;
        var semanticModelMs = 0d;
        var collectMs = 0d;
        var localTypeHintsMs = 0d;
        var patternTypeHintsMs = 0d;
        var forTargetTypeHintsMs = 0d;
        var functionParameterTypeHintsMs = 0d;
        var returnTypeHintsMs = 0d;
        var resultCount = 0;
        var outcome = "Completed";
        var cacheHit = false;

        try
        {
            stageStopwatch.Restart();
            using var semanticLease = await _documents.TryEnterDocumentSemanticAccessAsync(
                request.TextDocument.Uri,
                cancellationToken,
                "inlayHint").ConfigureAwait(false);
            gateWaitMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (semanticLease is null)
            {
                if (TryGetCachedHints(request, out var cachedHints))
                {
                    cacheHit = true;
                    outcome = "SkippedBusyCached";
                    resultCount = cachedHints.Length;
                    return new InlayHintContainer(cachedHints);
                }

                outcome = "SkippedBusy";
                return new InlayHintContainer();
            }

            stageStopwatch.Restart();
            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            analysisContextMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (context is null)
            {
                outcome = "NoContext";
                return new InlayHintContainer();
            }

            stageStopwatch.Restart();
            var semanticModel = await _documents.GetSemanticModelAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            semanticModelMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (semanticModel is null)
            {
                outcome = "NoSemanticModel";
                return new InlayHintContainer();
            }

            var sourceText = context.Value.SourceText;
            var root = context.Value.SyntaxTree.GetRoot(cancellationToken);
            var requestSpan = GetRequestedSpan(sourceText, request.Range);
            var isLargeDocument = sourceText.Length > MaxUnboundedDocumentLength;
            var isPreciseRequest = requestSpan.Length == 0;
            var allowExpensiveBinding = true;
            var collectionBudgetMs = double.PositiveInfinity;

            stageStopwatch.Restart();
            var hints = new List<InlayHint>();
            var collectionBudget = new InlayHintCollectionBudget(
                Stopwatch.StartNew(),
                cancellationToken,
                collectionBudgetMs,
                includeTooltips: sourceText.Length <= MaxUnboundedDocumentLength);
            var collectStopwatch = Stopwatch.StartNew();
            stageStopwatch.Restart();
            AddLocalTypeHints(
                hints,
                semanticModel,
                root,
                sourceText,
                requestSpan,
                allowInitializerBinding: allowExpensiveBinding,
                collectionBudget);
            localTypeHintsMs = stageStopwatch.Elapsed.TotalMilliseconds;
            stageStopwatch.Restart();
            if (collectionBudget.HasBudgetForAdditionalCategory())
                AddPatternTypeHints(hints, semanticModel, root, sourceText, requestSpan, collectionBudget);
            patternTypeHintsMs = stageStopwatch.Elapsed.TotalMilliseconds;
            stageStopwatch.Restart();
            if (collectionBudget.HasBudgetForAdditionalCategory())
                AddForTargetTypeHints(hints, semanticModel, root, sourceText, requestSpan, collectionBudget);
            forTargetTypeHintsMs = stageStopwatch.Elapsed.TotalMilliseconds;
            stageStopwatch.Restart();
            if (collectionBudget.HasBudgetForAdditionalCategory())
                AddFunctionExpressionParameterTypeHints(hints, semanticModel, root, sourceText, requestSpan, allowExpensiveBinding, collectionBudget);
            functionParameterTypeHintsMs = stageStopwatch.Elapsed.TotalMilliseconds;
            stageStopwatch.Restart();
            if (collectionBudget.HasBudgetForAdditionalCategory())
                AddReturnTypeHints(hints, semanticModel, root, sourceText, requestSpan, collectionBudget);
            returnTypeHintsMs = stageStopwatch.Elapsed.TotalMilliseconds;
            collectMs = collectStopwatch.Elapsed.TotalMilliseconds;
            var hintArray = hints.ToArray();
            resultCount = hintArray.Length;
            if (collectionBudget.IsExpired)
                outcome = "BudgetExpired";

            CacheHints(request, context.Value.Document.Version, hintArray);
            return new InlayHintContainer(hintArray);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            outcome = "Canceled";
            return new InlayHintContainer();
        }
        catch (Exception ex)
        {
            outcome = "Failed";
            _logger.LogError(
                ex,
                "Inlay hint request failed for {Uri} at range {StartLine}:{StartChar}-{EndLine}:{EndChar}.",
                request.TextDocument.Uri,
                request.Range.Start.Line,
                request.Range.Start.Character,
                request.Range.End.Line,
                request.Range.End.Character);
            return new InlayHintContainer();
        }
        finally
        {
            totalStopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "inlayHint",
                request.TextDocument.Uri,
                null,
                totalStopwatch.Elapsed.TotalMilliseconds,
                cacheHit: cacheHit,
                resultCount: resultCount,
                detail: $"{request.TextDocument.Uri} {request.Range.Start.Line}:{request.Range.Start.Character}-{request.Range.End.Line}:{request.Range.End.Character} outcome={outcome}",
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", analysisContextMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("semanticModel", semanticModelMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("collect", collectMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("localTypeHints", localTypeHintsMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("patternTypeHints", patternTypeHintsMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("forTargetTypeHints", forTargetTypeHintsMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("functionParameterTypeHints", functionParameterTypeHintsMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("returnTypeHints", returnTypeHintsMs)
                ]);
            if (totalStopwatch.Elapsed.TotalMilliseconds >= SlowInlayHintThresholdMs)
            {
                _logger.LogInformation(
                    "Inlay hint request completed for {Uri} in {ElapsedMs:F1}ms: gateWait={GateWaitMs:F1}ms analysisContext={AnalysisContextMs:F1}ms semanticModel={SemanticModelMs:F1}ms collect={CollectMs:F1}ms localTypes={LocalTypesMs:F1}ms patterns={PatternsMs:F1}ms forTargets={ForTargetsMs:F1}ms functionParameters={FunctionParametersMs:F1}ms returns={ReturnsMs:F1}ms hints={HintCount} outcome={Outcome}.",
                    request.TextDocument.Uri,
                    totalStopwatch.Elapsed.TotalMilliseconds,
                    gateWaitMs,
                    analysisContextMs,
                    semanticModelMs,
                    collectMs,
                    localTypeHintsMs,
                    patternTypeHintsMs,
                    forTargetTypeHintsMs,
                    functionParameterTypeHintsMs,
                    returnTypeHintsMs,
                    resultCount,
                    outcome);
            }
        }
    }

    private bool TryGetCachedHints(InlayHintParams request, out InlayHint[] hints)
    {
        hints = [];

        if (!_documents.TryGetDocument(request.TextDocument.Uri, out var document) ||
            document is null)
        {
            return false;
        }

        if (!_cache.TryGetValue(CreateCacheKey(request.TextDocument.Uri, document.Version), out var cachedHints))
            return false;

        hints = cachedHints
            .Where(hint => IsInRange(hint.Position, request.Range))
            .ToArray();
        return hints.Length > 0;
    }

    private void CacheHints(InlayHintParams request, VersionStamp version, InlayHint[] hints)
    {
        if (_cache.Count >= MaxCachedInlayHintEntries)
            _cache.Clear();

        var key = CreateCacheKey(request.TextDocument.Uri, version);
        _cache.AddOrUpdate(
            key,
            hints.ToImmutableArray(),
            (_, existing) => MergeCachedHints(existing, hints));
    }

    private static InlayHintDocumentCacheKey CreateCacheKey(DocumentUri uri, VersionStamp version)
        => new(uri.ToString(), version);

    private static ImmutableArray<InlayHint> MergeCachedHints(ImmutableArray<InlayHint> existing, InlayHint[] incoming)
    {
        if (existing.IsDefaultOrEmpty)
            return incoming.ToImmutableArray();

        if (incoming.Length == 0)
            return existing;

        var merged = existing.ToBuilder();
        var seen = existing
            .Select(static hint => (hint.Position.Line, hint.Position.Character, hint.Label.String))
            .ToHashSet();

        foreach (var hint in incoming)
        {
            if (seen.Add((hint.Position.Line, hint.Position.Character, hint.Label.String)))
                merged.Add(hint);
        }

        return merged.ToImmutable();
    }

    private static bool IsInRange(Position position, LspRange range)
    {
        if (position.Line < range.Start.Line || position.Line > range.End.Line)
            return false;

        if (position.Line == range.Start.Line && position.Character < range.Start.Character)
            return false;

        if (position.Line == range.End.Line && position.Character > range.End.Character)
            return false;

        return true;
    }

    private static void AddLocalTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        bool allowInitializerBinding,
        InlayHintCollectionBudget budget)
    {
        foreach (var declarator in DescendantNodesInSpan<VariableDeclaratorSyntax>(root, requestSpan))
        {
            if (budget.ShouldStop())
                return;

            if (declarator.TypeAnnotation is not null ||
                declarator.Identifier.IsMissing ||
                string.IsNullOrWhiteSpace(declarator.Identifier.ValueText) ||
                !IsInferredLocalLikeDeclaration(declarator))
            {
                continue;
            }

            var insertionPosition = GetTokenEndPosition(sourceText, declarator.Identifier);
            if (!ContainsPosition(requestSpan, insertionPosition))
                continue;

            if (budget.ShouldStop())
                return;

            var avoidInitializerBinding = !allowInitializerBinding &&
                ShouldAvoidInitializerBindingForInlay(declarator);
            if (avoidInitializerBinding && !CanResolveLocalHintWithoutInitializerBinding(declarator))
                continue;

            var local = semanticModel.TryGetAvailableLocalDeclarationSymbol(
                declarator,
                out var availableLocal,
                allowInitializerBinding: !avoidInitializerBinding)
                    ? availableLocal
                    : allowInitializerBinding
                        ? semanticModel.GetDeclaredSymbol(declarator) as ILocalSymbol
                        : null;

            if (local is null)
            {
                continue;
            }
            if (!TryFormatType(semanticModel, declarator, local.Type, out var typeDisplay))
            {
                continue;
            }

            hints.Add(CreateTypeHint(
                sourceText,
                insertionPosition,
                $": {typeDisplay}",
                local.Type,
                semanticModel,
                root,
                declarator,
                includeTooltip: budget.ShouldIncludeTooltip()));
        }
    }

    private static bool CanResolveLocalHintWithoutInitializerBinding(VariableDeclaratorSyntax declarator)
    {
        if (declarator.TypeAnnotation is not null)
            return false;

        if (declarator.Initializer?.Value is not { } initializer)
            return false;

        return !initializer.DescendantNodesAndSelf().OfType<InvocationExpressionSyntax>().Any() &&
            !initializer.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>().Any();
    }

    private static bool ShouldAvoidInitializerBindingForInlay(VariableDeclaratorSyntax declarator)
    {
        if (declarator.Initializer?.Value is not { } initializer)
            return false;

        return initializer.DescendantNodesAndSelf().Any(static node =>
            node is InvocationExpressionSyntax or FunctionExpressionSyntax);
    }

    private static void AddForTargetTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        InlayHintCollectionBudget budget)
    {
        foreach (var forStatement in DescendantNodesInSpan<ForStatementSyntax>(root, requestSpan))
        {
            if (budget.ShouldStop())
                return;

            if (forStatement.Target is not IdentifierNameSyntax identifierTarget ||
                identifierTarget.Identifier.IsMissing ||
                string.IsNullOrWhiteSpace(identifierTarget.Identifier.ValueText) ||
                identifierTarget.Identifier.ValueText == "_")
            {
                continue;
            }

            var insertionPosition = GetTokenEndPosition(sourceText, identifierTarget.Identifier);
            if (!ContainsPosition(requestSpan, insertionPosition))
                continue;

            if (budget.ShouldStop())
                return;

            if (semanticModel.GetBoundNode(forStatement) is not BoundForStatement { Local: { } local } ||
                !TryFormatType(semanticModel, forStatement, local.Type, out var typeDisplay))
            {
                continue;
            }

            hints.Add(CreateTypeHint(
                sourceText,
                insertionPosition,
                $": {typeDisplay}",
                local.Type,
                semanticModel,
                root,
                forStatement,
                includeTooltip: budget.ShouldIncludeTooltip()));
        }
    }

    private static void AddPatternTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        InlayHintCollectionBudget budget)
    {
        foreach (var designation in DescendantNodesInSpan<SingleVariableDesignationSyntax>(root, requestSpan))
        {
            if (budget.ShouldStop())
                return;

            if (!IsInferredPatternDesignation(designation))
                continue;

            var insertionPosition = GetTokenEndPosition(sourceText, designation.Identifier);
            if (!ContainsPosition(requestSpan, insertionPosition))
                continue;

            if (budget.ShouldStop())
                return;

            if (semanticModel.GetDeclaredSymbol(designation) is not ILocalSymbol local ||
                !TryFormatType(semanticModel, designation, local.Type, out var typeDisplay))
            {
                continue;
            }

            hints.Add(CreateTypeHint(
                sourceText,
                insertionPosition,
                $": {typeDisplay}",
                local.Type,
                semanticModel,
                root,
                designation,
                includeTooltip: budget.ShouldIncludeTooltip()));
        }
    }

    private static bool IsInferredPatternDesignation(SingleVariableDesignationSyntax designation)
    {
        if (designation.Identifier.IsMissing ||
            string.IsNullOrWhiteSpace(designation.Identifier.ValueText) ||
            designation.Identifier.ValueText == "_")
        {
            return false;
        }

        if (!designation.Ancestors().Any(static ancestor => ancestor is PatternSyntax))
            return false;

        if (designation.Ancestors().Any(static ancestor => ancestor is TypedVariableDesignationSyntax))
            return false;

        if (designation.Ancestors().OfType<DeclarationPatternSyntax>().Any(pattern =>
                pattern.Designation is not null &&
                pattern.Designation.Span.Contains(designation.Span)))
        {
            return false;
        }

        return true;
    }

    private static void AddFunctionExpressionParameterTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        bool allowBinding,
        InlayHintCollectionBudget budget)
    {
        foreach (var functionExpression in DescendantNodesInSpan<FunctionExpressionSyntax>(root, requestSpan))
        {
            if (budget.ShouldStop())
                return;

            var parameters = GetFunctionExpressionParameters(functionExpression).ToArray();
            for (var i = 0; i < parameters.Length; i++)
            {
                if (budget.ShouldStop())
                    return;

                AddFunctionExpressionParameterTypeHint(
                    hints,
                    semanticModel,
                    root,
                    sourceText,
                    requestSpan,
                    parameters[i],
                    contextualParameterType: null,
                    allowBinding,
                    budget);
            }
        }
    }

    private static void AddFunctionExpressionParameterTypeHint(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        ParameterSyntax parameter,
        ITypeSymbol? contextualParameterType,
        bool allowBinding,
        InlayHintCollectionBudget budget)
    {
        if (parameter.TypeAnnotation is not null ||
            parameter.Pattern is not null ||
            parameter.Identifier.IsMissing ||
            string.IsNullOrWhiteSpace(parameter.Identifier.ValueText) ||
            parameter.Identifier.ValueText == "_")
        {
            return;
        }

        var insertionPosition = GetTokenEndPosition(sourceText, parameter.Identifier);
        if (!ContainsPosition(requestSpan, insertionPosition))
            return;

        if (budget.ShouldStop())
            return;

        var parameterType = contextualParameterType;
        if (parameterType is null &&
            semanticModel.TryResolveFunctionExpressionParameterSymbolFast(
                parameter,
                out var fastParameter,
                allowCandidateLookup: allowBinding) &&
            fastParameter?.Type is { } fastParameterType)
        {
            parameterType = fastParameterType;
        }
        else if (parameterType is null &&
            allowBinding &&
            semanticModel.GetFunctionExpressionParameterSymbol(parameter) is { Type: { } resolvedParameterType })
        {
            parameterType = resolvedParameterType;
        }

        if (parameterType is null ||
            !TryFormatType(semanticModel, parameter, parameterType, out var typeDisplay))
        {
            return;
        }

        hints.Add(CreateTypeHint(
            sourceText,
            insertionPosition,
            $": {typeDisplay}",
            parameterType,
            semanticModel,
            root,
            parameter,
            includeTooltip: budget.ShouldIncludeTooltip()));
    }

    private static IEnumerable<ParameterSyntax> GetFunctionExpressionParameters(FunctionExpressionSyntax functionExpression)
    {
        switch (functionExpression)
        {
            case SimpleFunctionExpressionSyntax simple:
                yield return simple.Parameter;
                break;

            case ParenthesizedFunctionExpressionSyntax parenthesized:
                foreach (var parameter in parenthesized.ParameterList.Parameters)
                    yield return parameter;
                break;
        }
    }

    private static void AddReturnTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        InlayHintCollectionBudget budget)
    {
        foreach (var method in DescendantNodesInSpan<MethodDeclarationSyntax>(root, requestSpan))
        {
            if (budget.ShouldStop())
                return;

            AddReturnTypeHint(
                hints,
                semanticModel,
                root,
                sourceText,
                requestSpan,
                method,
                method.ReturnType,
                method.ParameterList,
                method.Body ?? (SyntaxNode?)method.ExpressionBody,
                IsAsyncDeclaration(method),
                budget);
        }

        foreach (var function in DescendantNodesInSpan<FunctionStatementSyntax>(root, requestSpan))
        {
            if (budget.ShouldStop())
                return;

            AddReturnTypeHint(
                hints,
                semanticModel,
                root,
                sourceText,
                requestSpan,
                function,
                function.ReturnType,
                function.ParameterList,
                function.Body ?? (SyntaxNode?)function.ExpressionBody,
                IsAsyncDeclaration(function),
                budget);
        }

        foreach (var functionExpression in DescendantNodesInSpan<FunctionExpressionSyntax>(root, requestSpan))
        {
            if (budget.ShouldStop())
                return;

            AddFunctionExpressionReturnTypeHint(hints, semanticModel, root, sourceText, requestSpan, functionExpression, budget);
        }
    }

    private static bool IsInferredLocalLikeDeclaration(VariableDeclaratorSyntax declarator)
        => declarator.Ancestors().Any(static ancestor => ancestor is LocalDeclarationStatementSyntax or UseDeclarationStatementSyntax);

    private static void AddReturnTypeHint(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        SyntaxNode declaration,
        ArrowTypeClauseSyntax? returnType,
        ParameterListSyntax parameterList,
        SyntaxNode? body,
        bool isAsync,
        InlayHintCollectionBudget budget)
    {
        if (HasExplicitReturnType(returnType) || body is null)
            return;

        var insertionPosition = parameterList.Span.End;
        if (!ContainsPosition(requestSpan, insertionPosition))
            return;

        if (budget.ShouldStop())
            return;

        if (!TryGetInferredReturnType(semanticModel, body, isAsync, out var inferredReturnType) ||
            !TryFormatType(semanticModel, declaration, inferredReturnType, out var typeDisplay))
        {
            return;
        }

        hints.Add(CreateTypeHint(
            sourceText,
            insertionPosition,
            $" -> {typeDisplay}",
            inferredReturnType,
            semanticModel,
            root,
            declaration,
            includeTooltip: budget.ShouldIncludeTooltip()));
    }

    private static bool HasExplicitReturnType(ArrowTypeClauseSyntax? returnType)
        => returnType is not null &&
           !returnType.ArrowToken.IsMissing &&
           !returnType.Type.IsMissing;

    private static void AddFunctionExpressionReturnTypeHint(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        FunctionExpressionSyntax functionExpression,
        InlayHintCollectionBudget budget)
    {
        var insertionPosition = functionExpression switch
        {
            ParenthesizedFunctionExpressionSyntax { ReturnType: null } parenthesized => parenthesized.ParameterList.Span.End,
            SimpleFunctionExpressionSyntax { ReturnType: null } simple => GetTokenEndPosition(sourceText, simple.Parameter.Identifier),
            _ => -1
        };

        if (insertionPosition < 0 || !ContainsPosition(requestSpan, insertionPosition))
            return;

        if (budget.ShouldStop())
            return;

        if (IsBlockFunctionExpressionWithoutValueReturn(functionExpression))
            return;

        if (!TryGetFunctionExpressionReturnType(semanticModel, functionExpression, out var returnType) ||
            !TryFormatType(semanticModel, functionExpression, returnType, out var typeDisplay))
        {
            return;
        }

        hints.Add(CreateTypeHint(
            sourceText,
            insertionPosition,
            $" -> {typeDisplay}",
            returnType,
            semanticModel,
            root,
            functionExpression,
            includeTooltip: budget.ShouldIncludeTooltip()));
    }

    private static bool IsBlockFunctionExpressionWithoutValueReturn(FunctionExpressionSyntax functionExpression)
    {
        var body = functionExpression switch
        {
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.Body,
            SimpleFunctionExpressionSyntax simple => simple.Body,
            _ => null
        };

        return body is not null &&
            !DescendantNodesExcludingNestedExecutableScopes(body)
                .OfType<ReturnStatementSyntax>()
                .Any(static returnStatement => returnStatement.Expression is not null);
    }

    private static InlayHint CreateTypeHint(
        SourceText sourceText,
        int insertionPosition,
        string text,
        ITypeSymbol type,
        SemanticModel semanticModel,
        SyntaxNode root,
        SyntaxNode contextNode,
        bool includeTooltip)
    {
        var range = PositionHelper.ToRange(sourceText, new TextSpan(insertionPosition, 0));
        return new InlayHint
        {
            Position = range.Start,
            Label = text,
            Kind = InlayHintKind.Type,
            Tooltip = includeTooltip ? CreateTooltip(type, semanticModel, root, contextNode, insertionPosition, text) : null,
            PaddingLeft = false,
            PaddingRight = false,
            TextEdits = new Container<TextEdit>(new TextEdit
            {
                Range = range,
                NewText = text
            })
        };
    }

    private static int GetTokenEndPosition(SourceText sourceText, SyntaxToken token)
    {
        var candidate = token.Span.End;
        var tokenText = token.Text;
        if (string.IsNullOrEmpty(tokenText))
            return candidate;

        var content = sourceText.ToString();
        if (candidate >= tokenText.Length &&
            candidate <= content.Length &&
            string.Equals(
                content.Substring(candidate - tokenText.Length, tokenText.Length),
                tokenText,
                StringComparison.Ordinal))
        {
            return candidate;
        }

        var searchPosition = Math.Clamp(candidate, 0, content.Length);
        var lineStart = content.LastIndexOf('\n', Math.Max(0, searchPosition - 1));
        lineStart = lineStart < 0 ? 0 : lineStart + 1;
        var lineEnd = content.IndexOf('\n', searchPosition);
        if (lineEnd < 0)
            lineEnd = content.Length;

        var line = content.Substring(lineStart, lineEnd - lineStart);
        var bestEnd = candidate;
        var bestDistance = int.MaxValue;
        var index = line.IndexOf(tokenText, StringComparison.Ordinal);
        while (index >= 0)
        {
            var end = lineStart + index + tokenText.Length;
            var distance = Math.Abs(end - candidate);
            if (distance < bestDistance)
            {
                bestEnd = end;
                bestDistance = distance;
            }

            index = line.IndexOf(tokenText, index + tokenText.Length, StringComparison.Ordinal);
        }

        return bestEnd;
    }

    private static bool TryFormatType(SemanticModel semanticModel, SyntaxNode contextNode, ITypeSymbol? type, out string display)
    {
        display = string.Empty;
        if (type is null ||
            type.TypeKind == TypeKind.Error ||
            type.ContainsErrorType())
        {
            return false;
        }

        display = type is ITypeUnionSymbol union
            ? string.Join(
                " | ",
                union.Types
                    .Select(member => FormatSingleType(semanticModel, contextNode, member))
                    .OrderBy(static value => value, StringComparer.Ordinal))
            : FormatSingleType(semanticModel, contextNode, type);

        return !string.IsNullOrWhiteSpace(display);
    }

    private static string FormatSingleType(SemanticModel semanticModel, SyntaxNode contextNode, ITypeSymbol type)
        => FormatTypeForInsertion(semanticModel.GetBinder(contextNode), type);

    private static string FormatTypeForInsertion(Binder binder, ITypeSymbol type)
    {
        if (TryFormatSpecialType(type, out var specialTypeDisplay))
            return specialTypeDisplay;

        if (type is LiteralTypeSymbol)
            return type.ToDisplayStringKeywordAware(SourceTypeDisplayFormat);

        if (type.GetNullableUnderlyingType() is { } nullableUnderlying)
        {
            var underlyingDisplay = FormatTypeForInsertion(binder, nullableUnderlying);
            if (nullableUnderlying is ITypeUnionSymbol ||
                IsStandardUnionType(nullableUnderlying) ||
                nullableUnderlying is INamedTypeSymbol { TypeKind: TypeKind.Delegate })
            {
                underlyingDisplay = $"({underlyingDisplay})";
            }

            return underlyingDisplay + "?";
        }

        if (type is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType)
            return delegateType.ToDisplayStringKeywordAware(SourceTypeDisplayFormat);

        if (type is ITypeParameterSymbol typeParameter)
            return typeParameter.Name;

        if (type is IArrayTypeSymbol arrayType)
        {
            var elementDisplay = FormatTypeForInsertion(binder, arrayType.ElementType);
            if (arrayType.ElementType is ITypeUnionSymbol ||
                IsStandardUnionType(arrayType.ElementType) ||
                arrayType.ElementType is INamedTypeSymbol { TypeKind: TypeKind.Delegate })
            {
                elementDisplay = $"({elementDisplay})";
            }

            if (arrayType.Rank == 1)
                return arrayType.FixedLength is int fixedLength
                    ? $"{elementDisplay}[{fixedLength}]"
                    : $"{elementDisplay}[]";

            return elementDisplay + "[" + new string(',', arrayType.Rank - 1) + "]";
        }

        if (type is IPointerTypeSymbol pointerType)
            return "*" + FormatTypeForInsertion(binder, pointerType.PointedAtType);

        if (type is RefTypeSymbol refType)
            return "&" + FormatTypeForInsertion(binder, refType.ElementType);

        if (type is IAddressTypeSymbol addressType)
            return "&" + FormatTypeForInsertion(binder, addressType.ReferencedType);

        if (type is ITupleTypeSymbol tupleType)
        {
            var elements = tupleType.TupleElements.Select(element =>
                string.IsNullOrWhiteSpace(element.Name)
                    ? FormatTypeForInsertion(binder, element.Type)
                    : $"{element.Name}: {FormatTypeForInsertion(binder, element.Type)}");
            return "(" + string.Join(", ", elements) + ")";
        }

        if (type is ITypeUnionSymbol unionType)
            return string.Join(
                " | ",
                unionType.Types
                    .Select(member => FormatTypeForInsertion(binder, member))
                    .OrderBy(static value => value, StringComparer.Ordinal));

        if (IsStandardUnionType(type) &&
            type is INamedTypeSymbol standardUnion &&
            !standardUnion.TypeArguments.IsDefaultOrEmpty)
        {
            return string.Join(
                " | ",
                standardUnion.TypeArguments
                    .Select(member => FormatTypeForInsertion(binder, member))
                    .OrderBy(static value => value, StringComparer.Ordinal));
        }

        if (type is INamedTypeSymbol namedType)
            return FormatNamedTypeForInsertion(binder, namedType);

        return type.ToDisplayStringKeywordAware(SourceTypeDisplayFormat);
    }

    private static string FormatNamedTypeForInsertion(Binder binder, INamedTypeSymbol type)
    {
        if (type.ContainingType is not null)
            return type.ToDisplayStringKeywordAware(QualifiedSourceTypeDisplayFormat);

        var name = type.Name;

        if (type.Arity <= 0)
            return name;

        var typeArguments = type.TypeArguments;
        if (typeArguments.IsDefaultOrEmpty)
        {
            var typeParameters = type.TypeParameters;
            if (typeParameters.IsDefaultOrEmpty)
                return name;

            return $"{name}<{string.Join(", ", typeParameters.Select(parameter => parameter.Name))}>";
        }

        var offset = Math.Max(0, typeArguments.Length - type.Arity);
        var arguments = typeArguments
            .Skip(offset)
            .Take(type.Arity)
            .Select(argument => FormatTypeForInsertion(binder, argument));

        return $"{name}<{string.Join(", ", arguments)}>";
    }

    private static bool CanUseSimpleName(Binder binder, INamedTypeSymbol type)
    {
        var matchingDefinitionSeen = false;
        var lookupType = binder.LookupType(type.Name);

        if (lookupType is INamedTypeSymbol lookupNamedType &&
            SameNamedTypeDefinition(lookupNamedType, type))
        {
            if (IsSourceTypeDeclaredInCurrentNamespace(binder, type))
                return true;

            matchingDefinitionSeen = true;
        }
        else if (lookupType is not null &&
            lookupType.Name == type.Name &&
            (lookupType is not INamedTypeSymbol lookupNamedOther || lookupNamedOther.Arity == type.Arity))
        {
            return false;
        }

        foreach (var candidate in GetVisibleTypeCandidates(binder, type.Name, lookupType))
        {
            if (candidate is INamedTypeSymbol namedCandidate &&
                SameNamedTypeDefinition(namedCandidate, type))
            {
                matchingDefinitionSeen = true;
                continue;
            }

            if (candidate.Name == type.Name &&
                (candidate is not INamedTypeSymbol namedOther || namedOther.Arity == type.Arity))
            {
                return false;
            }
        }

        return matchingDefinitionSeen;
    }

    private static IEnumerable<ITypeSymbol> GetVisibleTypeCandidates(Binder binder, string name, ITypeSymbol? knownLookupType)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);

        if (knownLookupType is not null &&
            seen.Add(GetTypeIdentity(knownLookupType)))
        {
            yield return knownLookupType;
        }

        foreach (var symbol in binder.LookupSymbols(name).OfType<ITypeSymbol>())
        {
            if (seen.Add(GetTypeIdentity(symbol)))
                yield return symbol;
        }
    }

    private static bool IsSourceTypeDeclaredInCurrentNamespace(Binder binder, INamedTypeSymbol type)
    {
        var definition = GetNamedTypeDefinition(type);
        if (definition.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return false;

        return SameNamespace(definition.ContainingNamespace, binder.CurrentNamespace);
    }

    private static bool SameNamespace(INamespaceSymbol? left, INamespaceSymbol? right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        if (left is null || right is null)
            return false;

        if (left.IsGlobalNamespace || right.IsGlobalNamespace)
            return left.IsGlobalNamespace && right.IsGlobalNamespace;

        return string.Equals(left.ToDisplayString(), right.ToDisplayString(), StringComparison.Ordinal);
    }

    private static bool SameNamedTypeDefinition(INamedTypeSymbol candidate, INamedTypeSymbol target)
    {
        var candidateDefinition = GetNamedTypeDefinition(candidate);
        var targetDefinition = GetNamedTypeDefinition(target);
        return SymbolEqualityComparer.Default.Equals(candidateDefinition, targetDefinition) ||
            string.Equals(
                candidateDefinition.ToFullyQualifiedMetadataName(),
                targetDefinition.ToFullyQualifiedMetadataName(),
                StringComparison.Ordinal);
    }

    private static INamedTypeSymbol GetNamedTypeDefinition(INamedTypeSymbol type)
        => type.OriginalDefinition as INamedTypeSymbol
            ?? type.ConstructedFrom as INamedTypeSymbol
            ?? type;

    private static string GetTypeIdentity(ITypeSymbol type)
        => type is INamedTypeSymbol named
            ? GetNamedTypeDefinition(named).ToFullyQualifiedMetadataName()
            : type.ToDisplayStringKeywordAware(QualifiedSourceTypeDisplayFormat);

    private static bool TryFormatSpecialType(ITypeSymbol type, out string display)
    {
        display = type.SpecialType switch
        {
            SpecialType.System_Object => "object",
            SpecialType.System_String => "string",
            SpecialType.System_Boolean => "bool",
            SpecialType.System_Char => "char",
            SpecialType.System_SByte => "sbyte",
            SpecialType.System_Byte => "byte",
            SpecialType.System_Int16 => "short",
            SpecialType.System_UInt16 => "ushort",
            SpecialType.System_Int32 => "int",
            SpecialType.System_UInt32 => "uint",
            SpecialType.System_Int64 => "long",
            SpecialType.System_UInt64 => "ulong",
            SpecialType.System_Decimal => "decimal",
            SpecialType.System_Single => "float",
            SpecialType.System_Double => "double",
            SpecialType.System_Unit => "()",
            _ => string.Empty
        };

        return display.Length > 0;
    }

    private static bool IsStandardUnionType(ITypeSymbol type)
    {
        if (type is not INamedTypeSymbol namedType)
            return false;

        var definition = GetNamedTypeDefinition(namedType);
        return definition.Arity is >= 2 and <= 5 &&
            string.Equals(definition.Name, "Union", StringComparison.Ordinal) &&
            string.Equals(definition.ContainingNamespace?.ToDisplayString(), "System", StringComparison.Ordinal);
    }

    private static StringOrMarkupContent CreateTooltip(
        ITypeSymbol type,
        SemanticModel semanticModel,
        SyntaxNode root,
        SyntaxNode contextNode,
        int offset,
        string insertionText)
        => new(new MarkupContent
        {
            Kind = MarkupKind.Markdown,
            Value = HoverHandler.BuildInlayTypeHoverText(type, semanticModel, root, contextNode, offset, insertionText)
        });

    private static readonly SymbolDisplayFormat SourceTypeDisplayFormat =
        SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithMiscellaneousOptions(
                SymbolDisplayFormat.MinimallyQualifiedFormat.MiscellaneousOptions |
                SymbolDisplayMiscellaneousOptions.EscapeIdentifiers |
                SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers);

    private static readonly SymbolDisplayFormat QualifiedSourceTypeDisplayFormat =
        SourceTypeDisplayFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces);

    private static bool TryGetInferredReturnType(
        SemanticModel semanticModel,
        SyntaxNode body,
        bool isAsync,
        out ITypeSymbol? inferredReturnType)
    {
        return TryInferReturnTypeFromAvailableSyntax(semanticModel, body, isAsync, out inferredReturnType) &&
            IsInlayHintReturnType(inferredReturnType);
    }

    private static bool IsInlayHintReturnType(ITypeSymbol? type)
        => type is not null &&
           type.SpecialType is not (SpecialType.System_Unit or SpecialType.System_Void);

    private static bool TryInferReturnTypeFromAvailableSyntax(
        SemanticModel semanticModel,
        SyntaxNode body,
        bool isAsync,
        out ITypeSymbol? inferredReturnType)
    {
        var types = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);

        if (body is ArrowExpressionClauseSyntax arrowExpression)
        {
            if (!TryAddAvailableReturnExpressionType(semanticModel, arrowExpression.Expression, types))
            {
                inferredReturnType = null;
                return false;
            }

            return TryCompleteAvailableReturnTypeInference(semanticModel, types, isAsync, out inferredReturnType);
        }

        foreach (var returnStatement in DescendantNodesExcludingNestedExecutableScopes(body).OfType<ReturnStatementSyntax>())
        {
            if (returnStatement.Expression is not { } expression)
                continue;

            if (!TryAddAvailableReturnExpressionType(semanticModel, expression, types))
            {
                inferredReturnType = null;
                return false;
            }
        }

        if (types.Count == 0 &&
            TryGetTailExpression(body) is { } tailExpression &&
            !TryAddAvailableReturnExpressionType(semanticModel, tailExpression, types))
        {
            inferredReturnType = null;
            return false;
        }

        return TryCompleteAvailableReturnTypeInference(semanticModel, types, isAsync, out inferredReturnType);
    }

    private static bool TryAddAvailableReturnExpressionType(
        SemanticModel semanticModel,
        ExpressionSyntax expression,
        HashSet<ITypeSymbol> types)
    {
        if (!semanticModel.TryGetAvailableTypeInfo(expression, out var typeInfo))
            return false;

        var type = typeInfo.Type ?? typeInfo.ConvertedType;
        if (type is null ||
            type.TypeKind == TypeKind.Error ||
            type.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
        {
            return true;
        }

        types.Add(TypeSymbolNormalization.NormalizeForInference(type));
        return true;
    }

    private static bool TryCompleteAvailableReturnTypeInference(
        SemanticModel semanticModel,
        HashSet<ITypeSymbol> types,
        bool isAsync,
        out ITypeSymbol? inferredReturnType)
    {
        inferredReturnType = types.Count switch
        {
            0 => null,
            1 => types.First(),
            _ => TypeSymbolNormalization.NormalizeUnion(types)
        };

        if (isAsync)
            inferredReturnType = AsyncReturnTypeUtilities.InferAsyncReturnType(semanticModel.Compilation, inferredReturnType);

        return inferredReturnType is not null;
    }

    private static ExpressionSyntax? TryGetTailExpression(SyntaxNode body)
    {
        var statements = body switch
        {
            BlockStatementSyntax blockStatement => blockStatement.Statements,
            BlockSyntax blockExpression => blockExpression.Statements,
            _ => default
        };

        return statements.Count > 0 &&
            statements[^1] is ExpressionStatementSyntax expressionStatement
                ? expressionStatement.Expression
                : null;
    }

    private static IEnumerable<SyntaxNode> DescendantNodesExcludingNestedExecutableScopes(SyntaxNode node)
    {
        foreach (var child in node.ChildNodes())
        {
            if (!ReferenceEquals(child, node) &&
                child is FunctionStatementSyntax or FunctionExpressionSyntax or MethodDeclarationSyntax)
            {
                continue;
            }

            yield return child;

            foreach (var descendant in DescendantNodesExcludingNestedExecutableScopes(child))
                yield return descendant;
        }
    }

    private static bool TryGetFunctionExpressionReturnType(
        SemanticModel semanticModel,
        FunctionExpressionSyntax functionExpression,
        out ITypeSymbol? returnType)
    {
        returnType = null;

        if (semanticModel.TryGetAvailableFunctionExpressionReturnType(functionExpression, out var availableReturnType) &&
            IsInlayHintReturnType(availableReturnType))
        {
            returnType = availableReturnType;
            return true;
        }

        var isAsync = IsAsyncFunctionExpression(functionExpression);
        var body = (SyntaxNode?)functionExpression.ExpressionBody ?? functionExpression.Body;

        if (body is not null &&
            TryInferReturnTypeFromAvailableSyntax(semanticModel, body, isAsync, out var inferredReturnType) &&
            IsInlayHintReturnType(inferredReturnType))
        {
            returnType = inferredReturnType;
            return true;
        }

        return false;
    }

    private static bool IsAsyncDeclaration(SyntaxNode declaration)
        => declaration switch
        {
            MethodDeclarationSyntax method => method.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.AsyncKeyword),
            FunctionStatementSyntax function => function.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.AsyncKeyword),
            _ => false
        };

    private static bool IsAsyncFunctionExpression(FunctionExpressionSyntax functionExpression)
        => functionExpression switch
        {
            SimpleFunctionExpressionSyntax simple => simple.AsyncKeyword.Kind == SyntaxKind.AsyncKeyword,
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.AsyncKeyword.Kind == SyntaxKind.AsyncKeyword,
            _ => false
        };

    private static TextSpan GetRequestedSpan(SourceText text, LspRange requestRange)
    {
        var start = PositionHelper.ToOffset(text, requestRange.Start);
        var end = PositionHelper.ToOffset(text, requestRange.End);
        if (end < start)
            (start, end) = (end, start);

        return new TextSpan(start, end - start);
    }

    private static bool ContainsPosition(TextSpan span, int position)
        => position >= span.Start && position <= span.End;

    private readonly record struct InlayHintDocumentCacheKey(
        string Uri,
        VersionStamp Version);

    private readonly struct InlayHintCollectionBudget
    {
        private const double MinTooltipBudgetMilliseconds = 50;
        private const double MinAdditionalCategoryBudgetMilliseconds = 350;

        private readonly Stopwatch _stopwatch;
        private readonly CancellationToken _cancellationToken;
        private readonly double _maxMilliseconds;

        public InlayHintCollectionBudget(
            Stopwatch stopwatch,
            CancellationToken cancellationToken,
            double maxMilliseconds,
            bool includeTooltips)
        {
            _stopwatch = stopwatch;
            _cancellationToken = cancellationToken;
            _maxMilliseconds = maxMilliseconds;
            IncludeTooltips = includeTooltips;
        }

        private bool IncludeTooltips { get; }

        public bool IsExpired => _stopwatch.Elapsed.TotalMilliseconds >= _maxMilliseconds;

        public bool HasBudgetForAdditionalCategory()
        {
            _cancellationToken.ThrowIfCancellationRequested();
            return double.IsPositiveInfinity(_maxMilliseconds) ||
                _stopwatch.Elapsed.TotalMilliseconds <= _maxMilliseconds - MinAdditionalCategoryBudgetMilliseconds;
        }

        public bool ShouldStop()
        {
            _cancellationToken.ThrowIfCancellationRequested();
            return IsExpired;
        }

        public bool ShouldIncludeTooltip()
        {
            _cancellationToken.ThrowIfCancellationRequested();
            return IncludeTooltips &&
                _stopwatch.Elapsed.TotalMilliseconds <= _maxMilliseconds - MinTooltipBudgetMilliseconds;
        }
    }
}
