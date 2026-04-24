using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class SemanticTokensHandler : SemanticTokensHandlerBase
{
    private const int MaxCachedSemanticTokenEntries = 256;
    private const double SemanticTokensLogThresholdMs = 150;
    internal static readonly SemanticTokensLegend Legend = new()
    {
        TokenTypes = new Container<SemanticTokenType>(
            SemanticTokenType.Keyword,
            SemanticTokenType.String,
            SemanticTokenType.Number,
            SemanticTokenType.Regexp,
            SemanticTokenType.Comment,
            SemanticTokenType.Operator,
            SemanticTokenType.Namespace,
            SemanticTokenType.Type,
            SemanticTokenType.Class,
            SemanticTokenType.Struct,
            SemanticTokenType.Interface,
            SemanticTokenType.Enum,
            SemanticTokenType.Method,
            SemanticTokenType.Property,
            SemanticTokenType.Variable,
            SemanticTokenType.Parameter,
            SemanticTokenType.Label,
            SemanticTokenType.Event),
        TokenModifiers = new Container<SemanticTokenModifier>(
            SemanticTokenModifier.Declaration,
            SemanticTokenModifier.Static,
            SemanticTokenModifier.Readonly,
            SemanticTokenModifier.Async,
            SemanticTokenModifier.Abstract)
    };

    private readonly DocumentStore _documents;
    private readonly ILogger<SemanticTokensHandler> _logger;
    private readonly ConcurrentDictionary<SemanticTokensCacheKey, SemanticTokenEntry[]> _tokenEntryCache = new();

    public SemanticTokensHandler(DocumentStore documents, ILogger<SemanticTokensHandler> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    protected override SemanticTokensRegistrationOptions CreateRegistrationOptions(SemanticTokensCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven"),
            Legend = Legend,
            Full = true,
            Range = false
        };

    protected override async Task<SemanticTokensDocument> GetSemanticTokensDocument(ITextDocumentIdentifierParams @params, CancellationToken cancellationToken)
    {
        await Task.CompletedTask.ConfigureAwait(false);
        return new SemanticTokensDocument(Legend);
    }

    protected override async Task Tokenize(SemanticTokensBuilder builder, ITextDocumentIdentifierParams identifier, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double contextMs = 0;
        double rootMs = 0;
        double semanticModelMs = 0;
        double classifyMs = 0;
        double materializeMs = 0;
        double pushMs = 0;

        try
        {
            using var _ = await _documents.EnterDocumentSemanticAccessAsync(identifier.TextDocument.Uri, cancellationToken, "semanticTokens").ConfigureAwait(false);
            gateWaitMs = stopwatch.Elapsed.TotalMilliseconds;

            var stageStopwatch = Stopwatch.StartNew();
            var context = await _documents.GetAnalysisContextAsync(identifier.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            contextMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (context is null)
                return;

            var syntaxTree = context.Value.SyntaxTree;
            var sourceText = context.Value.SourceText;
            var cacheKey = new SemanticTokensCacheKey(identifier.TextDocument.Uri.ToString(), context.Value.Document.Version);
            var requestedRange = GetRequestedRangeSpan(identifier, sourceText);

            if (_tokenEntryCache.TryGetValue(cacheKey, out var cachedEntries))
            {
                foreach (var entry in FilterEntriesForRange(cachedEntries, requestedRange))
                    PushSpan(builder, sourceText, entry.Span, entry.TokenType!.Value, entry.Modifiers);

                pushMs = stopwatch.Elapsed.TotalMilliseconds - gateWaitMs - contextMs;
                return;
            }

            stageStopwatch.Restart();
            var root = syntaxTree.GetRoot(cancellationToken);
            rootMs = stageStopwatch.Elapsed.TotalMilliseconds;

            stageStopwatch.Restart();
            var semanticModel = context.Value.Compilation.GetSemanticModel(syntaxTree);
            semanticModelMs = stageStopwatch.Elapsed.TotalMilliseconds;

            stageStopwatch.Restart();
            var classification = SemanticClassifier.Classify(root, semanticModel);
            classifyMs = stageStopwatch.Elapsed.TotalMilliseconds;

            stageStopwatch.Restart();
            var tokenEntries = classification.Tokens
                .Select(pair => CreateEntry(pair.Key.Span, pair.Value, pair.Key, semanticModel, root))
                .Where(static entry => entry!.TokenType is not null)
                .Cast<SemanticTokenEntry>()
                .ToArray();

            var triviaEntries = classification.Trivia
                .Select(pair => CreateEntry(pair.Key.Span, pair.Value))
                .Where(static entry => entry!.TokenType is not null)
                .Cast<SemanticTokenEntry>()
                .ToArray();

            var entries = tokenEntries
                .Concat(triviaEntries)
                .OrderBy(static entry => entry.Span.Start)
                .ThenBy(static entry => entry.Span.Length)
                .ToArray();
            materializeMs = stageStopwatch.Elapsed.TotalMilliseconds;
            CacheTokenEntries(cacheKey, entries);

            stageStopwatch.Restart();
            foreach (var entry in FilterEntriesForRange(entries, requestedRange))
                PushSpan(builder, sourceText, entry.Span, entry.TokenType!.Value, entry.Modifiers);
            pushMs = stageStopwatch.Elapsed.TotalMilliseconds;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            if (stopwatch.Elapsed.TotalMilliseconds >= SemanticTokensLogThresholdMs)
            {
                _logger.LogInformation(
                    "Semantic token request canceled for {Uri} after {ElapsedMs:F1}ms.",
                    identifier.TextDocument.Uri,
                    stopwatch.Elapsed.TotalMilliseconds);
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Semantic token request failed for {Uri}.", identifier.TextDocument.Uri);
        }
        finally
        {
            stopwatch.Stop();
            if (stopwatch.Elapsed.TotalMilliseconds >= SemanticTokensLogThresholdMs)
            {
                _logger.LogInformation(
                    "Semantic token request completed for {Uri} in {ElapsedMs:F1}ms: gateWait={GateWaitMs:F1}ms context={ContextMs:F1}ms root={RootMs:F1}ms semanticModel={SemanticModelMs:F1}ms classify={ClassifyMs:F1}ms materialize={MaterializeMs:F1}ms push={PushMs:F1}ms.",
                    identifier.TextDocument.Uri,
                    stopwatch.Elapsed.TotalMilliseconds,
                    gateWaitMs,
                    contextMs,
                    rootMs,
                    semanticModelMs,
                    classifyMs,
                    materializeMs,
                    pushMs);
            }
        }
    }

    private void CacheTokenEntries(SemanticTokensCacheKey cacheKey, SemanticTokenEntry[] entries)
    {
        if (_tokenEntryCache.Count >= MaxCachedSemanticTokenEntries)
            _tokenEntryCache.Clear();

        _tokenEntryCache[cacheKey] = entries;
    }

    private static IEnumerable<SemanticTokenEntry> FilterEntriesForRange(
        IEnumerable<SemanticTokenEntry> entries,
        TextSpan? requestedRange)
    {
        if (requestedRange is not { } range)
            return entries;

        return entries
            .Select(entry => TryClipEntryToRange(entry, range))
            .Where(static entry => entry is not null)!
            .Cast<SemanticTokenEntry>();
    }

    private static SemanticTokenEntry? TryClipEntryToRange(SemanticTokenEntry entry, TextSpan requestedRange)
    {
        var start = Math.Max(entry.Span.Start, requestedRange.Start);
        var end = Math.Min(entry.Span.End, requestedRange.End);
        if (end <= start)
            return null;

        var clippedSpan = start == entry.Span.Start && end == entry.Span.End
            ? entry.Span
            : TextSpan.FromBounds(start, end);

        return entry with { Span = clippedSpan };
    }

    private static TextSpan? GetRequestedRangeSpan(ITextDocumentIdentifierParams identifier, SourceText sourceText)
    {
        if (identifier is not SemanticTokensRangeParams rangeParams)
            return null;

        var start = PositionHelper.ToOffset(sourceText, rangeParams.Range.Start);
        var end = PositionHelper.ToOffset(sourceText, rangeParams.Range.End);
        if (end < start)
            (start, end) = (end, start);

        return TextSpan.FromBounds(start, end);
    }

    private static SemanticTokenEntry? CreateEntry(
        TextSpan span,
        SemanticClassification classification,
        SyntaxToken token,
        SemanticModel semanticModel,
        SyntaxNode root)
    {
        var tokenType = MapTokenType(classification, token, semanticModel, root);
        if (tokenType is null)
            return null;

        return new SemanticTokenEntry(span, tokenType, GetModifiers(token, semanticModel));
    }

    private static SemanticTokenEntry? CreateEntry(TextSpan span, SemanticClassification classification)
    {
        var tokenType = MapTokenType(classification);
        return tokenType is null
            ? null
            : new SemanticTokenEntry(span, tokenType, ImmutableArray<SemanticTokenModifier>.Empty);
    }

    private static SemanticTokenType? MapTokenType(SemanticClassification classification)
        => classification switch
        {
            SemanticClassification.Keyword => SemanticTokenType.Keyword,
            SemanticClassification.NumericLiteral => SemanticTokenType.Number,
            SemanticClassification.StringLiteral => SemanticTokenType.String,
            SemanticClassification.Operator => SemanticTokenType.Operator,
            SemanticClassification.Interpolation => SemanticTokenType.Operator,
            SemanticClassification.Comment => SemanticTokenType.Comment,
            SemanticClassification.Namespace => SemanticTokenType.Namespace,
            SemanticClassification.Type => SemanticTokenType.Type,
            SemanticClassification.Method => SemanticTokenType.Method,
            SemanticClassification.Parameter => SemanticTokenType.Parameter,
            SemanticClassification.Local => SemanticTokenType.Variable,
            SemanticClassification.Label => SemanticTokenType.Label,
            SemanticClassification.Property => SemanticTokenType.Property,
            SemanticClassification.Field => SemanticTokenType.Variable,
            SemanticClassification.Event => SemanticTokenType.Event,
            SemanticClassification.NullableAnnotation => SemanticTokenType.Operator,
            _ => null!
        };

    private static SemanticTokenType? MapTokenType(
        SemanticClassification classification,
        SyntaxToken token,
        SemanticModel semanticModel,
        SyntaxNode root)
    {
        if (classification == SemanticClassification.Type)
        {
            var symbol = TryGetAssociatedSymbol(token, semanticModel);
            if (symbol is ITypeSymbol typeSymbol)
            {
                return typeSymbol.TypeKind switch
                {
                    TypeKind.Class => SemanticTokenType.Class,
                    TypeKind.Struct => SemanticTokenType.Struct,
                    TypeKind.Interface => SemanticTokenType.Interface,
                    TypeKind.Enum => SemanticTokenType.Enum,
                    _ => SemanticTokenType.Type
                };
            }

            return token.Parent switch
            {
                ClassDeclarationSyntax => SemanticTokenType.Class,
                StructDeclarationSyntax or RecordDeclarationSyntax or UnionDeclarationSyntax => SemanticTokenType.Struct,
                InterfaceDeclarationSyntax => SemanticTokenType.Interface,
                EnumDeclarationSyntax => SemanticTokenType.Enum,
                _ => SemanticTokenType.Type
            };
        }

        if (classification == SemanticClassification.StringLiteral &&
            TryGetStringSyntax(token, semanticModel, root, out var stringSyntax) &&
            string.Equals(stringSyntax, "Regex", StringComparison.Ordinal))
        {
            return SemanticTokenType.Regexp;
        }

        return MapTokenType(classification);
    }

    private static bool TryGetStringSyntax(
        SyntaxToken token,
        SemanticModel semanticModel,
        SyntaxNode root,
        out string syntax)
    {
        if (TryGetStringSyntax(token, semanticModel, out syntax))
            return true;

        var rootedToken = root.FindToken(token.Span.Start);
        return rootedToken.Span == token.Span &&
            TryGetStringSyntax(rootedToken, semanticModel, out syntax);
    }

    private static bool TryGetStringSyntax(SyntaxToken token, SemanticModel semanticModel, out string syntax)
    {
        syntax = string.Empty;

        var expression = token.Parent switch
        {
            LiteralExpressionSyntax literal => literal,
            InterpolatedStringExpressionSyntax interpolated => interpolated,
            InterpolatedStringTextSyntax text => text.AncestorsAndSelf().OfType<InterpolatedStringExpressionSyntax>().FirstOrDefault(),
            _ => token.Parent?.AncestorsAndSelf().OfType<ExpressionSyntax>().FirstOrDefault()
        };

        if (expression is null)
            return false;

        var argument = expression.AncestorsAndSelf().OfType<ArgumentSyntax>().FirstOrDefault();
        if (argument is null || argument.Expression.Span != expression.Span)
            return false;

        var invocation = argument.Parent?.Parent as InvocationExpressionSyntax;
        if (invocation is null)
            return false;

        var method = TryGetInvocationMethod(invocation, semanticModel);
        if (method is null)
            return false;

        var parameter = TryGetArgumentParameter(invocation.ArgumentList, argument, method);
        if (parameter is null)
            return false;

        foreach (var attribute in parameter.GetAttributes())
        {
            if (!IsStringSyntaxAttribute(attribute))
                continue;

            var syntaxArgument = attribute.ConstructorArguments.FirstOrDefault();
            if (syntaxArgument.Value is string value && !string.IsNullOrWhiteSpace(value))
            {
                syntax = value;
                return true;
            }
        }

        return TryGetStringSyntaxFromParameterSyntax(parameter, out syntax);
    }

    private static bool TryGetStringSyntaxFromParameterSyntax(IParameterSymbol parameter, out string syntax)
    {
        syntax = string.Empty;

        foreach (var reference in parameter.DeclaringSyntaxReferences)
        {
            if (reference.GetSyntax() is not ParameterSyntax parameterSyntax)
                continue;

            foreach (var attribute in parameterSyntax.AttributeLists.SelectMany(static list => list.Attributes))
            {
                var attributeName = attribute.Name.ToString();
                if (!attributeName.EndsWith("StringSyntax", StringComparison.Ordinal) &&
                    !attributeName.EndsWith("StringSyntaxAttribute", StringComparison.Ordinal))
                {
                    continue;
                }

                var argumentExpression = attribute.ArgumentList?.Arguments.FirstOrDefault()?.Expression;
                if (argumentExpression is LiteralExpressionSyntax { Token.Value: string literalValue })
                {
                    syntax = literalValue;
                    return !string.IsNullOrWhiteSpace(syntax);
                }

                if (argumentExpression is MemberAccessExpressionSyntax memberAccess)
                {
                    syntax = memberAccess.Name.Identifier.ValueText;
                    return !string.IsNullOrWhiteSpace(syntax);
                }
            }
        }

        return false;
    }

    private static IMethodSymbol? TryGetInvocationMethod(InvocationExpressionSyntax invocation, SemanticModel semanticModel)
    {
        var invocationInfo = semanticModel.GetSymbolInfo(invocation);
        if (invocationInfo.Symbol is IMethodSymbol invocationMethod)
            return invocationMethod;

        var candidate = invocationInfo.CandidateSymbols.OfType<IMethodSymbol>().FirstOrDefault();
        if (candidate is not null)
            return candidate;

        var expressionInfo = semanticModel.GetSymbolInfo(invocation.Expression);
        if (expressionInfo.Symbol is IMethodSymbol expressionMethod)
            return expressionMethod;

        return expressionInfo.CandidateSymbols.OfType<IMethodSymbol>().FirstOrDefault();
    }

    private static IParameterSymbol? TryGetArgumentParameter(
        ArgumentListSyntax argumentList,
        ArgumentSyntax argument,
        IMethodSymbol method)
    {
        var name = argument.NameColon?.Name.Identifier.ValueText;
        if (!string.IsNullOrWhiteSpace(name))
        {
            return method.Parameters.FirstOrDefault(parameter =>
                string.Equals(parameter.Name, name, StringComparison.Ordinal));
        }

        for (var i = 0; i < argumentList.Arguments.Count; i++)
        {
            if (argumentList.Arguments[i].Span != argument.Span)
                continue;

            return i < method.Parameters.Length ? method.Parameters[i] : null;
        }

        return null;
    }

    private static bool IsStringSyntaxAttribute(AttributeData attribute)
        => string.Equals(
            attribute.AttributeClass.ToFullyQualifiedMetadataName(),
            "System.Diagnostics.CodeAnalysis.StringSyntaxAttribute",
            StringComparison.Ordinal);

    private static ImmutableArray<SemanticTokenModifier> GetModifiers(SyntaxToken token, SemanticModel semanticModel)
    {
        var symbol = TryGetAssociatedSymbol(token, semanticModel);
        if (symbol is null)
            return ImmutableArray<SemanticTokenModifier>.Empty;

        var modifiers = ImmutableArray.CreateBuilder<SemanticTokenModifier>();
        if (IsDeclarationToken(token))
            modifiers.Add(SemanticTokenModifier.Declaration);

        if (symbol.IsStatic)
            modifiers.Add(SemanticTokenModifier.Static);

        if (symbol is IMethodSymbol { IsReadOnly: true } or IFieldSymbol { IsReadOnly: true })
            modifiers.Add(SemanticTokenModifier.Readonly);

        if (symbol is IMethodSymbol { IsAsync: true })
            modifiers.Add(SemanticTokenModifier.Async);

        if (symbol is INamedTypeSymbol { IsAbstract: true } or IMethodSymbol { IsAbstract: true })
            modifiers.Add(SemanticTokenModifier.Abstract);

        return modifiers.ToImmutable();
    }

    private static ISymbol? TryGetAssociatedSymbol(SyntaxToken token, SemanticModel semanticModel)
    {
        if (token.Kind != SyntaxKind.IdentifierToken)
            return null;

        var bindNode = GetBindableParent(token);
        if (bindNode is null)
            return null;

        var info = semanticModel.GetSymbolInfo(bindNode);
        return semanticModel.GetDeclaredSymbol(bindNode)
            ?? info.Symbol
            ?? info.CandidateSymbols.FirstOrDefault();
    }

    private static bool IsDeclarationToken(SyntaxToken token)
        => token.Parent switch
        {
            ClassDeclarationSyntax declaration => declaration.Identifier == token,
            StructDeclarationSyntax declaration => declaration.Identifier == token,
            RecordDeclarationSyntax declaration => declaration.Identifier == token,
            InterfaceDeclarationSyntax declaration => declaration.Identifier == token,
            EnumDeclarationSyntax declaration => declaration.Identifier == token,
            UnionDeclarationSyntax declaration => declaration.Identifier == token,
            DelegateDeclarationSyntax declaration => declaration.Identifier == token,
            MethodDeclarationSyntax declaration => declaration.Identifier == token,
            PropertyDeclarationSyntax declaration => declaration.Identifier == token,
            EventDeclarationSyntax declaration => declaration.Identifier == token,
            ParameterSyntax declaration => declaration.Identifier == token,
            VariableDeclaratorSyntax declaration => declaration.Identifier == token,
            _ => false
        };

    private static SyntaxNode? GetBindableParent(SyntaxToken token)
    {
        var node = token.Parent;

        if (node is IdentifierNameSyntax && node.Parent is NamespaceDeclarationSyntax ns && ns.Name == node)
            return ns;

        while (node is not null)
        {
            if (node.Parent is MemberAccessExpressionSyntax memberAccess && memberAccess.Name == node)
            {
                node = memberAccess;
                continue;
            }

            if (node.Parent is MemberBindingExpressionSyntax memberBinding && memberBinding.Name == node)
            {
                node = memberBinding;
                continue;
            }

            if (node.Parent is InvocationExpressionSyntax invocation && invocation.Expression == node)
            {
                node = invocation;
                continue;
            }

            break;
        }

        return node;
    }

    private static void PushSpan(
        SemanticTokensBuilder builder,
        SourceText sourceText,
        TextSpan span,
        SemanticTokenType tokenType,
        ImmutableArray<SemanticTokenModifier> modifiers)
    {
        if (span.Length <= 0)
            return;

        var (startLine, startColumn) = sourceText.GetLineAndColumn(span.Start);
        var (endLine, endColumn) = sourceText.GetLineAndColumn(span.End);

        startLine--;
        startColumn--;
        endLine--;
        endColumn--;

        if (startLine == endLine)
        {
            builder.Push(startLine, startColumn, Math.Max(0, endColumn - startColumn), tokenType, modifiers);
            return;
        }

        for (var line = startLine; line <= endLine; line++)
        {
            var lineLength = sourceText.GetLineLength(line);
            var segmentStart = line == startLine ? startColumn : 0;
            var segmentEnd = line == endLine ? endColumn : lineLength;
            var length = Math.Max(0, segmentEnd - segmentStart);
            if (length == 0)
                continue;

            builder.Push(line, segmentStart, length, tokenType, modifiers);
        }
    }

    private sealed record SemanticTokenEntry(
        TextSpan Span,
        SemanticTokenType? TokenType,
        ImmutableArray<SemanticTokenModifier> Modifiers);

    private readonly record struct SemanticTokensCacheKey(string Uri, VersionStamp Version);
}
