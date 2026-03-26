using System.Collections.Concurrent;
using System.Collections.Immutable;

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
    internal static readonly SemanticTokensLegend Legend = new()
    {
        TokenTypes = new Container<SemanticTokenType>(
            SemanticTokenType.Keyword,
            SemanticTokenType.String,
            SemanticTokenType.Number,
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
    private readonly ConcurrentDictionary<string, SemanticTokensDocument> _tokenDocuments = new(StringComparer.Ordinal);

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
            Range = true
        };

    protected override async Task<SemanticTokensDocument> GetSemanticTokensDocument(ITextDocumentIdentifierParams @params, CancellationToken cancellationToken)
    {
        await Task.CompletedTask.ConfigureAwait(false);
        return _tokenDocuments.GetOrAdd(
            @params.TextDocument.Uri.ToString(),
            static _ => new SemanticTokensDocument(Legend));
    }

    protected override async Task Tokenize(SemanticTokensBuilder builder, ITextDocumentIdentifierParams identifier, CancellationToken cancellationToken)
    {
        try
        {
            using var _ = await _documents.EnterCompilerAccessAsync(cancellationToken).ConfigureAwait(false);
            var context = await _documents.GetAnalysisContextAsync(identifier.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            if (context is null)
                return;

            var compilation = context.Value.Compilation;
            var syntaxTree = context.Value.SyntaxTree;
            var sourceText = context.Value.SourceText;
            var root = syntaxTree.GetRoot(cancellationToken);
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            var classification = SemanticClassifier.Classify(root, semanticModel);

            var tokenEntries = classification.Tokens
                .Select(pair => CreateEntry(pair.Key.Span, pair.Value, pair.Key, semanticModel))
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

            foreach (var entry in entries)
                PushSpan(builder, sourceText, entry.Span, entry.TokenType!.Value, entry.Modifiers);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Semantic token request failed for {Uri}.", identifier.TextDocument.Uri);
        }
    }

    private static SemanticTokenEntry? CreateEntry(
        TextSpan span,
        SemanticClassification classification,
        SyntaxToken token,
        SemanticModel semanticModel)
    {
        var tokenType = MapTokenType(classification, token, semanticModel);
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
        SemanticModel semanticModel)
    {
        if (classification == SemanticClassification.Type)
            return MapTypeTokenType(token, semanticModel);

        return MapTokenType(classification);
    }

    private static SemanticTokenType MapTypeTokenType(SyntaxToken token, SemanticModel semanticModel)
    {
        var bindNode = GetSemanticTokenBindableNode(token);
        if (bindNode is not null)
        {
            var info = semanticModel.GetSymbolInfo(bindNode);
            var symbol = info.Symbol
                ?? info.CandidateSymbols.FirstOrDefault()
                ?? semanticModel.GetDeclaredSymbol(bindNode);

            if (symbol is ITypeSymbol typeSymbol)
                return typeSymbol.TypeKind switch
                {
                    TypeKind.Class => SemanticTokenType.Class,
                    TypeKind.Struct => SemanticTokenType.Struct,
                    TypeKind.Interface => SemanticTokenType.Interface,
                    TypeKind.Enum => SemanticTokenType.Enum,
                    _ => SemanticTokenType.Type
                };
        }

        return SemanticTokenType.Type;
    }

    private static ImmutableArray<SemanticTokenModifier> GetModifiers(SyntaxToken token, SemanticModel semanticModel)
    {
        var bindNode = GetSemanticTokenBindableNode(token);
        if (bindNode is null)
            return ImmutableArray<SemanticTokenModifier>.Empty;

        var info = semanticModel.GetSymbolInfo(bindNode);
        var symbol = info.Symbol
            ?? info.CandidateSymbols.FirstOrDefault()
            ?? semanticModel.GetDeclaredSymbol(bindNode);

        if (symbol is null)
            return ImmutableArray<SemanticTokenModifier>.Empty;

        var builder = ImmutableArray.CreateBuilder<SemanticTokenModifier>(4);

        if (semanticModel.GetDeclaredSymbol(bindNode) is not null)
            builder.Add(SemanticTokenModifier.Declaration);

        if (symbol.IsStatic)
            builder.Add(SemanticTokenModifier.Static);

        if (symbol is IMethodSymbol { IsAbstract: true } ||
            symbol is INamedTypeSymbol { IsAbstract: true })
        {
            builder.Add(SemanticTokenModifier.Abstract);
        }

        if (symbol is IMethodSymbol { IsAsync: true })
            builder.Add(SemanticTokenModifier.Async);

        if (symbol is IFieldSymbol { IsReadOnly: true } ||
            symbol is IMethodSymbol { IsReadOnly: true } ||
            symbol is IPropertySymbol { IsMutable: false })
        {
            builder.Add(SemanticTokenModifier.Readonly);
        }

        return builder.ToImmutable();
    }

    private static SyntaxNode? GetSemanticTokenBindableNode(SyntaxToken token)
    {
        var node = token.Parent;

        if (node is IdentifierNameSyntax && node.Parent is NamespaceDeclarationSyntax ns && ns.Name == node)
            return ns;

        while (node is not null)
        {
            if (node.Parent is MemberAccessExpressionSyntax memberAccess && memberAccess.Name == node)
                node = memberAccess;
            else if (node.Parent is MemberBindingExpressionSyntax memberBinding && memberBinding.Name == node)
                node = memberBinding;
            else if (node.Parent is InvocationExpressionSyntax invocation && invocation.Expression == node)
                node = invocation;
            else
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
}
