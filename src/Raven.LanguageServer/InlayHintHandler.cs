using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class InlayHintHandler : IInlayHintsHandler
{
    private readonly DocumentStore _documents;
    private readonly ILogger<InlayHintHandler> _logger;

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

    public async Task<InlayHintContainer> Handle(InlayHintParams request, CancellationToken cancellationToken)
    {
        try
        {
            using var _ = await _documents.EnterDocumentSemanticAccessAsync(
                request.TextDocument.Uri,
                cancellationToken,
                "inlayHint").ConfigureAwait(false);

            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            if (context is null)
                return new InlayHintContainer();

            var semanticModel = await _documents.GetSemanticModelAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            if (semanticModel is null)
                return new InlayHintContainer();

            var sourceText = context.Value.SourceText;
            var root = context.Value.SyntaxTree.GetRoot(cancellationToken);
            var requestSpan = GetRequestedSpan(sourceText, request.Range);

            var hints = new List<InlayHint>();
            AddLocalTypeHints(hints, semanticModel, root, sourceText, requestSpan);
            AddReturnTypeHints(hints, semanticModel, root, sourceText, requestSpan);

            return new InlayHintContainer(hints);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return new InlayHintContainer();
        }
        catch (Exception ex)
        {
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
    }

    private static void AddLocalTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan)
    {
        foreach (var declarator in root.DescendantNodes().OfType<VariableDeclaratorSyntax>())
        {
            if (declarator.TypeAnnotation is not null ||
                declarator.Identifier.IsMissing ||
                string.IsNullOrWhiteSpace(declarator.Identifier.ValueText) ||
                !declarator.Ancestors().OfType<LocalDeclarationStatementSyntax>().Any())
            {
                continue;
            }

            var insertionPosition = declarator.Identifier.Span.End;
            if (!ContainsPosition(requestSpan, insertionPosition))
                continue;

            if (semanticModel.GetDeclaredSymbol(declarator) is not ILocalSymbol local ||
                !TryFormatType(local.Type, out var typeDisplay))
            {
                continue;
            }

            hints.Add(CreateTypeHint(sourceText, insertionPosition, $": {typeDisplay}"));
        }
    }

    private static void AddReturnTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan)
    {
        foreach (var method in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            AddReturnTypeHint(
                hints,
                semanticModel,
                sourceText,
                requestSpan,
                method,
                method.ReturnType,
                method.ParameterList,
                method.Body ?? (SyntaxNode?)method.ExpressionBody);
        }

        foreach (var function in root.DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            AddReturnTypeHint(
                hints,
                semanticModel,
                sourceText,
                requestSpan,
                function,
                function.ReturnType,
                function.ParameterList,
                function.Body ?? (SyntaxNode?)function.ExpressionBody);
        }
    }

    private static void AddReturnTypeHint(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SourceText sourceText,
        TextSpan requestSpan,
        SyntaxNode declaration,
        ArrowTypeClauseSyntax? returnType,
        ParameterListSyntax parameterList,
        SyntaxNode? body)
    {
        if (returnType is not null || body is null)
            return;

        var insertionPosition = parameterList.Span.End;
        if (!ContainsPosition(requestSpan, insertionPosition))
            return;

        if (semanticModel.GetDeclaredSymbol(declaration) is not IMethodSymbol ||
            !TryGetInferredReturnType(semanticModel, body, out var inferredReturnType) ||
            !TryFormatType(inferredReturnType, out var typeDisplay))
        {
            return;
        }

        hints.Add(CreateTypeHint(sourceText, insertionPosition, $" -> {typeDisplay}"));
    }

    private static InlayHint CreateTypeHint(SourceText sourceText, int insertionPosition, string text)
    {
        var range = PositionHelper.ToRange(sourceText, new TextSpan(insertionPosition, 0));
        return new InlayHint
        {
            Position = range.Start,
            Label = text,
            Kind = InlayHintKind.Type,
            PaddingLeft = false,
            PaddingRight = false,
            TextEdits = new Container<TextEdit>(new TextEdit
            {
                Range = range,
                NewText = text
            })
        };
    }

    private static bool TryFormatType(ITypeSymbol? type, out string display)
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
                    .Select(FormatSingleType)
                    .OrderBy(static value => value, StringComparer.Ordinal))
            : FormatSingleType(type);

        return !string.IsNullOrWhiteSpace(display);
    }

    private static string FormatSingleType(ITypeSymbol type)
        => type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);

    private static bool TryGetInferredReturnType(SemanticModel semanticModel, SyntaxNode body, out ITypeSymbol? inferredReturnType)
    {
        inferredReturnType = ReturnTypeCollector.Infer(semanticModel.GetBoundNode(body));
        return inferredReturnType is not null &&
            inferredReturnType.SpecialType is not (SpecialType.System_Unit or SpecialType.System_Void);
    }

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
}
