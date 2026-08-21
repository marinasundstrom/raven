using MediatR;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.JsonRpc;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Text;

namespace Raven.LanguageServer;

using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

[Method(MethodName, Direction.ClientToServer)]
internal sealed record MacroEmbeddedLanguageProjectionParams : IRequest<MacroEmbeddedLanguageProjectionResponse?>
{
    public const string MethodName = "raven/macroEmbeddedLanguageProjection";

    public required TextDocumentIdentifier TextDocument { get; init; }

    public required Position Position { get; init; }
}

internal sealed record MacroEmbeddedLanguageProjectionResponse(
    string LanguageId,
    string Text,
    LspRange Range);

internal sealed class MacroEmbeddedLanguageProjectionHandler :
    IJsonRpcRequestHandler<MacroEmbeddedLanguageProjectionParams, MacroEmbeddedLanguageProjectionResponse?>
{
    private readonly DocumentStore _documents;
    private readonly ILogger<MacroEmbeddedLanguageProjectionHandler> _logger;

    public MacroEmbeddedLanguageProjectionHandler(
        DocumentStore documents,
        ILogger<MacroEmbeddedLanguageProjectionHandler> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    public async Task<MacroEmbeddedLanguageProjectionResponse?> Handle(
        MacroEmbeddedLanguageProjectionParams request,
        CancellationToken cancellationToken)
    {
        try
        {
            var context = await _documents.GetAnalysisContextAsync(
                request.TextDocument.Uri,
                request.Position,
                cancellationToken).ConfigureAwait(false);
            if (context is null)
                return null;

            using var semanticAccess = await _documents.EnterDocumentSemanticModelAccessAsync(
                request.TextDocument.Uri,
                context.Value,
                cancellationToken,
                "macroEmbeddedLanguageProjection").ConfigureAwait(false);
            var semanticModel = semanticAccess.SemanticModel;
            if (semanticModel is null)
                return null;

            var root = context.Value.SyntaxTree.GetRoot(cancellationToken);
            var offset = Math.Clamp(
                PositionHelper.ToOffset(context.Value.SourceText, request.Position),
                0,
                root.FullSpan.End);
            var projection = semanticModel.GetMacroEmbeddedLanguageProjection(offset, cancellationToken);
            return projection is null
                ? null
                : CreateResponse(context.Value.SourceText, projection);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return null;
        }
        catch (Exception exception)
        {
            _logger.LogError(
                exception,
                "Embedded-language projection request failed for {Uri} at {Line}:{Character}.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character);
            return null;
        }
    }

    internal static MacroEmbeddedLanguageProjectionResponse CreateResponse(
        SourceText sourceText,
        MacroEmbeddedLanguageProjection projection)
        => CreateResponse(sourceText, projection.LanguageId, projection.Text, projection.Span);

    internal static MacroEmbeddedLanguageProjectionResponse CreateResponse(
        SourceText sourceText,
        string languageId,
        string text,
        TextSpan span)
        => new(
            languageId,
            text,
            PositionHelper.ToRange(sourceText, span));
}
