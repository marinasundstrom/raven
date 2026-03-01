using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class HoverHandler : IHoverHandler
{
    private readonly DocumentStore _documents;
    private readonly ILogger<HoverHandler> _logger;

    public HoverHandler(DocumentStore documents, ILogger<HoverHandler> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    public HoverRegistrationOptions GetRegistrationOptions(HoverCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven")
        };

    public void SetCapability(HoverCapability capability)
    {
    }

    public async Task<Hover?> Handle(HoverParams request, CancellationToken cancellationToken)
    {
        try
        {
            if (!_documents.TryGetDocument(request.TextDocument.Uri, out var document))
                return null;

            var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            if (syntaxTree is null)
                return null;

            if (!_documents.TryGetCompilation(request.TextDocument.Uri, out var compilation) || compilation is null)
                return null;

            var sourceText = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            var root = syntaxTree.GetRoot(cancellationToken);
            var offset = PositionHelper.ToOffset(sourceText, request.Position);

            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            if (resolution is null)
                return null;

            var symbol = resolution.Value.Symbol;
            var signature = BuildSignature(symbol);
            var containing = BuildContainingDisplay(symbol);
            var documentation = symbol.GetDocumentationComment();
            var functionCaptures = semanticModel.GetCapturedVariables(symbol);
            var isCapturedVariable = semanticModel.IsCapturedVariable(symbol);
            var hoverText = BuildHoverText(
                signature,
                BuildKindDisplay(symbol),
                containing,
                documentation,
                functionCaptures,
                isCapturedVariable);

            return new Hover
            {
                Contents = new MarkedStringsOrMarkupContent(new MarkupContent
                {
                    Kind = MarkupKind.Markdown,
                    Value = hoverText
                }),
                Range = PositionHelper.ToRange(sourceText, resolution.Value.Node.Span)
            };
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return null;
        }
        catch (Exception ex)
        {
            _logger.LogError(
                ex,
                "Hover request failed for {Uri} at {Line}:{Character}.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character);
            return null;
        }
    }

    private static string BuildHoverText(
        string signature,
        string kind,
        string? containing,
        DocumentationComment? documentation,
        ImmutableArray<ISymbol> capturedVariables,
        bool isCapturedVariable)
    {
        var docsText = FormatDocumentation(documentation);
        var captureText = FormatCaptureText(capturedVariables, isCapturedVariable);
        var contextText = !string.IsNullOrWhiteSpace(containing)
            ? $"{kind} in `{containing}`"
            : kind;

        var parts = new List<string>
        {
            $"```raven\n{signature}\n```",
            contextText
        };

        if (!string.IsNullOrWhiteSpace(captureText))
            parts.Add(captureText);

        if (!string.IsNullOrWhiteSpace(docsText))
            parts.Add($"---\n\n{docsText}");

        return string.Join("\n\n", parts);
    }

    private static string BuildSignature(ISymbol symbol)
    {
        var plainTypeFormat = SymbolDisplayFormat.RavenSignatureFormat
            .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
            .WithKindOptions(SymbolDisplayKindOptions.None);

        if (symbol is IMethodSymbol { MethodKind: MethodKind.LambdaMethod } lambda)
        {
            var parameters = string.Join(
                ", ",
                lambda.Parameters.Select(parameter =>
                {
                    var parameterType = parameter.Type.ToDisplayString(plainTypeFormat);
                    return $"{parameter.Name}: {parameterType}";
                }));
            var returnType = lambda.ReturnType.ToDisplayString(plainTypeFormat);
            return $"({parameters}) -> {returnType}";
        }

        if (symbol is IParameterSymbol parameter)
        {
            var binding = parameter.IsMutable ? "var" : "val";
            var parameterType = parameter.Type.ToDisplayString(plainTypeFormat);
            return $"{binding} {parameter.Name}: {parameterType}";
        }

        if (symbol is ILocalSymbol local)
        {
            var binding = local.IsMutable ? "var" : "val";
            var localType = local.Type.ToDisplayString(plainTypeFormat);
            return $"{binding} {local.Name}: {localType}";
        }

        if (symbol is ITypeSymbol typeSymbol)
            return typeSymbol.ToDisplayString(plainTypeFormat);

        return symbol.ToDisplayString(SymbolDisplayFormat.RavenTooltipFormat);
    }

    private static string? BuildContainingDisplay(ISymbol symbol)
    {
        var containing = GetUserFacingContainingSymbol(symbol);
        return containing?.ToDisplayString(
            SymbolDisplayFormat.RavenSignatureFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly));
    }

    private static ISymbol? GetUserFacingContainingSymbol(ISymbol symbol)
    {
        var containing = symbol.ContainingSymbol;
        while (containing is IMethodSymbol { MethodKind: MethodKind.LambdaMethod } lambdaContainer)
            containing = lambdaContainer.ContainingSymbol;
        return containing;
    }

    private static string BuildKindDisplay(ISymbol symbol)
    {
        if (symbol is IMethodSymbol { MethodKind: MethodKind.LambdaMethod })
            return "Lambda";

        return symbol.Kind.ToString();
    }

    private static string? FormatDocumentation(DocumentationComment? documentation)
    {
        if (documentation is null)
            return null;

        if (string.IsNullOrWhiteSpace(documentation.Content))
            return null;

        return documentation.Format switch
        {
            DocumentationFormat.Markdown => documentation.Content.Trim(),
            DocumentationFormat.Xml => $"```xml\n{documentation.Content.Trim()}\n```",
            _ => documentation.Content.Trim()
        };
    }

    private static string? FormatCaptureText(
        ImmutableArray<ISymbol> capturedVariables,
        bool isCapturedVariable)
    {
        if (capturedVariables.IsDefaultOrEmpty)
            return isCapturedVariable ? "Captured variable" : null;

        var captures = string.Join(
            ", ",
            capturedVariables
                .Select(static symbol => symbol.Name)
                .Where(static name => !string.IsNullOrWhiteSpace(name))
                .Distinct(StringComparer.Ordinal));

        if (string.IsNullOrWhiteSpace(captures))
            return isCapturedVariable ? "Captured variable" : null;

        return $"Captures: `{captures}`";
    }
}
