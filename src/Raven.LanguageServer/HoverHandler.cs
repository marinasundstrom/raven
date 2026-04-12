using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class HoverHandler : IHoverHandler
{
    private const int MaxCachedHoverEntries = 512;
    private const double SlowHoverThresholdMs = 250;
    private const double HoverLifecycleLogThresholdMs = 100;

    private readonly DocumentStore _documents;
    private readonly ILogger<HoverHandler> _logger;
    private readonly ConcurrentDictionary<HoverCacheKey, HoverCacheEntry> _hoverCache = new();

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

    internal void InvalidateDocument(DocumentUri uri)
    {
        var uriText = uri.ToString();
        foreach (var key in _hoverCache.Keys)
        {
            if (string.Equals(key.Uri, uriText, StringComparison.Ordinal))
                _hoverCache.TryRemove(key, out _);
        }
    }

    public async Task<Hover?> Handle(HoverParams request, CancellationToken cancellationToken)
    {
        var totalStopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double analysisContextMs = 0;
        double semanticModelMs = 0;
        double resolutionMs = 0;

        try
        {
            var stageStopwatch = Stopwatch.StartNew();
            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            analysisContextMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (context is null)
                return null;

            var syntaxTree = context.Value.SyntaxTree;
            var sourceText = context.Value.SourceText;
            var cacheKey = new HoverCacheKey(
                request.TextDocument.Uri.ToString(),
                context.Value.Document.Version,
                request.Position.Line,
                request.Position.Character);

            if (_hoverCache.TryGetValue(cacheKey, out var cachedEntry))
            {
                LogTrackedHover(
                    request.TextDocument.Uri.GetFileSystemPath(),
                    request.Position,
                    "cache-hit",
                    context.Value.Document.Version,
                    null,
                    cachedEntry.Hover?.Range,
                    null,
                    null);

                LanguageServerPerformanceInstrumentation.RecordOperation(
                    "hover",
                    request.TextDocument.Uri,
                    null,
                    totalStopwatch.Elapsed.TotalMilliseconds,
                    cacheHit: true,
                    stages:
                    [
                        new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                        new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", analysisContextMs)
                    ]);
                return cachedEntry.Hover;
            }

            var root = syntaxTree.GetRoot(cancellationToken);
            var offset = Math.Clamp(PositionHelper.ToOffset(sourceText, request.Position), 0, root.FullSpan.End);

            var syntaxOnlyHover = TryBuildFunctionExpressionDeclarationHover(sourceText, root, offset);
            if (syntaxOnlyHover is not null)
                return CacheHover(cacheKey, syntaxOnlyHover);

            var gateWaitStopwatch = Stopwatch.StartNew();
            using var semanticLease = await _documents.TryEnterDocumentSemanticAccessAsync(request.TextDocument.Uri, cancellationToken, "hover").ConfigureAwait(false);
            gateWaitMs = gateWaitStopwatch.Elapsed.TotalMilliseconds;
            if (semanticLease is null)
                return null;

            stageStopwatch.Restart();
            var semanticModel = await _documents.GetSemanticModelAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            semanticModelMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (semanticModel is null)
                return null;

            var macroHover = TryBuildMacroExpansionHover(sourceText, semanticModel, root, offset);
            if (macroHover is not null)
                return CacheHover(cacheKey, macroHover);

            var literalHover = TryBuildLiteralHover(sourceText, semanticModel, root, offset);
            if (literalHover is not null)
                return CacheHover(cacheKey, literalHover);

            var patternHover = TryBuildPatternDeclarationHover(sourceText, semanticModel, root, offset);
            if (patternHover is not null)
                return CacheHover(cacheKey, patternHover);

            stageStopwatch.Restart();
            var resolution = TryResolveDeclaredHoverSymbol(semanticModel, root, offset)
                ?? SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            resolutionMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (resolution is null)
                return CacheHover(cacheKey, null);

            var symbol = resolution.Value.Symbol;
            var signature = BuildDisplaySignatureForResolvedHover(resolution.Value, semanticModel, root, offset);
            if (signature == "()" &&
                resolution.Value.Node is IdentifierNameSyntax identifier &&
                (resolution.Value.Kind == SymbolResolutionKind.TypePosition ||
                 identifier.AncestorsAndSelf().OfType<TypeSyntax>().Any()))
            {
                _logger.LogWarning(
                    "Suspicious unit hover signature for {ResolutionKind} identifier {Identifier} in {Uri} at {Line}:{Character}. SymbolKind={SymbolKind} SymbolDisplay={SymbolDisplay}",
                    resolution.Value.Kind,
                    identifier.Identifier.ValueText,
                    request.TextDocument.Uri,
                    request.Position.Line,
                    request.Position.Character,
                    symbol.Kind,
                    symbol.ToDisplayString(SymbolDisplayFormat.RavenTooltipFormat));
            }
            var containing = BuildContainingDisplay(symbol, semanticModel);
            var documentation = symbol.GetDocumentationComment();
            var functionCaptures = semanticModel.GetCapturedVariables(symbol);
            if (functionCaptures.IsDefaultOrEmpty)
                functionCaptures = semanticModel.GetCapturedVariables(resolution.Value.Node);
            var isCapturedVariable = semanticModel.IsCapturedVariable(symbol);
            var hoverText = BuildHoverText(
                signature,
                BuildKindDisplayForResolution(resolution.Value.Kind, symbol),
                containing,
                documentation,
                functionCaptures,
                isCapturedVariable);

            var hoverRange = PositionHelper.ToRange(sourceText, GetHoverSpanForResolution(resolution.Value));
            LogTrackedHover(
                request.TextDocument.Uri.GetFileSystemPath(),
                request.Position,
                resolution.Value.Kind.ToString(),
                context.Value.Document.Version,
                symbol,
                hoverRange,
                resolution.Value.Node,
                signature);

            return CacheHover(cacheKey, new Hover
            {
                Contents = new MarkedStringsOrMarkupContent(new MarkupContent
                {
                    Kind = MarkupKind.Markdown,
                    Value = hoverText
                }),
                Range = hoverRange
            });
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            if (totalStopwatch.Elapsed.TotalMilliseconds >= HoverLifecycleLogThresholdMs)
            {
                _logger.LogInformation(
                    "Hover request canceled for {Uri} at {Line}:{Character} after {ElapsedMs:F1}ms.",
                    request.TextDocument.Uri,
                    request.Position.Line,
                    request.Position.Character,
                    totalStopwatch.Elapsed.TotalMilliseconds);
            }

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
        finally
        {
            totalStopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "hover",
                request.TextDocument.Uri,
                null,
                totalStopwatch.Elapsed.TotalMilliseconds,
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", analysisContextMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("semanticModel", semanticModelMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("resolution", resolutionMs)
                ]);

            if (totalStopwatch.Elapsed.TotalMilliseconds >= SlowHoverThresholdMs)
            {
                _logger.LogInformation(
                    "Slow hover for {Uri} at {Line}:{Character}: total={TotalMs:F1}ms gateWait={GateWaitMs:F1}ms context={ContextMs:F1}ms semanticModel={SemanticModelMs:F1}ms resolution={ResolutionMs:F1}ms.",
                    request.TextDocument.Uri,
                    request.Position.Line,
                    request.Position.Character,
                    totalStopwatch.Elapsed.TotalMilliseconds,
                    gateWaitMs,
                    analysisContextMs,
                    semanticModelMs,
                    resolutionMs);
            }
            else if (totalStopwatch.Elapsed.TotalMilliseconds >= HoverLifecycleLogThresholdMs)
            {
                _logger.LogDebug(
                    "Hover request completed for {Uri} at {Line}:{Character} in {TotalMs:F1}ms.",
                    request.TextDocument.Uri,
                    request.Position.Line,
                    request.Position.Character,
                    totalStopwatch.Elapsed.TotalMilliseconds);
            }
        }
    }

    private Hover? CacheHover(HoverCacheKey cacheKey, Hover? hover)
    {
        if (_hoverCache.Count >= MaxCachedHoverEntries)
            _hoverCache.Clear();

        _hoverCache[cacheKey] = new HoverCacheEntry(hover);
        return hover;
    }

    private void LogTrackedHover(
        string path,
        Position position,
        string outcome,
        VersionStamp documentVersion,
        ISymbol? symbol,
        OmniSharp.Extensions.LanguageServer.Protocol.Models.Range? range,
        SyntaxNode? node,
        string? signature)
    {
        if (!path.EndsWith("/samples/projects/efcore-expression-trees/src/main.rvn", StringComparison.Ordinal))
            return;

        _logger.LogInformation(
            "Tracked hover {Outcome} for {Path} at {Line}:{Character}. DocumentVersion={DocumentVersion} Symbol={SymbolKind}:{SymbolDisplay} Node={NodeType} NodeSpan={NodeSpanStart}-{NodeSpanEnd} Range={RangeStartLine}:{RangeStartCharacter}-{RangeEndLine}:{RangeEndCharacter} Signature={Signature}",
            outcome,
            path,
            position.Line,
            position.Character,
            documentVersion,
            symbol?.Kind.ToString() ?? "<null>",
            symbol?.ToDisplayString(SymbolDisplayFormat.RavenTooltipFormat) ?? "<null>",
            node?.GetType().Name ?? "<null>",
            node?.Span.Start,
            node?.Span.End,
            range?.Start.Line,
            range?.Start.Character,
            range?.End.Line,
            range?.End.Character,
            signature ?? "<null>");

        WriteTrackedHoverTrace(
            path,
            position,
            outcome,
            documentVersion,
            symbol,
            range,
            node,
            signature);
    }

    private static void WriteTrackedHoverTrace(
        string path,
        Position position,
        string outcome,
        VersionStamp documentVersion,
        ISymbol? symbol,
        OmniSharp.Extensions.LanguageServer.Protocol.Models.Range? range,
        SyntaxNode? node,
        string? signature)
    {
        try
        {
            var logPath = ResolveTraceLogPath();
            var line = new StringBuilder()
                .Append(DateTimeOffset.UtcNow.ToString("O"))
                .Append(" [HoverTrace] outcome=").Append(outcome)
                .Append(" path=").Append(path)
                .Append(" pos=").Append(position.Line).Append(':').Append(position.Character)
                .Append(" version=").Append(documentVersion.ToString())
                .Append(" symbolKind=").Append(symbol?.Kind.ToString() ?? "<null>")
                .Append(" symbol=").Append(symbol?.ToDisplayString(SymbolDisplayFormat.RavenTooltipFormat) ?? "<null>")
                .Append(" node=").Append(node?.GetType().Name ?? "<null>")
                .Append(" nodeSpan=").Append(node?.Span.Start.ToString() ?? "<null>").Append('-').Append(node?.Span.End.ToString() ?? "<null>")
                .Append(" range=").Append(range?.Start.Line.ToString() ?? "<null>").Append(':').Append(range?.Start.Character.ToString() ?? "<null>")
                .Append('-').Append(range?.End.Line.ToString() ?? "<null>").Append(':').Append(range?.End.Character.ToString() ?? "<null>")
                .Append(" signature=").Append(signature ?? "<null>")
                .AppendLine()
                .ToString();

            File.AppendAllText(logPath, line);
        }
        catch
        {
        }
    }

    private static string ResolveTraceLogPath()
    {
        var roots = new[]
        {
            TryFindRepositoryRoot(Directory.GetCurrentDirectory()),
            TryFindRepositoryRoot(AppContext.BaseDirectory)
        };

        var root = roots.FirstOrDefault(static path => !string.IsNullOrWhiteSpace(path))
            ?? Directory.GetCurrentDirectory();

        var logsDirectory = Path.Combine(root, "logs");
        Directory.CreateDirectory(logsDirectory);
        return Path.Combine(logsDirectory, "raven-lsp.log");
    }

    private static string? TryFindRepositoryRoot(string startPath)
    {
        if (string.IsNullOrWhiteSpace(startPath))
            return null;

        var directory = Directory.Exists(startPath)
            ? new DirectoryInfo(startPath)
            : new DirectoryInfo(Path.GetDirectoryName(startPath)!);

        while (directory is not null)
        {
            if (File.Exists(Path.Combine(directory.FullName, "Raven.sln")))
                return directory.FullName;

            directory = directory.Parent;
        }

        return null;
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

    private static Hover? TryBuildLiteralHover(SourceText sourceText, SemanticModel semanticModel, SyntaxNode root, int offset)
    {
        foreach (var candidateOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidateOffset);
            }
            catch
            {
                continue;
            }

            if (!LiteralHoverPreviewFormatter.TryCreatePreview(semanticModel, token, out var preview, out var span))
                continue;

            var hoverText = BuildHoverText(
                preview,
                kind: "Literal",
                containing: null,
                documentation: null,
                capturedVariables: ImmutableArray<ISymbol>.Empty,
                isCapturedVariable: false);

            return new Hover
            {
                Contents = new MarkedStringsOrMarkupContent(new MarkupContent
                {
                    Kind = MarkupKind.Markdown,
                    Value = hoverText
                }),
                Range = PositionHelper.ToRange(sourceText, span)
            };
        }

        return null;
    }

    private static Hover? TryBuildMacroExpansionHover(SourceText sourceText, SemanticModel semanticModel, SyntaxNode root, int offset)
    {
        if (!MacroExpansionDisplayService.TryCreateForOffset(sourceText, semanticModel, root, offset, out var display))
            return null;

        var parts = new List<string>
        {
            $"```raven\n{display.PreviewText}\n```",
            $"Macro `{display.InvocationDisplay}` expansion preview."
        };

        if (TryGetMacroHint(semanticModel.Compilation, display.MacroName, out var macroHint))
            parts.Add(macroHint);

        parts.Add("Use `Show macro expansion` to inspect the full expansion.");

        var hoverText = string.Join("\n\n", parts);

        return new Hover
        {
            Contents = new MarkedStringsOrMarkupContent(new MarkupContent
            {
                Kind = MarkupKind.Markdown,
                Value = hoverText
            }),
            Range = PositionHelper.ToRange(sourceText, display.Span)
        };
    }

    private static bool TryGetMacroHint(Compilation compilation, string macroName, out string hint)
    {
        foreach (var macroReference in compilation.MacroReferences)
        {
            IEnumerable<IRavenMacroPlugin> plugins;
            try
            {
                plugins = macroReference.GetPlugins().ToArray();
            }
            catch
            {
                continue;
            }

            foreach (var plugin in plugins)
            {
                ImmutableArray<IMacroDefinition> macros;
                try
                {
                    macros = plugin.GetMacros();
                }
                catch
                {
                    continue;
                }

                var macro = macros.FirstOrDefault(candidate => string.Equals(candidate.Name, macroName, StringComparison.Ordinal));
                if (macro is null)
                    continue;

                var kindDisplay = macro.Kind switch
                {
                    MacroKind.AttachedDeclaration => "Attached declaration macro",
                    MacroKind.FreestandingExpression => "Freestanding expression macro",
                    _ => "Macro"
                };
                var targetsDisplay = macro.Targets == MacroTarget.None
                    ? null
                    : $"Targets `{FormatMacroTargets(macro.Targets)}`.";
                var argumentsDisplay = macro.AcceptsArguments
                    ? "Accepts arguments."
                    : "No arguments.";

                hint = string.Join(
                    " ",
                    new[] { kindDisplay + ".", targetsDisplay, argumentsDisplay }.Where(static part => !string.IsNullOrWhiteSpace(part)));
                return true;
            }
        }

        hint = string.Empty;
        return false;
    }

    private static string FormatMacroTargets(MacroTarget targets)
    {
        return string.Join(
            ", ",
            Enum.GetValues<MacroTarget>()
                .Where(target => target != MacroTarget.None && targets.HasFlag(target))
                .Select(static target => target.ToString()));
    }

    private static Hover? TryBuildPatternDeclarationHover(SourceText sourceText, SemanticModel semanticModel, SyntaxNode root, int offset)
    {
        var plainTypeFormat = CreatePlainTypeFormat();

        foreach (var candidateOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidateOffset);
            }
            catch
            {
                continue;
            }

            if (token.Kind == SyntaxKind.IdentifierToken &&
                token.Parent?.AncestorsAndSelf().OfType<SingleVariableDesignationSyntax>().FirstOrDefault() is { } designation)
            {
                var declaredLocal = semanticModel.GetDeclaredSymbol(designation) as ILocalSymbol;
                var binding = declaredLocal is { IsMutable: true }
                    ? "var"
                    : designation.BindingKeyword.Kind == SyntaxKind.VarKeyword ? "var" : "val";
                string? patternTypeDisplay = null;
                if (declaredLocal?.Type is { TypeKind: not TypeKind.Error } declaredLocalType)
                {
                    patternTypeDisplay = declaredLocalType.ToDisplayString(plainTypeFormat);
                }
                else
                    if (designation.GetAncestor<DeclarationPatternSyntax>() is { } declarationPattern)
                    {
                        patternTypeDisplay = TryResolveTypeSymbolFromSyntax(semanticModel, declarationPattern.Type, out var declaredType)
                            ? declaredType.ToDisplayString(plainTypeFormat)
                            : declarationPattern.Type.ToString();
                    }
                    else if (TryInferPatternDeclaredLocalType(designation, semanticModel, out var designationType))
                    {
                        patternTypeDisplay = designationType.ToDisplayString(plainTypeFormat);
                    }

                if (patternTypeDisplay is null)
                    continue;

                var designationSignature = $"{binding} {designation.Identifier.ValueText}: {patternTypeDisplay}";
                var designationHoverText = BuildHoverText(
                    designationSignature,
                    kind: "Local",
                    containing: null,
                    documentation: null,
                    capturedVariables: ImmutableArray<ISymbol>.Empty,
                    isCapturedVariable: false);

                return new Hover
                {
                    Contents = new MarkedStringsOrMarkupContent(new MarkupContent
                    {
                        Kind = MarkupKind.Markdown,
                        Value = designationHoverText
                    }),
                    Range = PositionHelper.ToRange(sourceText, designation.Identifier.Span)
                };
            }

            if (token.Kind != SyntaxKind.IdentifierToken ||
                token.Parent is not IdentifierNameSyntax identifierName ||
                identifierName.Parent is not ConstantPatternSyntax)
            {
                continue;
            }

            var pattern = identifierName.AncestorsAndSelf().FirstOrDefault(static n => n is PositionalPatternSyntax or SequencePatternSyntax);
            if (pattern is null)
                continue;

            var patternAssignment = identifierName.AncestorsAndSelf().OfType<PatternDeclarationAssignmentStatementSyntax>().FirstOrDefault();
            if (patternAssignment is null)
                continue;

            var inferredType = InferPatternElementType(pattern, token, patternAssignment.Right, semanticModel);
            var typeDisplay = inferredType?.ToDisplayString(plainTypeFormat) ?? "Error";
            var signature = $"{token.ValueText}: {typeDisplay}";
            var hoverText = BuildHoverText(
                signature,
                kind: "Local",
                containing: null,
                documentation: null,
                capturedVariables: ImmutableArray<ISymbol>.Empty,
                isCapturedVariable: false);

            return new Hover
            {
                Contents = new MarkedStringsOrMarkupContent(new MarkupContent
                {
                    Kind = MarkupKind.Markdown,
                    Value = hoverText
                }),
                Range = PositionHelper.ToRange(sourceText, token.Span)
            };
        }

        return null;
    }

    private static SymbolResolutionResult? TryResolveDeclaredHoverSymbol(SemanticModel semanticModel, SyntaxNode root, int offset)
    {
        foreach (var candidateOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidateOffset);
            }
            catch
            {
                continue;
            }

            if (token.Kind != SyntaxKind.IdentifierToken)
                continue;

            if (token.Parent is SingleVariableDesignationSyntax single &&
                token == single.Identifier &&
                semanticModel.GetDeclaredSymbol(single) is { } singleSymbol)
            {
                return new SymbolResolutionResult(SymbolResolutionKind.Declaration, singleSymbol, single);
            }

            if (token.Parent is TypedVariableDesignationSyntax typed &&
                typed.Designation is SingleVariableDesignationSyntax typedSingle &&
                token == typedSingle.Identifier &&
                semanticModel.GetDeclaredSymbol(typedSingle) is { } typedSymbol)
            {
                return new SymbolResolutionResult(SymbolResolutionKind.Declaration, typedSymbol, typedSingle);
            }
        }

        return null;
    }

    private static Hover? TryBuildFunctionExpressionDeclarationHover(
        SourceText sourceText,
        SyntaxNode root,
        int offset)
    {
        foreach (var candidateOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidateOffset);
            }
            catch
            {
                continue;
            }

            if (token.Kind != SyntaxKind.IdentifierToken ||
                token.Parent is not FunctionExpressionSyntax functionExpression ||
                !IsFunctionExpressionIdentifierToken(functionExpression, token) ||
                IsInsideFunctionExpressionBody(functionExpression, token))
            {
                continue;
            }

            var signature = BuildFunctionExpressionSyntaxSignature(functionExpression);
            if (string.IsNullOrWhiteSpace(signature))
                continue;

            var containing = functionExpression.Ancestors()
                .OfType<TypeDeclarationSyntax>()
                .Select(static type => type.Identifier.ValueText)
                .FirstOrDefault();

            var hoverText = BuildHoverText(
                signature,
                kind: "Function",
                containing: containing,
                documentation: null,
                capturedVariables: ImmutableArray<ISymbol>.Empty,
                isCapturedVariable: false);

            return new Hover
            {
                Contents = new MarkedStringsOrMarkupContent(new MarkupContent
                {
                    Kind = MarkupKind.Markdown,
                    Value = hoverText
                }),
                Range = PositionHelper.ToRange(sourceText, token.Span)
            };
        }

        return null;
    }

    private static string BuildFunctionExpressionSyntaxSignature(FunctionExpressionSyntax functionExpression)
    {
        static string FormatParameter(ParameterSyntax parameter)
        {
            var parameterName = parameter.Identifier.ValueText;
            if (parameter.TypeAnnotation?.Type is not { } typeSyntax)
                return parameterName;

            return $"{parameterName}: {typeSyntax}";
        }

        var isStatic = functionExpression switch
        {
            SimpleFunctionExpressionSyntax simple => simple.StaticKeyword.Kind == SyntaxKind.StaticKeyword,
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.StaticKeyword.Kind == SyntaxKind.StaticKeyword,
            _ => false
        };

        var name = functionExpression switch
        {
            ParenthesizedFunctionExpressionSyntax parenthesized when parenthesized.Identifier.Kind == SyntaxKind.IdentifierToken
                => parenthesized.Identifier.ValueText,
            _ => null
        };

        var parameters = functionExpression switch
        {
            SimpleFunctionExpressionSyntax simple when simple.Parameter is not null
                => [FormatParameter(simple.Parameter)],
            ParenthesizedFunctionExpressionSyntax parenthesized when parenthesized.ParameterList is not null
                => parenthesized.ParameterList.Parameters.Select(FormatParameter),
            _ => Enumerable.Empty<string>()
        };

        var returnTypeSyntax = functionExpression switch
        {
            SimpleFunctionExpressionSyntax simple => simple.ReturnType?.Type,
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.ReturnType?.Type,
            _ => null
        };

        var returnTypeDisplay = returnTypeSyntax is null
            ? "?"
            : returnTypeSyntax.ToString();

        var staticPrefix = isStatic ? "static " : string.Empty;
        var namePrefix = string.IsNullOrWhiteSpace(name) ? string.Empty : $"func {name}";
        var parameterList = string.Join(", ", parameters);
        return string.IsNullOrEmpty(namePrefix)
            ? $"{staticPrefix}({parameterList}) -> {returnTypeDisplay}"
            : $"{staticPrefix}{namePrefix}({parameterList}) -> {returnTypeDisplay}";
    }

    private static TextSpan GetHoverSpanForResolution(SymbolResolutionResult resolution)
    {
        var node = resolution.Node;

        if (node is not IdentifierNameSyntax and
            not SingleVariableDesignationSyntax and
            not VariableDeclaratorSyntax and
            not ParameterSyntax &&
            TryGetDeclaredIdentifierSpan(resolution.Symbol, out var declaredSpan))
        {
            return declaredSpan;
        }

        return node switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.Span,
            SingleVariableDesignationSyntax designation => designation.Identifier.Span,
            VariableDeclaratorSyntax declarator => declarator.Identifier.Span,
            ParameterSyntax parameter when !parameter.Identifier.IsMissing => parameter.Identifier.Span,
            MethodDeclarationSyntax method when !method.Identifier.IsMissing => method.Identifier.Span,
            FunctionStatementSyntax function when !function.Identifier.IsMissing => function.Identifier.Span,
            InvocationExpressionSyntax { Expression: IdentifierNameSyntax identifier } => identifier.Identifier.Span,
            InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax memberAccess } when !memberAccess.Name.Identifier.IsMissing
                => memberAccess.Name.Identifier.Span,
            MemberAccessExpressionSyntax memberAccess when !memberAccess.Name.Identifier.IsMissing => memberAccess.Name.Identifier.Span,
            _ => node.Span
        };
    }

    private static bool TryGetDeclaredIdentifierSpan(ISymbol symbol, out TextSpan span)
    {
        foreach (var syntax in symbol.DeclaringSyntaxReferences.Select(static reference => reference.GetSyntax()))
        {
            switch (syntax)
            {
                case VariableDeclaratorSyntax declarator when !declarator.Identifier.IsMissing:
                    span = declarator.Identifier.Span;
                    return true;
                case SingleVariableDesignationSyntax designation when !designation.Identifier.IsMissing:
                    span = designation.Identifier.Span;
                    return true;
                case ParameterSyntax parameter when !parameter.Identifier.IsMissing:
                    span = parameter.Identifier.Span;
                    return true;
                case MethodDeclarationSyntax method when !method.Identifier.IsMissing:
                    span = method.Identifier.Span;
                    return true;
                case FunctionStatementSyntax function when !function.Identifier.IsMissing:
                    span = function.Identifier.Span;
                    return true;
            }
        }

        span = default;
        return false;
    }

    private static ITypeSymbol? InferPatternElementType(
        SyntaxNode pattern,
        SyntaxToken token,
        ExpressionSyntax right,
        SemanticModel semanticModel)
    {
        var rightType = semanticModel.GetTypeInfo(right).Type;
        if (rightType is null)
            return null;

        if (pattern is PositionalPatternSyntax positional)
        {
            var index = -1;
            for (var i = 0; i < positional.Elements.Count; i++)
            {
                if (!positional.Elements[i].Span.Contains(token.Span))
                    continue;

                index = i;
                break;
            }

            if (index >= 0 && rightType is ITupleTypeSymbol tupleType && index < tupleType.TupleElements.Length)
                return tupleType.TupleElements[index].Type;

            return null;
        }

        if (pattern is SequencePatternSyntax sequence)
        {
            var element = sequence.Elements.FirstOrDefault(e => e.Span.Contains(token.Span));
            if (element is null)
                return null;

            if (!TryGetSequencePatternElementType(rightType, semanticModel, out var sequenceElementType))
                return null;

            var elementIndex = FindSequencePatternElementIndex(sequence, element);
            return GetSequencePatternElementValueType(sequence, elementIndex, rightType, sequenceElementType, semanticModel);
        }

        return null;
    }

    private static IEnumerable<int> NormalizeOffsets(int offset, int maxOffset)
    {
        if (maxOffset < 0)
            yield break;

        var clamped = Math.Clamp(offset, 0, maxOffset);
        yield return clamped;

        if (clamped > 0)
            yield return clamped - 1;
    }

    private static string BuildSignature(ISymbol symbol, SyntaxNode contextNode, SemanticModel semanticModel)
    {
        var plainTypeFormat = CreatePlainTypeFormat();

        if (symbol is IMethodSymbol { MethodKind: MethodKind.LambdaMethod } lambda)
        {
            var parameters = FormatParameters(lambda.Parameters, plainTypeFormat);
            var returnType = lambda.ReturnType.ToDisplayString(plainTypeFormat);
            return $"({parameters}) -> {returnType}";
        }

        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor } constructor)
        {
            var containingType = constructor.ContainingType;
            var parameters = FormatParameters(constructor.Parameters, plainTypeFormat);
            var accessibilityPrefix = GetNonPublicAccessibilityPrefix(constructor);

            if (containingType?.IsUnion == true)
            {
                var declarationTypeFormat = CreatePlainTypeFormat()
                    .WithKindOptions(SymbolDisplayKindOptions.IncludeTypeKeyword)
                    .WithMiscellaneousOptions(
                        CreatePlainTypeFormat().MiscellaneousOptions |
                        SymbolDisplayMiscellaneousOptions.IncludeUnionMemberTypes);
                var unionDisplay = containingType.ToDisplayString(declarationTypeFormat);
                return $"{accessibilityPrefix}{unionDisplay}({parameters})";
            }

            var constructorName = containingType?.Name ?? constructor.Name;
            var typeParams = containingType is not null && !containingType.TypeParameters.IsDefaultOrEmpty
                ? $"<{string.Join(", ", containingType.TypeParameters.Select(static tp => tp.Name))}>"
                : string.Empty;
            return $"{accessibilityPrefix}{constructorName}{typeParams}({parameters})";
        }

        if (symbol is IMethodSymbol method)
        {
            method = GetPreferredMethodForDisplay(method);

            if (MethodSignatureContainsError(method) &&
                TryBuildMethodSyntaxSignature(method, out var syntaxSignature))
            {
                return syntaxSignature;
            }

            var parameters = FormatParameters(
                GetDisplayParametersForMethod(method, contextNode, semanticModel),
                plainTypeFormat);
            var returnType = method.ReturnType.ToDisplayString(plainTypeFormat);
            // Use concrete type arguments when available (inferred at a call site),
            // otherwise fall back to type parameter names for the generic definition.
            var typeParameters = method.TypeParameters.IsDefaultOrEmpty
                ? string.Empty
                : !method.TypeArguments.IsDefaultOrEmpty &&
                  method.TypeArguments.Length == method.TypeParameters.Length &&
                  method.TypeArguments.Any(static a => a is not ITypeParameterSymbol)
                    ? $"<{string.Join(", ", method.TypeArguments.Select(a => a.ToDisplayString(plainTypeFormat)))}>"
                    : $"<{string.Join(", ", method.TypeParameters.Select(static tp => tp.Name))}>";
            var isExtensionAsInstance = IsExtensionMethodAccessedAsInstance(method, contextNode, semanticModel);
            var staticPrefix = !isExtensionAsInstance &&
                               (IsLocalFunctionDeclaredStatic(method) || (!IsFunctionStatementSymbol(method) && method.IsStatic))
                ? "static "
                : string.Empty;
            var accessibilityPrefix = IsFunctionStatementSymbol(method)
                ? string.Empty
                : GetNonPublicAccessibilityPrefix(method);
            return $"{accessibilityPrefix}{staticPrefix}func {method.Name}{typeParameters}({parameters}) -> {returnType}";
        }

        if (symbol is IEventSymbol ev)
        {
            var eventType = ev.Type.ToDisplayString(plainTypeFormat);
            var accessibilityPrefix = GetNonPublicAccessibilityPrefix(ev);
            return $"{accessibilityPrefix}event {ev.Name}: {eventType}";
        }

        if (symbol is IParameterSymbol parameter)
        {
            var parameterTypeSymbol = parameter.Type;
            if (parameterTypeSymbol.ContainsErrorType() &&
                contextNode.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault() is { } parameterSyntax &&
                semanticModel.GetFunctionExpressionParameterSymbol(parameterSyntax) is { Type: { } contextualParameterType } &&
                !contextualParameterType.ContainsErrorType())
            {
                parameterTypeSymbol = contextualParameterType;
            }
            else if (parameterTypeSymbol.ContainsErrorType() &&
                     TryInferLambdaParameterTypeFromFunctionTarget(contextNode, semanticModel, out var targetedParameterType))
            {
                parameterTypeSymbol = targetedParameterType;
            }
            else if (parameterTypeSymbol.ContainsErrorType() &&
                TryInferDeclaredTypeFromContext(contextNode, semanticModel, out var declaredParameterType))
            {
                parameterTypeSymbol = declaredParameterType;
            }
            else if (parameterTypeSymbol.ContainsErrorType() &&
                TryInferLambdaParameterTypeFromContext(parameter, contextNode, semanticModel, out var inferredParameterType))
            {
                parameterTypeSymbol = inferredParameterType;
            }
            else if (parameterTypeSymbol.ContainsErrorType() &&
                     TryInferReceiverTypeFromMemberAccessContext(parameter.Name, contextNode, semanticModel, out inferredParameterType))
            {
                parameterTypeSymbol = inferredParameterType;
            }

            var parameterType = parameterTypeSymbol.ToDisplayString(plainTypeFormat);
            var accessibilityPrefix = GetNonPublicParameterAccessibilityPrefix(parameter);
            var promotedBindingPrefix = GetPromotedPrimaryConstructorBindingPrefix(parameter);
            return $"{accessibilityPrefix}{promotedBindingPrefix}{parameter.Name}: {parameterType}";
        }

        if (symbol is ILocalSymbol local)
        {
            var binding = local.IsMutable ? "var" : "val";
            var localTypeSymbol = local.Type;
            if (localTypeSymbol.ContainsErrorType() &&
                TryInferPatternDeclaredLocalType(
                    contextNode.AncestorsAndSelf().OfType<SingleVariableDesignationSyntax>().FirstOrDefault()
                    ?? local.DeclaringSyntaxReferences.Select(static reference => reference.GetSyntax()).OfType<SingleVariableDesignationSyntax>().FirstOrDefault(),
                    semanticModel,
                    out var patternLocalType))
            {
                localTypeSymbol = patternLocalType;
            }

            if (localTypeSymbol.ContainsErrorType() &&
                TryInferDeclaredTypeFromContext(contextNode, semanticModel, out var declaredLocalType))
            {
                localTypeSymbol = declaredLocalType;
            }

            if (localTypeSymbol.ContainsErrorType() &&
                TryInferReceiverTypeFromMemberAccessContext(local.Name, contextNode, semanticModel, out var inferredLocalType))
            {
                localTypeSymbol = inferredLocalType;
            }

            var localType = localTypeSymbol.ToDisplayString(plainTypeFormat);
            return $"{binding} {local.Name}: {localType}";
        }

        if (symbol is IUnionCaseTypeSymbol unionCase)
        {
            var parameters = FormatParameters(unionCase.ConstructorParameters, plainTypeFormat);
            return $"{unionCase.Name}({parameters})";
        }

        if (symbol is ITypeSymbol typeSymbol)
        {
            var declarationTypeFormat = CreatePlainTypeFormat()
                .WithMiscellaneousOptions(
                    CreatePlainTypeFormat().MiscellaneousOptions |
                    SymbolDisplayMiscellaneousOptions.IncludeUnionMemberTypes);

            if (contextNode is FunctionTypeSyntax functionTypeSyntax &&
                TryFormatFunctionTypeSyntaxSignature(functionTypeSyntax, semanticModel, declarationTypeFormat, out var functionTypeSignature))
            {
                return functionTypeSignature;
            }

            if (typeSymbol is INamedTypeSymbol delegateType &&
                delegateType.TypeKind == TypeKind.Delegate)
            {
                if (TryFormatDelegateTypeSignature(delegateType, declarationTypeFormat, out var delegateSignature))
                    return delegateSignature;

                return delegateType.ToDisplayString(declarationTypeFormat);
            }

            var typeFormat = declarationTypeFormat.WithKindOptions(SymbolDisplayKindOptions.IncludeTypeKeyword);
            var text = typeSymbol.ToDisplayString(typeFormat);

            // Append base class / base interface list (e.g. "class Foo: Bar")
            if (typeSymbol is INamedTypeSymbol namedType)
            {
                var bases = new System.Collections.Generic.List<string>();

                // Only show user-defined base types (SpecialType.None excludes object, ValueType, etc.)
                if (namedType.BaseType is { SpecialType: SpecialType.None } baseType)
                    bases.Add(baseType.ToDisplayString(declarationTypeFormat));

                foreach (var iface in namedType.Interfaces)
                    bases.Add(iface.ToDisplayString(declarationTypeFormat));

                if (bases.Count > 0)
                    text += ": " + string.Join(", ", bases);
            }

            return text;
        }

        return symbol.ToDisplayString(SymbolDisplayFormat.RavenTooltipFormat);
    }

    private static IMethodSymbol GetPreferredMethodForDisplay(IMethodSymbol method)
    {
        if (!MethodSignatureContainsError(method))
            return method;

        if (method.OriginalDefinition is IMethodSymbol originalDefinition &&
            !ReferenceEquals(originalDefinition, method) &&
            !MethodSignatureContainsError(originalDefinition))
        {
            return originalDefinition;
        }

        return method;
    }

    private static bool MethodSignatureContainsError(IMethodSymbol method)
        => method.ReturnType.ContainsErrorType() ||
           method.Parameters.Any(static parameter => parameter.Type.ContainsErrorType());

    private static bool TryBuildMethodSyntaxSignature(IMethodSymbol method, out string signature)
    {
        signature = string.Empty;

        foreach (var syntax in method.DeclaringSyntaxReferences.Select(static reference => reference.GetSyntax()))
        {
            switch (syntax)
            {
                case MethodDeclarationSyntax declaration:
                    signature = BuildMethodSyntaxSignature(
                        method,
                        declaration.Identifier.ValueText,
                        declaration.TypeParameterList?.ToString() ?? string.Empty,
                        declaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword),
                        GetNonPublicAccessibilityPrefix(method),
                        declaration.ParameterList?.Parameters ?? [],
                        GetMethodReturnTypeDisplay(declaration.ReturnType));
                    return true;
                case FunctionStatementSyntax function:
                    signature = BuildMethodSyntaxSignature(
                        method,
                        function.Identifier.ValueText,
                        function.TypeParameterList?.ToString() ?? string.Empty,
                        function.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword),
                        string.Empty,
                        function.ParameterList?.Parameters ?? [],
                        GetMethodReturnTypeDisplay(function.ReturnType));
                    return true;
            }
        }

        return false;
    }

    private static string BuildMethodSyntaxSignature(
        IMethodSymbol method,
        string declaredName,
        string typeParameterDisplay,
        bool isStatic,
        string accessibilityPrefix,
        IEnumerable<ParameterSyntax> parameters,
        string returnType)
    {
        var parameterDisplay = string.Join(", ", parameters.Select(FormatParameterSyntaxForMethodSignature));
        var staticPrefix = isStatic ? "static " : string.Empty;
        var methodName = string.IsNullOrWhiteSpace(declaredName) ? method.Name : declaredName;
        return $"{accessibilityPrefix}{staticPrefix}func {methodName}{typeParameterDisplay}({parameterDisplay}) -> {returnType}";
    }

    private static string FormatParameterSyntaxForMethodSignature(ParameterSyntax parameter)
    {
        var refPrefix = parameter.RefKindKeyword.Kind switch
        {
            SyntaxKind.RefKeyword => "ref ",
            SyntaxKind.OutKeyword => "out ",
            SyntaxKind.InKeyword => "in ",
            _ => string.Empty
        };
        var typeDisplay = parameter.TypeAnnotation?.Type.ToString() ?? "?";
        return $"{refPrefix}{parameter.Identifier.ValueText}: {typeDisplay}";
    }

    private static string GetMethodReturnTypeDisplay(ArrowTypeClauseSyntax? returnType)
        => returnType?.Type.ToString() ?? "unit";

    private static bool TryInferPatternDeclaredLocalType(
        SingleVariableDesignationSyntax? designation,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        if (designation is null)
            return false;

        if (designation.GetAncestor<DeclarationPatternSyntax>() is { } declarationPattern &&
            TryResolveTypeSymbolFromSyntax(semanticModel, declarationPattern.Type, out var declaredType))
        {
            inferredType = declaredType;
            return true;
        }

        if (designation.GetAncestor<SequencePatternElementSyntax>() is { } sequenceElement &&
            designation.GetAncestor<SequencePatternSyntax>() is { } sequencePattern &&
            TryGetPatternInputType(semanticModel, sequencePattern, out var sequenceInputType) &&
            TryGetSequencePatternElementType(sequenceInputType, semanticModel, out var sequenceElementType))
        {
            var elementIndex = FindSequencePatternElementIndex(sequencePattern, sequenceElement);
            inferredType = GetSequencePatternElementValueType(
                sequencePattern,
                elementIndex,
                sequenceInputType,
                sequenceElementType,
                semanticModel);
            return true;
        }

        if (designation.GetAncestor<PositionalPatternSyntax>() is { } positionalPattern &&
            TryGetPatternInputType(semanticModel, positionalPattern, out var positionalInputType))
        {
            var tupleElementTypes = GetTupleElementTypes(positionalInputType);
            for (var i = 0; i < positionalPattern.Elements.Count && i < tupleElementTypes.Length; i++)
            {
                if (!positionalPattern.Elements[i].Span.Contains(designation.Span))
                    continue;

                inferredType = tupleElementTypes[i];
                return true;
            }
        }

        return false;
    }

    private static string BuildSignatureForResolvedHover(
        SymbolResolutionResult resolution,
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset)
    {
        var symbol = resolution.Symbol;
        var contextNode = resolution.Node;

        if (contextNode.AncestorsAndSelf().OfType<SingleVariableDesignationSyntax>().FirstOrDefault() is { } single &&
            semanticModel.GetDeclaredSymbol(single) is { } declaredSymbol)
        {
            symbol = declaredSymbol;
        }

        if (resolution.Kind is SymbolResolutionKind.Declaration or SymbolResolutionKind.PatternLocal or SymbolResolutionKind.BlockLocal &&
            symbol is ILocalSymbol declarationLocal &&
            TryBuildPatternDeclarationSignatureOverride(declarationLocal, root, offset, semanticModel, out var patternDeclarationSignature))
        {
            return patternDeclarationSignature;
        }

        if (resolution.Kind is SymbolResolutionKind.Declaration or SymbolResolutionKind.PatternLocal or SymbolResolutionKind.BlockLocal &&
            symbol is ILocalSymbol local &&
            local.Type.ContainsErrorType() &&
            TryInferPatternDeclaredLocalTypeAtOffset(root, offset, semanticModel, out var localTypeAtOffset))
        {
            var plainTypeFormat = CreatePlainTypeFormat();
            var binding = local.IsMutable ? "var" : "val";
            return $"{binding} {local.Name}: {localTypeAtOffset.ToDisplayString(plainTypeFormat)}";
        }

        if (resolution.Kind is SymbolResolutionKind.ParameterDeclaration or SymbolResolutionKind.Declaration &&
            TryBuildDeclaredTypeHoverSignatureOverride(symbol, semanticModel, root, offset, out var declaredTypeSignature))
            return declaredTypeSignature;

        var signature = BuildSignature(symbol, contextNode, semanticModel);

        if (!TryBuildReceiverErrorSignatureOverride(symbol, semanticModel, root, offset, out var overridden))
            return signature;

        return overridden;
    }

    private static string BuildDisplaySignatureForResolvedHover(
        SymbolResolutionResult resolution,
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset)
    {
        var signature = BuildSignatureForResolvedHover(resolution, semanticModel, root, offset);

        return IsExtensionHoverSymbol(resolution.Symbol)
            ? $"(extension) {signature}"
            : signature;
    }

    private static string BuildDisplaySignatureForHover(
        ISymbol symbol,
        SyntaxNode contextNode,
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset)
    {
        var kind = contextNode switch
        {
            TypeSyntax => SymbolResolutionKind.TypePosition,
            ParameterSyntax => SymbolResolutionKind.ParameterDeclaration,
            _ => SymbolResolutionKind.SymbolInfo
        };

        return BuildDisplaySignatureForResolvedHover(
            new SymbolResolutionResult(kind, symbol, contextNode),
            semanticModel,
            root,
            offset);
    }

    private static string BuildSignatureForHover(
        ISymbol symbol,
        SyntaxNode contextNode,
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset)
    {
        var kind = contextNode switch
        {
            TypeSyntax => SymbolResolutionKind.TypePosition,
            ParameterSyntax => SymbolResolutionKind.ParameterDeclaration,
            _ => SymbolResolutionKind.SymbolInfo
        };

        return BuildSignatureForResolvedHover(
            new SymbolResolutionResult(kind, symbol, contextNode),
            semanticModel,
            root,
            offset);
    }

    private static bool TryBuildDeclaredTypeHoverSignatureOverride(
        ISymbol symbol,
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset,
        out string signature)
    {
        signature = string.Empty;

        if (symbol is not ILocalSymbol and not IParameterSymbol)
            return false;

        var typeSymbol = symbol switch
        {
            ILocalSymbol local => local.Type,
            IParameterSymbol parameter => parameter.Type,
            _ => null
        };

        if (typeSymbol is null || !typeSymbol.ContainsErrorType())
            return false;

        if (!TryInferDeclaredTypeAtOffset(root, offset, semanticModel, out var declaredType))
            return false;

        var plainTypeFormat = CreatePlainTypeFormat();
        var bindingPrefix = symbol switch
        {
            ILocalSymbol local => $"{(local.IsMutable ? "var" : "val")} {local.Name}: ",
            IParameterSymbol parameter => $"{GetNonPublicParameterAccessibilityPrefix(parameter)}{GetPromotedPrimaryConstructorBindingPrefix(parameter)}{parameter.Name}: ",
            _ => string.Empty
        };

        signature = bindingPrefix + declaredType.ToDisplayString(plainTypeFormat);
        return true;
    }

    private static bool TryGetPatternInputType(
        SemanticModel semanticModel,
        SyntaxNode patternNode,
        out ITypeSymbol inputType)
    {
        inputType = null!;

        if (patternNode.GetAncestor<PatternDeclarationAssignmentStatementSyntax>() is { } patternAssignment)
        {
            var type = GetExpressionType(semanticModel, patternAssignment.Right);
            if (type is not null && type.TypeKind != TypeKind.Error)
            {
                inputType = type;
                return true;
            }
        }

        if (patternNode.GetAncestor<IsPatternExpressionSyntax>() is { } isPattern)
        {
            var type = GetExpressionType(semanticModel, isPattern.Expression);
            if (type is not null && type.TypeKind != TypeKind.Error)
            {
                inputType = type;
                return true;
            }
        }

        if (patternNode.GetAncestor<MatchExpressionSyntax>() is { } matchExpression)
        {
            var type = GetExpressionType(semanticModel, matchExpression.Expression);
            if (type is not null && type.TypeKind != TypeKind.Error)
            {
                inputType = type;
                return true;
            }
        }

        if (patternNode.GetAncestor<MatchStatementSyntax>() is { } matchStatement)
        {
            var type = GetExpressionType(semanticModel, matchStatement.Expression);
            if (type is not null && type.TypeKind != TypeKind.Error)
            {
                inputType = type;
                return true;
            }
        }

        if (patternNode.GetAncestor<ForStatementSyntax>() is { } forStatement &&
            forStatement.Target is PatternSyntax)
        {
            var collectionType = GetExpressionType(semanticModel, forStatement.Expression);
            if (TryGetForIterationElementType(collectionType, semanticModel, out var elementType))
            {
                inputType = elementType;
                return true;
            }
        }

        if (patternNode.GetAncestor<ParameterSyntax>() is { Pattern: not null } parameterSyntax &&
            semanticModel.GetDeclaredSymbol(parameterSyntax) is IParameterSymbol parameterSymbol &&
            parameterSymbol.Type is { TypeKind: not TypeKind.Error } parameterType)
        {
            inputType = parameterType;
            return true;
        }

        return false;
    }

    private static ITypeSymbol? GetExpressionType(SemanticModel semanticModel, ExpressionSyntax expression)
    {
        var type = semanticModel.GetTypeInfo(expression).Type;
        if (type is not null && type.TypeKind != TypeKind.Error)
            return type;

        var operationType = semanticModel.GetOperation(expression)?.Type;
        if (operationType is not null && operationType.TypeKind != TypeKind.Error)
            return operationType;

        var symbol = SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, expression, out var symbolInfo)
            ? symbolInfo.Symbol
            : null;
        return symbol switch
        {
            ILocalSymbol local => local.Type.TypeKind != TypeKind.Error && local.Type is not IArrayTypeSymbol { ElementType.TypeKind: TypeKind.Error }
                ? local.Type
                : TryGetReferencedIdentifierDeclaredType(semanticModel, expression) ?? local.Type,
            IParameterSymbol parameter => parameter.Type.TypeKind != TypeKind.Error && parameter.Type is not IArrayTypeSymbol { ElementType.TypeKind: TypeKind.Error }
                ? parameter.Type
                : TryGetReferencedIdentifierDeclaredType(semanticModel, expression) ?? parameter.Type,
            IPropertySymbol property => property.Type,
            IFieldSymbol field => field.Type,
            IEventSymbol ev => ev.Type,
            _ => TryGetReferencedIdentifierDeclaredType(semanticModel, expression) ?? type
        };
    }

    private static ITypeSymbol? TryGetReferencedIdentifierDeclaredType(SemanticModel semanticModel, ExpressionSyntax expression)
    {
        if (expression is not IdentifierNameSyntax identifier)
            return null;

        var root = expression.SyntaxTree!.GetRoot();
        var name = identifier.Identifier.ValueText;

        var parameterType = root.DescendantNodes()
            .OfType<ParameterSyntax>()
            .Where(parameter => parameter.Identifier.ValueText == name &&
                                parameter.Span.Start <= expression.Span.Start &&
                                parameter.TypeAnnotation is not null)
            .Select(parameter => parameter.TypeAnnotation!.Type)
            .Select(typeSyntax => TryResolveTypeSymbolFromSyntax(semanticModel, typeSyntax, out var resolvedType) ? resolvedType : null)
            .LastOrDefault(type => type is not null);
        if (parameterType is not null)
            return parameterType;

        var localType = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Where(declarator => declarator.Identifier.ValueText == name &&
                                 declarator.Span.Start <= expression.Span.Start &&
                                 declarator.TypeAnnotation is not null)
            .Select(declarator => declarator.TypeAnnotation!.Type)
            .Select(typeSyntax => TryResolveTypeSymbolFromSyntax(semanticModel, typeSyntax, out var resolvedType) ? resolvedType : null)
            .LastOrDefault(type => type is not null);

        return localType;
    }

    private static bool TryBuildPatternDeclarationSignatureOverride(
        ILocalSymbol local,
        SyntaxNode root,
        int offset,
        SemanticModel semanticModel,
        out string signature)
    {
        signature = string.Empty;
        var plainTypeFormat = CreatePlainTypeFormat();

        foreach (var candidateOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidateOffset);
            }
            catch
            {
                continue;
            }

            var designation = token.Parent?.AncestorsAndSelf().OfType<SingleVariableDesignationSyntax>().FirstOrDefault();
            if (designation is null || !string.Equals(designation.Identifier.ValueText, local.Name, StringComparison.Ordinal))
                continue;

            var binding = local.IsMutable || designation.BindingKeyword.Kind == SyntaxKind.VarKeyword ? "var" : "val";

            if (local.Type.TypeKind != TypeKind.Error)
            {
                signature = $"{binding} {local.Name}: {local.Type.ToDisplayString(plainTypeFormat)}";
                return true;
            }

            if (designation.GetAncestor<DeclarationPatternSyntax>() is { } declarationPattern)
            {
                var typeDisplay = TryResolveTypeSymbolFromSyntax(semanticModel, declarationPattern.Type, out var declaredType)
                    ? declaredType.ToDisplayString(plainTypeFormat)
                    : declarationPattern.Type.ToString();
                signature = $"{binding} {local.Name}: {typeDisplay}";
                return true;
            }

            if (designation.GetAncestor<SequencePatternElementSyntax>() is { } sequenceElement &&
                designation.GetAncestor<SequencePatternSyntax>() is { } sequencePattern &&
                TryGetPatternInputType(semanticModel, sequencePattern, out var sequenceInputType) &&
                TryGetSequencePatternElementType(sequenceInputType, semanticModel, out var sequenceElementType))
            {
                var elementIndex = FindSequencePatternElementIndex(sequencePattern, sequenceElement);
                var elementType = GetSequencePatternElementValueType(
                    sequencePattern,
                    elementIndex,
                    sequenceInputType,
                    sequenceElementType,
                    semanticModel);
                signature = $"{binding} {local.Name}: {elementType.ToDisplayString(plainTypeFormat)}";
                return true;
            }

            if (designation.GetAncestor<PositionalPatternSyntax>() is { } positionalPattern &&
                TryGetPatternInputType(semanticModel, positionalPattern, out var positionalInputType))
            {
                var tupleElementTypes = GetTupleElementTypes(positionalInputType);
                for (var i = 0; i < positionalPattern.Elements.Count && i < tupleElementTypes.Length; i++)
                {
                    if (!positionalPattern.Elements[i].Span.Contains(designation.Span))
                        continue;

                    signature = $"{binding} {local.Name}: {tupleElementTypes[i].ToDisplayString(plainTypeFormat)}";
                    return true;
                }
            }
        }

        return false;
    }

    private static bool TryInferPatternDeclaredLocalTypeAtOffset(
        SyntaxNode root,
        int offset,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        foreach (var candidateOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidateOffset);
            }
            catch
            {
                continue;
            }

            if (token.Parent?.AncestorsAndSelf().OfType<SingleVariableDesignationSyntax>().FirstOrDefault() is not { } designation)
                continue;

            if (TryInferPatternDeclaredLocalType(designation, semanticModel, out inferredType))
                return true;
        }

        return false;
    }

    private static bool TryGetForIterationElementType(
        ITypeSymbol? collectionType,
        SemanticModel semanticModel,
        out ITypeSymbol elementType)
    {
        elementType = null!;
        if (collectionType is null || collectionType.TypeKind == TypeKind.Error)
            return false;

        if (collectionType is IArrayTypeSymbol arrayType)
        {
            elementType = arrayType.ElementType;
            return true;
        }

        if (collectionType.SpecialType == SpecialType.System_String)
        {
            elementType = semanticModel.Compilation.GetSpecialType(SpecialType.System_Char);
            return true;
        }

        if (collectionType is INamedTypeSymbol namedType)
        {
            foreach (var candidate in EnumerateSelfAndInterfaces(namedType))
            {
                if (candidate.TypeArguments.Length == 1 &&
                    candidate.Name is "IEnumerable" or "IAsyncEnumerable")
                {
                    elementType = candidate.TypeArguments[0];
                    return true;
                }
            }
        }

        return false;

        static IEnumerable<INamedTypeSymbol> EnumerateSelfAndInterfaces(INamedTypeSymbol type)
        {
            yield return type;
            foreach (var iface in type.AllInterfaces)
                yield return iface;
        }
    }

    private static bool TryGetSequencePatternElementType(
        ITypeSymbol inputType,
        SemanticModel semanticModel,
        out ITypeSymbol elementType)
    {
        elementType = null!;

        if (inputType.TypeKind == TypeKind.Error)
            return false;

        if (inputType is IArrayTypeSymbol arrayType)
        {
            elementType = arrayType.ElementType;
            return true;
        }

        if (inputType.SpecialType == SpecialType.System_String)
        {
            elementType = semanticModel.Compilation.GetSpecialType(SpecialType.System_Char);
            return true;
        }

        if (inputType is INamedTypeSymbol namedType)
        {
            foreach (var candidate in EnumerateSelfAndInterfaces(namedType))
            {
                if (TryGetIndexableElementType(candidate, out var indexerElementType))
                {
                    elementType = indexerElementType;
                    return true;
                }
            }
        }

        return false;

        static IEnumerable<INamedTypeSymbol> EnumerateSelfAndInterfaces(INamedTypeSymbol type)
        {
            yield return type;
            foreach (var iface in type.AllInterfaces)
                yield return iface;
        }

        static bool TryGetIndexableElementType(INamedTypeSymbol type, out ITypeSymbol indexerElementType)
        {
            indexerElementType = null!;

            var hasCount = type
                .GetMembers("Count")
                .OfType<IPropertySymbol>()
                .Any(static property =>
                    property.Parameters.Length == 0 &&
                    property.Type.SpecialType == SpecialType.System_Int32 &&
                    property.GetMethod is not null);

            if (!hasCount)
                return false;

            var indexer = type
                .GetMembers("Item")
                .OfType<IPropertySymbol>()
                .FirstOrDefault(static property =>
                    property.Parameters.Length == 1 &&
                    property.Parameters[0].Type.SpecialType == SpecialType.System_Int32 &&
                    property.GetMethod is not null);

            if (indexer is null)
                return false;

            indexerElementType = indexer.Type;
            return true;
        }
    }

    private static ITypeSymbol GetSequenceSliceType(
        ITypeSymbol valueType,
        ITypeSymbol elementType,
        SemanticModel semanticModel,
        int? fixedLength = null)
    {
        if (valueType.SpecialType == SpecialType.System_String)
            return semanticModel.Compilation.GetSpecialType(SpecialType.System_String);

        return semanticModel.Compilation.CreateArrayTypeSymbol(elementType, fixedLength: fixedLength);
    }

    private static ITypeSymbol GetSequencePatternElementValueType(
        SequencePatternSyntax pattern,
        int elementIndex,
        ITypeSymbol inputType,
        ITypeSymbol elementType,
        SemanticModel semanticModel)
    {
        if (elementIndex < 0 || elementIndex >= pattern.Elements.Count)
            return elementType;

        var (elementWidths, elementKinds, restIndex) = GetSequencePatternLayout(pattern);
        if (elementKinds[elementIndex] == BoundPositionalPattern.SequenceElementKind.Single)
            return elementType;

        if (inputType is IArrayTypeSymbol arrayType && arrayType.FixedLength is int sourceFixedLength)
        {
            if (elementKinds[elementIndex] == BoundPositionalPattern.SequenceElementKind.FixedSegment)
                return GetSequenceSliceType(inputType, elementType, semanticModel, elementWidths[elementIndex]);

            if (elementKinds[elementIndex] == BoundPositionalPattern.SequenceElementKind.RestSegment && restIndex >= 0)
            {
                var fixedWidth = elementWidths.Where(static width => width > 0).Sum();
                var restWidth = sourceFixedLength - fixedWidth;
                if (restWidth >= 0)
                    return GetSequenceSliceType(inputType, elementType, semanticModel, restWidth);
            }
        }

        return GetSequenceSliceType(inputType, elementType, semanticModel);
    }

    private static (ImmutableArray<int> Widths, ImmutableArray<BoundPositionalPattern.SequenceElementKind> Kinds, int RestIndex)
        GetSequencePatternLayout(SequencePatternSyntax pattern)
    {
        var widths = ImmutableArray.CreateBuilder<int>(pattern.Elements.Count);
        var kinds = ImmutableArray.CreateBuilder<BoundPositionalPattern.SequenceElementKind>(pattern.Elements.Count);
        var restIndex = -1;

        for (var i = 0; i < pattern.Elements.Count; i++)
        {
            var element = pattern.Elements[i];
            var prefix = element.Prefix;
            var dotDotKind = prefix.DotDotToken.Kind;
            if (dotDotKind is not SyntaxKind.DotDotToken and not SyntaxKind.DotDotDotToken)
            {
                widths.Add(1);
                kinds.Add(BoundPositionalPattern.SequenceElementKind.Single);
                continue;
            }

            if (prefix.SegmentLengthToken.Kind == SyntaxKind.NumericLiteralToken)
            {
                var width = int.TryParse(prefix.SegmentLengthToken.Text, out var parsedWidth)
                    ? parsedWidth
                    : 0;
                widths.Add(width);
                kinds.Add(BoundPositionalPattern.SequenceElementKind.FixedSegment);
                continue;
            }

            widths.Add(-1);
            kinds.Add(BoundPositionalPattern.SequenceElementKind.RestSegment);
            if (restIndex < 0)
                restIndex = i;
        }

        return (widths.ToImmutable(), kinds.ToImmutable(), restIndex);
    }

    private static int FindSequencePatternElementIndex(SequencePatternSyntax pattern, SequencePatternElementSyntax target)
    {
        for (var i = 0; i < pattern.Elements.Count; i++)
        {
            if (pattern.Elements[i].Span == target.Span)
                return i;
        }

        return -1;
    }

    private static ImmutableArray<ITypeSymbol> GetTupleElementTypes(ITypeSymbol expectedType)
    {
        if (expectedType is ITupleTypeSymbol tupleType)
            return tupleType.TupleElements.Select(static element => element.Type).ToImmutableArray();

        if (expectedType is INamedTypeSymbol namedType &&
            namedType.IsTupleType &&
            !namedType.TupleElements.IsDefaultOrEmpty)
        {
            return namedType.TupleElements.Select(static element => element.Type).ToImmutableArray();
        }

        return ImmutableArray<ITypeSymbol>.Empty;
    }

    private static bool TryBuildReceiverErrorSignatureOverride(
        ISymbol symbol,
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset,
        out string signature)
    {
        signature = string.Empty;

        var plainTypeFormat = CreatePlainTypeFormat();

        var symbolName = symbol.Name;
        var isErrorParameter = symbol is IParameterSymbol parameter && parameter.Type.ContainsErrorType();
        var localBinding = symbol is ILocalSymbol local && local.Type.ContainsErrorType()
            ? local.IsMutable ? "var" : "val"
            : null;

        if (!isErrorParameter && localBinding is null)
            return false;

        var clampedOffset = Math.Clamp(offset, 0, root.FullSpan.End);
        var memberAccess = FindMemberAccessAtOffset(root, clampedOffset);
        if (memberAccess?.Expression is not IdentifierNameSyntax receiverIdentifier ||
            !string.Equals(receiverIdentifier.Identifier.ValueText, symbolName, StringComparison.Ordinal))
        {
            return false;
        }

        var receiverType = semanticModel.GetTypeInfo(memberAccess.Expression).Type;
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            if (TryInferLambdaParameterTypeByNameFromContext(symbolName, receiverIdentifier, semanticModel, out var inferredLambdaType))
                receiverType = inferredLambdaType;
        }

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            var memberSymbol =
                (SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, memberAccess.Name, out var memberNameInfo)
                    ? memberNameInfo.Symbol
                    : null)
                ??
                (SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, memberAccess, out var memberAccessInfo)
                    ? memberAccessInfo.Symbol
                    : null);
            receiverType = memberSymbol switch
            {
                IPropertySymbol property => property.ContainingType,
                IFieldSymbol field => field.ContainingType,
                IMethodSymbol method => method.ContainingType,
                _ => null
            };
        }

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        signature = localBinding is not null
            ? $"{localBinding} {symbolName}: {receiverType.ToDisplayString(plainTypeFormat)}"
            : $"{symbolName}: {receiverType.ToDisplayString(plainTypeFormat)}";
        return true;
    }

    private static MemberAccessExpressionSyntax? FindMemberAccessAtOffset(SyntaxNode root, int offset)
    {
        var access = root
            .DescendantNodesAndSelf()
            .OfType<MemberAccessExpressionSyntax>()
            .Where(member => member.Span.Contains(offset))
            .OrderBy(member => member.Span.Length)
            .FirstOrDefault();

        if (access is not null)
            return access;

        if (offset <= 0)
            return null;

        return root
            .DescendantNodesAndSelf()
            .OfType<MemberAccessExpressionSyntax>()
            .Where(member => member.Span.Contains(offset - 1))
            .OrderBy(member => member.Span.Length)
            .FirstOrDefault();
    }

    private static string GetNonPublicAccessibilityPrefix(ISymbol symbol)
    {
        var accessibility = symbol.DeclaredAccessibility;
        if (accessibility is Accessibility.NotApplicable or Accessibility.Public)
            return string.Empty;

        return AccessibilityUtilities.GetDisplayText(accessibility) + " ";
    }

    private static string GetNonPublicParameterAccessibilityPrefix(IParameterSymbol parameter)
    {
        foreach (var syntaxReference in parameter.DeclaringSyntaxReferences)
        {
            if (syntaxReference.GetSyntax() is not ParameterSyntax parameterSyntax)
                continue;

            var kind = parameterSyntax.AccessibilityKeyword.Kind;
            if (kind is SyntaxKind.PrivateKeyword or SyntaxKind.InternalKeyword or SyntaxKind.ProtectedKeyword)
                return parameterSyntax.AccessibilityKeyword.Text + " ";
        }

        return string.Empty;
    }

    private static string FormatType(ITypeSymbol type, SymbolDisplayFormat format)
    {
        return type is UnitTypeSymbol
            ? "unit"
            : type.ToDisplayString(format);
    }

    private static bool TryFormatDelegateTypeSignature(
        INamedTypeSymbol delegateType,
        SymbolDisplayFormat plainTypeFormat,
        out string signature)
    {
        var invokeMethod = delegateType.GetDelegateInvokeMethod();
        if (invokeMethod is null)
        {
            signature = string.Empty;
            return false;
        }

        var parameters = string.Join(
            ", ",
            invokeMethod.Parameters.Select(parameter =>
            {
                var modifier = parameter.RefKind switch
                {
                    RefKind.In => "in ",
                    RefKind.Ref => "ref ",
                    RefKind.Out => "out ",
                    RefKind.RefReadOnly => "ref readonly ",
                    _ => string.Empty
                };

                return modifier + parameter.Type.ToDisplayString(plainTypeFormat);
            }));

        var returnType = invokeMethod.ReturnType.ToDisplayString(plainTypeFormat);
        signature = $"({parameters}) -> {returnType}";
        return true;
    }

    private static bool TryFormatFunctionTypeSyntaxSignature(
        FunctionTypeSyntax functionTypeSyntax,
        SemanticModel semanticModel,
        SymbolDisplayFormat plainTypeFormat,
        out string signature)
    {
        _ = semanticModel;
        _ = plainTypeFormat;

        var parameterTypes = functionTypeSyntax.Parameter is { } singleParameter
            ? [singleParameter.ToString()]
            : functionTypeSyntax.ParameterList is { } parameterList
                ? parameterList.Parameters.Select(static parameter => parameter.ToString()).ToList()
                : [];

        signature = $"({string.Join(", ", parameterTypes)}) -> {functionTypeSyntax.ReturnType}";
        return true;
    }

    private static ImmutableArray<IParameterSymbol> GetDisplayParametersForMethod(
        IMethodSymbol method,
        SyntaxNode contextNode,
        SemanticModel semanticModel)
    {
        if (IsExtensionMethodAccessedAsInstance(method, contextNode, semanticModel) &&
            !method.Parameters.IsDefaultOrEmpty)
        {
            return method.Parameters.RemoveAt(0);
        }

        return method.Parameters;
    }

    private static bool IsExtensionMethodAccessedAsInstance(
        IMethodSymbol method,
        SyntaxNode contextNode,
        SemanticModel semanticModel)
    {
        if (!method.IsExtensionMethod)
            return false;

        // We want C#-like behavior when the extension is used through member access:
        //   receiver.ExtMethod(...)
        // and NOT when called statically:
        //   Extensions.ExtMethod(receiver, ...)
        var nameNode = contextNode;

        // Hover resolution may give us the member access node or the identifier node.
        if (nameNode is MemberAccessExpressionSyntax memberAccess)
            nameNode = memberAccess.Name;

        if (nameNode is not IdentifierNameSyntax identifier)
            return false;

        if (identifier.Parent is not MemberAccessExpressionSyntax parentAccess ||
            parentAccess.Name != identifier)
        {
            return false;
        }

        // If the receiver resolves to a type/namespace, this is a static-style access.
        var receiverSymbol = SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, parentAccess.Expression, out var receiverInfo)
            ? receiverInfo.Symbol
            : null;
        if (receiverSymbol is ITypeSymbol or INamespaceSymbol)
            return false;

        return true;
    }

    private static string? BuildContainingDisplay(ISymbol symbol, SemanticModel semanticModel)
    {
        if (TryGetExtensionContainerDisplay(symbol, out var extensionContaining))
            return extensionContaining;

        if (symbol is IParameterSymbol parameterSymbol &&
            IsPromotedPrimaryConstructorParameter(parameterSymbol) &&
            parameterSymbol.ContainingSymbol is IMethodSymbol constructor &&
            constructor.ContainingType is { } containingType)
        {
            return containingType.ToDisplayString(
                SymbolDisplayFormat.RavenSignatureFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly));
        }

        if (symbol is IMethodSymbol method &&
            TryGetEnclosingCallableDisplayForLocalFunction(method, semanticModel, out var localContaining))
        {
            return localContaining;
        }

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

    private static string BuildKindDisplayForResolution(SymbolResolutionKind resolutionKind, ISymbol symbol)
    {
        if (resolutionKind == SymbolResolutionKind.TypePosition)
            return "Type";

        if (symbol is IMethodSymbol method && method.ExtensionMemberKind != ExtensionMemberKind.None)
            return "Extension method";

        if (symbol is IPropertySymbol property && property.ExtensionMemberKind != ExtensionMemberKind.None)
            return "Extension property";

        if (symbol is IParameterSymbol parameterSymbol &&
            IsPromotedPrimaryConstructorParameter(parameterSymbol))
        {
            return "Property";
        }

        if (symbol is IMethodSymbol functionMethod && IsFunctionStatementSymbol(functionMethod))
            return "Function";

        if (symbol is IMethodSymbol { MethodKind: MethodKind.LambdaMethod })
            return "Function expression";

        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor })
            return "Constructor";

        return symbol.Kind.ToString();
    }

    private static string BuildKindDisplay(ISymbol symbol)
        => BuildKindDisplayForResolution(SymbolResolutionKind.SymbolInfo, symbol);

    private static bool TryGetExtensionContainerDisplay(ISymbol symbol, out string? display)
    {
        display = null;

        var containingType = symbol switch
        {
            IMethodSymbol method when method.ExtensionMemberKind != ExtensionMemberKind.None => method.ContainingType,
            IPropertySymbol property when property.ExtensionMemberKind != ExtensionMemberKind.None => property.ContainingType,
            _ => null
        };

        if (containingType is null)
            return false;

        display = containingType.ToDisplayString(
            SymbolDisplayFormat.RavenSignatureFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
                .WithKindOptions(SymbolDisplayKindOptions.None));
        return true;
    }

    private static bool IsExtensionHoverSymbol(ISymbol symbol)
    {
        return symbol switch
        {
            IMethodSymbol method => method.ExtensionMemberKind != ExtensionMemberKind.None,
            IPropertySymbol property => property.ExtensionMemberKind != ExtensionMemberKind.None,
            _ => false
        };
    }

    private static bool IsPromotedPrimaryConstructorParameter(IParameterSymbol parameter)
    {
        foreach (var syntaxReference in parameter.DeclaringSyntaxReferences)
        {
            if (syntaxReference.GetSyntax() is not ParameterSyntax parameterSyntax)
                continue;

            if (parameterSyntax.Parent is not ParameterListSyntax { Parent: TypeDeclarationSyntax typeDeclaration })
                continue;

            var refKeywordKind = parameterSyntax.RefKindKeyword.Kind;
            var typeIsByRef = parameterSyntax.TypeAnnotation?.Type is ByRefTypeSyntax;
            if (refKeywordKind is not SyntaxKind.None || typeIsByRef)
                return false;

            var bindingKeyword = parameterSyntax.BindingKeyword.Kind;
            var isRecord = typeDeclaration is RecordDeclarationSyntax;
            return isRecord || bindingKeyword is SyntaxKind.ValKeyword or SyntaxKind.VarKeyword;
        }

        return false;
    }

    private static string GetPromotedPrimaryConstructorBindingPrefix(IParameterSymbol parameter)
    {
        foreach (var syntaxReference in parameter.DeclaringSyntaxReferences)
        {
            if (syntaxReference.GetSyntax() is not ParameterSyntax parameterSyntax)
                continue;

            if (parameterSyntax.Parent is not ParameterListSyntax { Parent: TypeDeclarationSyntax typeDeclaration })
                continue;

            var refKeywordKind = parameterSyntax.RefKindKeyword.Kind;
            var typeIsByRef = parameterSyntax.TypeAnnotation?.Type is ByRefTypeSyntax;
            if (refKeywordKind is not SyntaxKind.None || typeIsByRef)
                return string.Empty;

            var bindingKeyword = parameterSyntax.BindingKeyword.Kind;
            if (bindingKeyword == SyntaxKind.ValKeyword)
                return "val ";

            if (bindingKeyword == SyntaxKind.VarKeyword)
                return "var ";

            if (typeDeclaration is RecordDeclarationSyntax)
                return parameter.IsMutable ? "var " : "val ";
        }

        return string.Empty;
    }

    private static bool TryGetEnclosingCallableDisplayForLocalFunction(
        IMethodSymbol method,
        SemanticModel semanticModel,
        out string containingDisplay)
    {
        containingDisplay = string.Empty;
        if (!IsFunctionStatementSymbol(method))
            return false;

        var functionSyntax = method.DeclaringSyntaxReferences
            .Select(static r => r.GetSyntax())
            .OfType<FunctionStatementSyntax>()
            .FirstOrDefault();
        if (functionSyntax is null)
            return false;

        var containingSyntax = functionSyntax.Ancestors().FirstOrDefault(static node =>
            node is FunctionStatementSyntax
                or MethodDeclarationSyntax
                or ConstructorDeclarationSyntax
                or ParameterlessConstructorDeclarationSyntax
                or InitializerBlockDeclarationSyntax
                or AccessorDeclarationSyntax);
        if (containingSyntax is null)
            return false;

        var containingSymbol = semanticModel.GetDeclaredSymbol(containingSyntax);
        if (containingSymbol is null)
            return false;

        containingDisplay = FormatEnclosingCallableDisplay(containingSymbol);
        return !string.IsNullOrWhiteSpace(containingDisplay);
    }

    private static bool IsFunctionStatementSymbol(IMethodSymbol method)
    {
        return method.DeclaringSyntaxReferences.Any(static r => r.GetSyntax() is FunctionStatementSyntax);
    }

    private static bool IsLocalFunctionDeclaredStatic(IMethodSymbol method)
    {
        var functionStatement = method.DeclaringSyntaxReferences
            .Select(static r => r.GetSyntax())
            .OfType<FunctionStatementSyntax>()
            .FirstOrDefault();
        if (functionStatement is null)
            return false;

        return functionStatement.Modifiers.Any(static m => m.Kind == SyntaxKind.StaticKeyword);
    }

    private static string FormatEnclosingCallableDisplay(ISymbol symbol)
    {
        if (symbol is not IMethodSymbol method)
        {
            return symbol.ToDisplayString(
                SymbolDisplayFormat.RavenSignatureFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly));
        }

        var plainTypeFormat = CreatePlainTypeFormat();
        var parameters = FormatParameters(method.Parameters, plainTypeFormat);
        var returnType = method.ReturnType.ToDisplayString(plainTypeFormat);
        var staticPrefix = IsMethodDeclaredStaticForDisplay(method) ? "static " : string.Empty;
        return $"{staticPrefix}func {method.Name}({parameters}) -> {returnType}";
    }

    private static SymbolDisplayFormat CreatePlainTypeFormat()
    {
        var miscOptions = SymbolDisplayFormat.RavenSignatureFormat.MiscellaneousOptions |
                          SymbolDisplayMiscellaneousOptions.IncludeTupleElementNames;

        return SymbolDisplayFormat.RavenSignatureFormat
            .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
            .WithKindOptions(SymbolDisplayKindOptions.None)
            .WithMiscellaneousOptions(miscOptions);
    }

    private static bool IsMethodDeclaredStaticForDisplay(IMethodSymbol method)
    {
        foreach (var syntax in method.DeclaringSyntaxReferences.Select(static r => r.GetSyntax()))
        {
            switch (syntax)
            {
                case FunctionStatementSyntax function:
                    return function.Modifiers.Any(static m => m.Kind == SyntaxKind.StaticKeyword);
                case MethodDeclarationSyntax declaration:
                    return declaration.Modifiers.Any(static m => m.Kind == SyntaxKind.StaticKeyword);
                case ConstructorDeclarationSyntax declaration:
                    return declaration.Modifiers.Any(static m => m.Kind == SyntaxKind.StaticKeyword);
                case ParameterlessConstructorDeclarationSyntax declaration:
                    return declaration.Modifiers.Any(static m => m.Kind == SyntaxKind.StaticKeyword);
                case InitializerBlockDeclarationSyntax declaration:
                    return declaration.Modifiers.Any(static m => m.Kind == SyntaxKind.StaticKeyword);
            }
        }

        return method.IsStatic;
    }

    private static bool TryInferLambdaParameterTypeFromContext(
        IParameterSymbol parameter,
        SyntaxNode contextNode,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
        => TryInferLambdaParameterTypeByNameFromContext(parameter.Name, contextNode, semanticModel, out inferredType);

    private static bool TryInferLambdaParameterTypeFromFunctionTarget(
        SyntaxNode contextNode,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        var parameterSyntax = contextNode.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault();
        var functionExpression = contextNode.AncestorsAndSelf().OfType<FunctionExpressionSyntax>().FirstOrDefault();
        if (parameterSyntax is null || functionExpression is null)
            return false;

        var functionType = semanticModel.TryGetFunctionExpressionDelegateType(functionExpression, out var contextualFunctionType)
            ? contextualFunctionType
            : semanticModel.GetTypeInfo(functionExpression).ConvertedType
                ?? semanticModel.GetTypeInfo(functionExpression).Type;

        var delegateType = UnwrapDelegateType(functionType);
        var invokeMethod = delegateType?.GetDelegateInvokeMethod();
        if (invokeMethod is null || invokeMethod.Parameters.IsDefaultOrEmpty)
            return false;

        var parameterIndex = GetLambdaParameterIndex(functionExpression, parameterSyntax.Identifier.ValueText);
        if (parameterIndex < 0 || parameterIndex >= invokeMethod.Parameters.Length)
            return false;

        var parameterType = invokeMethod.Parameters[parameterIndex].Type;
        if (parameterType is null || parameterType.ContainsErrorType())
            return false;

        inferredType = parameterType is NullableTypeSymbol nullable ? nullable.UnderlyingType : parameterType;
        return true;
    }

    private static bool TryInferDeclaredTypeFromContext(
        SyntaxNode contextNode,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        foreach (var typeSyntax in contextNode.AncestorsAndSelf().OfType<TypeSyntax>())
        {
            if (!TryResolveTypeSymbolFromSyntax(semanticModel, typeSyntax, out var resolvedType))
                continue;

            inferredType = resolvedType;
            return true;
        }

        return false;
    }

    private static bool TryInferDeclaredTypeAtOffset(
        SyntaxNode root,
        int offset,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        foreach (var candidateOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidateOffset);
            }
            catch
            {
                continue;
            }

            var typeSyntax = token.Parent?.AncestorsAndSelf().OfType<TypeSyntax>().FirstOrDefault();
            if (typeSyntax is null)
                continue;

            var typeSyntaxes = token.Parent!
                .AncestorsAndSelf()
                .OfType<TypeSyntax>()
                .Where(typeNode => typeNode.Span.Contains(token.Span));

            foreach (var candidateTypeSyntax in typeSyntaxes)
            {
                if (!TryResolveTypeSymbolFromSyntax(semanticModel, candidateTypeSyntax, out var resolvedType))
                    continue;

                inferredType = resolvedType;
                return true;
            }
        }

        return false;
    }

    private static bool TryResolveTypeSymbolFromSyntax(
        SemanticModel semanticModel,
        TypeSyntax typeSyntax,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        var typeInfo = semanticModel.GetTypeInfo(typeSyntax);
        var resolvedType = typeInfo.Type ?? typeInfo.ConvertedType;
        if (resolvedType is null || resolvedType.TypeKind == TypeKind.Error)
        {
            var typeSymbol = SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, typeSyntax, out var typeSymbolInfo)
                ? typeSymbolInfo.Symbol
                : null;
            resolvedType = typeSymbol switch
            {
                ITypeSymbol resolved => resolved,
                IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasedType } => aliasedType,
                _ => resolvedType
            };
        }

        if ((resolvedType is null || resolvedType.TypeKind == TypeKind.Error || resolvedType is IArrayTypeSymbol { ElementType.TypeKind: TypeKind.Error }) &&
            TryResolveArrayTypeFromSyntax(semanticModel, typeSyntax, out var reconstructedArrayType))
        {
            resolvedType = reconstructedArrayType;
        }

        if ((resolvedType is null || resolvedType.TypeKind == TypeKind.Error) &&
            TryResolveBuiltInTypeFromSyntax(semanticModel, typeSyntax, out var builtInType))
        {
            resolvedType = builtInType;
        }

        if (resolvedType is null || resolvedType.TypeKind == TypeKind.Error)
            return false;

        inferredType = resolvedType;
        return true;
    }

    private static bool IsFunctionExpressionIdentifierToken(FunctionExpressionSyntax functionExpression, SyntaxToken token)
        => functionExpression is ParenthesizedFunctionExpressionSyntax parenthesized &&
           token == parenthesized.Identifier;

    private static bool IsInsideFunctionExpressionBody(FunctionExpressionSyntax functionExpression, SyntaxToken token)
    {
        var body = (SyntaxNode?)functionExpression.Body ?? functionExpression.ExpressionBody;
        return body is not null && body.Span.Contains(token.Span);
    }

    private static bool TryResolveArrayTypeFromSyntax(
        SemanticModel semanticModel,
        TypeSyntax typeSyntax,
        out ITypeSymbol resolvedType)
    {
        resolvedType = null!;

        if (typeSyntax is not ArrayTypeSyntax arrayTypeSyntax)
            return false;

        if (!TryResolveTypeSymbolFromSyntax(semanticModel, arrayTypeSyntax.ElementType, out var currentElementType))
            return false;

        var currentType = currentElementType;
        foreach (var rankSpecifier in arrayTypeSyntax.RankSpecifiers)
        {
            var rank = rankSpecifier.CommaTokens.Count + 1;
            currentType = semanticModel.Compilation.CreateArrayTypeSymbol(
                currentType,
                rank,
                TryGetFixedArraySize(rankSpecifier));
        }

        resolvedType = currentType;
        return true;
    }

    private static int? TryGetFixedArraySize(ArrayRankSpecifierSyntax rankSpecifier)
        => rankSpecifier.CommaTokens.Count == 0 &&
           rankSpecifier.SizeToken.Kind == SyntaxKind.NumericLiteralToken &&
           int.TryParse(rankSpecifier.SizeToken.ValueText, out var parsedSize)
            ? parsedSize
            : null;

    private static bool TryResolveBuiltInTypeFromSyntax(
        SemanticModel semanticModel,
        TypeSyntax typeSyntax,
        out ITypeSymbol resolvedType)
    {
        resolvedType = null!;

        var specialType = typeSyntax.ToString() switch
        {
            "bool" => SpecialType.System_Boolean,
            "byte" => SpecialType.System_Byte,
            "sbyte" => SpecialType.System_SByte,
            "short" => SpecialType.System_Int16,
            "ushort" => SpecialType.System_UInt16,
            "int" => SpecialType.System_Int32,
            "uint" => SpecialType.System_UInt32,
            "long" => SpecialType.System_Int64,
            "ulong" => SpecialType.System_UInt64,
            "char" => SpecialType.System_Char,
            "float" => SpecialType.System_Single,
            "double" => SpecialType.System_Double,
            "decimal" => SpecialType.System_Decimal,
            "string" => SpecialType.System_String,
            "object" => SpecialType.System_Object,
            _ => SpecialType.None
        };

        if (specialType == SpecialType.None)
            return false;

        resolvedType = semanticModel.Compilation.GetSpecialType(specialType);
        return resolvedType.TypeKind != TypeKind.Error;
    }

    private static bool TryInferLambdaParameterTypeByNameFromContext(
        string parameterName,
        SyntaxNode contextNode,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        var functionExpression = contextNode.AncestorsAndSelf().OfType<FunctionExpressionSyntax>().FirstOrDefault();
        if (functionExpression is null)
            return false;

        if (!SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, functionExpression, out var functionInfo))
            functionInfo = default;
        var lambdaParameterIndex = GetLambdaParameterIndex(functionExpression, parameterName);
        if (functionInfo.Symbol is IMethodSymbol functionMethod &&
            !functionMethod.Parameters.IsDefaultOrEmpty)
        {
            var fromMethod = TryGetDelegateParameter(functionMethod.Parameters, parameterName, lambdaParameterIndex);

            if (fromMethod is not null &&
                fromMethod.Type is { TypeKind: not TypeKind.Error } typedFromMethod)
            {
                inferredType = typedFromMethod is NullableTypeSymbol nullable ? nullable.UnderlyingType : typedFromMethod;
                return true;
            }
        }

        if (functionExpression.Parent is not ArgumentSyntax argument ||
            argument.Parent is not ArgumentListSyntax argumentList ||
            argumentList.Parent is not InvocationExpressionSyntax invocation)
        {
            return TryInferLambdaParameterTypeFromAssignmentTarget(
                parameterName,
                functionExpression,
                semanticModel,
                out inferredType);
        }

        var argumentIndex = 0;
        foreach (var current in argumentList.Arguments)
        {
            if (current.Span == argument.Span && current.Kind == argument.Kind)
                break;

            argumentIndex++;
        }

        if (invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
            memberAccess.Name is IdentifierNameSyntax memberNameIdentifier &&
            semanticModel.GetTypeInfo(memberAccess.Expression).Type is INamedTypeSymbol invocationReceiverType &&
            invocationReceiverType.TypeKind != TypeKind.Error)
        {
            foreach (var method in invocationReceiverType.GetMembers(memberNameIdentifier.Identifier.ValueText).OfType<IMethodSymbol>())
            {
                if (method.Parameters.Length <= argumentIndex)
                    continue;

                if (method.Parameters[argumentIndex].Type is not INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType)
                    continue;

                var invokeMethod = delegateType.GetDelegateInvokeMethod();
                if (invokeMethod is null || invokeMethod.Parameters.IsDefaultOrEmpty)
                    continue;

                var delegateParameter = TryGetDelegateParameter(invokeMethod.Parameters, parameterName, lambdaParameterIndex);

                if (delegateParameter is null || delegateParameter.Type.TypeKind == TypeKind.Error)
                    continue;

                inferredType = delegateParameter.Type is NullableTypeSymbol nullable
                    ? nullable.UnderlyingType
                    : delegateParameter.Type;
                return true;
            }
        }

        static IEnumerable<IMethodSymbol> EnumerateCandidateMethods(SymbolInfo info)
        {
            if (info.Symbol is IMethodSymbol method)
                yield return method;

            if (info.CandidateSymbols.IsDefaultOrEmpty)
                yield break;

            foreach (var candidate in info.CandidateSymbols.OfType<IMethodSymbol>())
                yield return candidate;
        }

        if (!SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, invocation, out var invocationInfo))
            invocationInfo = default;
        foreach (var method in EnumerateCandidateMethods(invocationInfo))
        {
            if (method.Parameters.Length <= argumentIndex)
                continue;

            if (method.Parameters[argumentIndex].Type is not INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType)
                continue;

            var invokeMethod = delegateType.GetDelegateInvokeMethod();
            if (invokeMethod is null || invokeMethod.Parameters.IsDefaultOrEmpty)
                continue;

            var delegateParameter = TryGetDelegateParameter(invokeMethod.Parameters, parameterName, lambdaParameterIndex);

            if (delegateParameter is null)
                continue;

            inferredType = delegateParameter.Type is NullableTypeSymbol nullable
                ? nullable.UnderlyingType
                : delegateParameter.Type;
            return inferredType.TypeKind != TypeKind.Error;
        }

        return false;
    }

    private static bool TryInferLambdaParameterTypeFromAssignmentTarget(
        string parameterName,
        FunctionExpressionSyntax functionExpression,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        ExpressionSyntax? targetExpression = null;

        if (functionExpression.Parent is AssignmentExpressionSyntax assignmentExpression &&
            assignmentExpression.Right.Span == functionExpression.Span &&
            assignmentExpression.Right.Kind == functionExpression.Kind &&
            assignmentExpression.Left is ExpressionSyntax assignmentExpressionLeft)
        {
            targetExpression = assignmentExpressionLeft;
        }
        else if (functionExpression.Parent is AssignmentStatementSyntax assignmentStatement &&
                 assignmentStatement.Right.Span == functionExpression.Span &&
                 assignmentStatement.Right.Kind == functionExpression.Kind &&
                 assignmentStatement.Left is ExpressionSyntax assignmentStatementLeft)
        {
            targetExpression = assignmentStatementLeft;
        }

        if (targetExpression is null)
            return false;

        var targetType = semanticModel.GetTypeInfo(targetExpression).ConvertedType
            ?? semanticModel.GetTypeInfo(targetExpression).Type;

        var delegateType = UnwrapDelegateType(targetType);
        if (delegateType is null)
        {
            if (targetExpression is MemberAccessExpressionSyntax memberAccess &&
                memberAccess.Name is IdentifierNameSyntax memberName &&
                semanticModel.GetTypeInfo(memberAccess.Expression).Type is INamedTypeSymbol receiverType &&
                receiverType.TypeKind != TypeKind.Error)
            {
                for (var currentType = receiverType; currentType is not null; currentType = currentType.BaseType)
                {
                    targetType = currentType.GetMembers(memberName.Identifier.ValueText)
                        .Select(member => member switch
                        {
                            IEventSymbol eventSymbol => eventSymbol.Type,
                            IPropertySymbol property => property.Type,
                            IFieldSymbol field => field.Type,
                            _ => null
                        })
                        .FirstOrDefault(type => type is not null && !type.ContainsErrorType());

                    if (targetType is not null)
                        break;
                }
            }

            var targetSymbol = targetExpression is MemberAccessExpressionSyntax targetMemberAccess
                ? (SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, targetMemberAccess.Name, out var targetMemberInfo)
                    ? targetMemberInfo.Symbol
                    : null)
                  ?? (SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, targetExpression, out var targetExpressionInfo)
                    ? targetExpressionInfo.Symbol
                    : null)
                : (SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, targetExpression, out var directTargetInfo)
                    ? directTargetInfo.Symbol
                    : null);

            if (targetSymbol is IMethodSymbol { AssociatedSymbol: { } associatedSymbol })
                targetSymbol = associatedSymbol;
            else if (targetSymbol is IFieldSymbol { AssociatedSymbol: { } associatedFieldSymbol })
                targetSymbol = associatedFieldSymbol;

            targetType = targetSymbol switch
            {
                IEventSymbol eventSymbol => eventSymbol.Type,
                ILocalSymbol local => local.Type,
                IFieldSymbol field => field.Type,
                IPropertySymbol property => property.Type,
                IParameterSymbol parameter => parameter.Type,
                _ => targetType
            };

            delegateType = UnwrapDelegateType(targetType);
            if (delegateType is null)
                return false;
        }

        var invokeMethod = delegateType.GetDelegateInvokeMethod();
        if (invokeMethod is null || invokeMethod.Parameters.IsDefaultOrEmpty)
            return false;

        var lambdaParameterIndex = GetLambdaParameterIndex(functionExpression, parameterName);
        var delegateParameter = TryGetDelegateParameter(invokeMethod.Parameters, parameterName, lambdaParameterIndex);

        if (delegateParameter is null || delegateParameter.Type.ContainsErrorType())
            return false;

        inferredType = delegateParameter.Type is NullableTypeSymbol nullable
            ? nullable.UnderlyingType
            : delegateParameter.Type;
        return true;
    }

    private static INamedTypeSymbol? UnwrapDelegateType(ITypeSymbol? type)
    {
        return type switch
        {
            INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType => delegateType,
            NullableTypeSymbol { UnderlyingType: INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType } => delegateType,
            _ => null
        };
    }

    private static int GetLambdaParameterIndex(FunctionExpressionSyntax functionExpression, string parameterName)
    {
        if (functionExpression is not ParenthesizedFunctionExpressionSyntax parenthesized ||
            parenthesized.ParameterList is null)
            return -1;

        var parameters = parenthesized.ParameterList.Parameters;
        if (parameters.Green is null || parameters.Count == 0)
            return -1;

        for (var i = 0; i < parameters.Count; i++)
        {
            var parameter = parameters[i];
            if (parameter is null)
                continue;

            var identifier = parameter.Identifier;
            if (identifier.Kind == SyntaxKind.None || identifier.IsMissing)
                continue;

            if (string.Equals(identifier.ValueText, parameterName, StringComparison.Ordinal))
                return i;
        }

        return -1;
    }

    private static IParameterSymbol? TryGetDelegateParameter(
        ImmutableArray<IParameterSymbol> parameters,
        string parameterName,
        int parameterIndex)
    {
        if (parameters.IsDefaultOrEmpty)
            return null;

        if (parameters.Length == 1)
            return parameters[0];

        if (parameterIndex >= 0 && parameterIndex < parameters.Length)
            return parameters[parameterIndex];

        return parameters.FirstOrDefault(p => string.Equals(p.Name, parameterName, StringComparison.Ordinal));
    }

    private static bool TryInferReceiverTypeFromMemberAccessContext(
        string symbolName,
        SyntaxNode contextNode,
        SemanticModel semanticModel,
        out ITypeSymbol inferredType)
    {
        inferredType = null!;

        var receiverIdentifier = contextNode switch
        {
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                     memberAccess.Expression.Span == identifier.Span &&
                     memberAccess.Expression.Kind == identifier.Kind &&
                     string.Equals(identifier.Identifier.ValueText, symbolName, StringComparison.Ordinal)
                => identifier,
            MemberAccessExpressionSyntax { Expression: IdentifierNameSyntax identifier }
                when string.Equals(identifier.Identifier.ValueText, symbolName, StringComparison.Ordinal)
                => identifier,
            _ => contextNode.AncestorsAndSelf()
                .OfType<IdentifierNameSyntax>()
                .FirstOrDefault(identifier =>
                    identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                    memberAccess.Expression.Span == identifier.Span &&
                    memberAccess.Expression.Kind == identifier.Kind &&
                    string.Equals(identifier.Identifier.ValueText, symbolName, StringComparison.Ordinal))
        };

        if (receiverIdentifier is null ||
            receiverIdentifier.Parent is not MemberAccessExpressionSyntax receiverMemberAccess)
        {
            return false;
        }

        var receiverType = semanticModel.GetTypeInfo(receiverMemberAccess.Expression).Type;
        if ((receiverType is null || receiverType.TypeKind == TypeKind.Error) &&
            TryInferLambdaParameterTypeByNameFromContext(symbolName, receiverIdentifier, semanticModel, out var inferredLambdaType))
        {
            receiverType = inferredLambdaType;
        }

        if (receiverType is not null && receiverType.TypeKind != TypeKind.Error)
        {
            inferredType = receiverType;
            return true;
        }

        var accessedMember = SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, receiverMemberAccess.Name, out var accessedMemberInfo)
            ? accessedMemberInfo.Symbol
            : null;
        var inferredContainingType = accessedMember switch
        {
            IPropertySymbol property => property.ContainingType,
            IFieldSymbol field => field.ContainingType,
            IMethodSymbol method => method.ContainingType,
            _ => null
        };

        if (inferredContainingType is null || inferredContainingType.TypeKind == TypeKind.Error)
            return false;

        inferredType = inferredContainingType;
        return true;
    }

    private static string FormatParameters(IEnumerable<IParameterSymbol> parameters, SymbolDisplayFormat format)
    {
        return string.Join(
            ", ",
            parameters.Select(parameter =>
            {
                var paramsPrefix = parameter.IsVarParams ? "params " : string.Empty;
                var refPrefix = parameter.RefKind switch
                {
                    RefKind.Ref => "ref ",
                    RefKind.Out => "out ",
                    RefKind.In => "in ",
                    RefKind.RefReadOnly => "ref readonly ",
                    RefKind.RefReadOnlyParameter => "ref readonly ",
                    _ => string.Empty
                };
                var parameterType = parameter.Type.ToDisplayString(format);
                return $"{paramsPrefix}{refPrefix}{parameter.Name}: {parameterType}";
            }));
    }

    private static string? FormatDocumentation(DocumentationComment? documentation)
    {
        return DocumentationMarkdownFormatter.FormatForEditor(documentation);
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

internal readonly record struct HoverCacheKey(string Uri, VersionStamp Version, int Line, int Character);
internal readonly record struct HoverCacheEntry(Hover? Hover);
