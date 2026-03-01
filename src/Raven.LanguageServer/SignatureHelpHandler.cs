using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class SignatureHelpHandler : ISignatureHelpHandler
{
    private readonly DocumentStore _documents;
    private readonly ILogger<SignatureHelpHandler> _logger;

    public SignatureHelpHandler(DocumentStore documents, ILogger<SignatureHelpHandler> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    public SignatureHelpRegistrationOptions GetRegistrationOptions(SignatureHelpCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven"),
            TriggerCharacters = new Container<string>("(", ","),
            RetriggerCharacters = new Container<string>(")")
        };

    public void SetCapability(SignatureHelpCapability capability)
    {
    }

    public async Task<SignatureHelp?> Handle(SignatureHelpParams request, CancellationToken cancellationToken)
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

            var invocation = TryGetInvocationAtPosition(root, offset);
            if (invocation is null)
                return null;

            var symbolInfo = semanticModel.GetSymbolInfo(invocation);
            var methods = GetCandidateMethods(symbolInfo, semanticModel, invocation);
            if (methods.IsDefaultOrEmpty)
                return null;

            var argumentIndex = GetArgumentIndex(invocation.ArgumentList, offset);
            var activeSignature = GetActiveSignatureIndex(methods, symbolInfo.Symbol as IMethodSymbol, argumentIndex);
            var activeParameter = GetActiveParameterIndex(methods[activeSignature], invocation.ArgumentList, offset, argumentIndex);

            var plainTypeFormat = SymbolDisplayFormat.RavenSignatureFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
                .WithKindOptions(SymbolDisplayKindOptions.None);

            var signatures = methods
                .Select(method => CreateSignatureInformation(method, plainTypeFormat))
                .ToArray();

            return new SignatureHelp
            {
                Signatures = new Container<SignatureInformation>(signatures),
                ActiveSignature = activeSignature,
                ActiveParameter = activeParameter
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
                "Signature help request failed for {Uri} at {Line}:{Character}.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character);
            return null;
        }
    }

    private static SignatureInformation CreateSignatureInformation(IMethodSymbol method, SymbolDisplayFormat plainTypeFormat)
    {
        var name = method.MethodKind == MethodKind.Constructor
            ? method.ContainingType?.Name ?? method.Name
            : method.Name;

        var parameterLabels = method.Parameters
            .Select(parameter => FormatParameter(parameter, plainTypeFormat))
            .ToArray();

        var signatureLabel = method.MethodKind == MethodKind.Constructor
            ? $"{name}({string.Join(", ", parameterLabels)})"
            : $"{name}({string.Join(", ", parameterLabels)}) -> {method.ReturnType.ToDisplayString(plainTypeFormat)}";

        var parameterInfos = method.Parameters
            .Select(parameter => new ParameterInformation
            {
                Label = FormatParameter(parameter, plainTypeFormat)
            })
            .ToArray();

        return new SignatureInformation
        {
            Label = signatureLabel,
            Parameters = new Container<ParameterInformation>(parameterInfos),
            Documentation = FormatDocumentation(method.GetDocumentationComment())
        };
    }

    private static string FormatParameter(IParameterSymbol parameter, SymbolDisplayFormat plainTypeFormat)
    {
        var refPrefix = parameter.RefKind switch
        {
            RefKind.Ref => "ref ",
            RefKind.Out => "out ",
            RefKind.In => "in ",
            RefKind.RefReadOnly => "ref readonly ",
            RefKind.RefReadOnlyParameter => "ref readonly ",
            _ => string.Empty
        };

        var typeDisplay = parameter.Type.ToDisplayString(plainTypeFormat);
        return $"{refPrefix}{parameter.Name}: {typeDisplay}";
    }

    private static StringOrMarkupContent? FormatDocumentation(DocumentationComment? documentation)
    {
        if (documentation is null || string.IsNullOrWhiteSpace(documentation.Content))
            return null;

        return documentation.Format switch
        {
            DocumentationFormat.Markdown => new StringOrMarkupContent(new MarkupContent
            {
                Kind = MarkupKind.Markdown,
                Value = documentation.Content.Trim()
            }),
            DocumentationFormat.Xml => new StringOrMarkupContent(new MarkupContent
            {
                Kind = MarkupKind.Markdown,
                Value = $"```xml\n{documentation.Content.Trim()}\n```"
            }),
            _ => new StringOrMarkupContent(documentation.Content.Trim())
        };
    }

    private static ImmutableArray<IMethodSymbol> GetCandidateMethods(
        SymbolInfo symbolInfo,
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation)
    {
        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        void AddIfNotPresent(IMethodSymbol method)
        {
            foreach (var existing in builder)
            {
                if (SymbolEqualityComparer.Default.Equals(existing, method))
                    return;
            }

            builder.Add(method);
        }

        if (symbolInfo.Symbol is IMethodSymbol selectedMethod)
            AddIfNotPresent(selectedMethod);

        if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            foreach (var candidate in symbolInfo.CandidateSymbols.OfType<IMethodSymbol>())
                AddIfNotPresent(candidate);
        }

        if (builder.Count == 0)
        {
            if (symbolInfo.Symbol is INamedTypeSymbol selectedType)
            {
                foreach (var constructor in selectedType.InstanceConstructors)
                    AddIfNotPresent(constructor);
            }

            if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                foreach (var candidateType in symbolInfo.CandidateSymbols.OfType<INamedTypeSymbol>())
                {
                    foreach (var constructor in candidateType.InstanceConstructors)
                        AddIfNotPresent(constructor);
                }
            }
        }

        if (builder.Count == 0)
        {
            var typeInfo = semanticModel.GetTypeInfo(invocation.Expression);
            if ((typeInfo.Type ?? typeInfo.ConvertedType) is INamedTypeSymbol expressionType)
            {
                foreach (var constructor in expressionType.InstanceConstructors)
                    AddIfNotPresent(constructor);
            }
        }

        return builder
            .OrderBy(method => method.Parameters.Length)
            .ThenBy(method => method.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat))
            .ToImmutableArray();
    }

    private static InvocationExpressionSyntax? TryGetInvocationAtPosition(SyntaxNode root, int offset)
    {
        foreach (var normalizedOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(normalizedOffset);
            }
            catch
            {
                continue;
            }

            var invocation = token.Parent?
                .AncestorsAndSelf()
                .OfType<InvocationExpressionSyntax>()
                .FirstOrDefault(candidate =>
                    IsWithinArgumentList(candidate.ArgumentList, normalizedOffset));

            if (invocation is not null)
                return invocation;
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

    private static bool IsWithinArgumentList(ArgumentListSyntax argumentList, int offset)
    {
        var start = argumentList.OpenParenToken.Span.Start;
        var end = argumentList.CloseParenToken.Span.End;
        return offset >= start && offset <= end;
    }

    private static int GetArgumentIndex(ArgumentListSyntax argumentList, int offset)
    {
        if (argumentList.Arguments.Count == 0)
            return 0;

        var firstArgument = argumentList.Arguments[0];
        if (offset <= firstArgument.Span.Start)
            return 0;

        var commaCount = argumentList.Arguments
            .GetSeparators()
            .Count(separator => separator.Span.Start < offset);

        return Math.Max(0, commaCount);
    }

    private static int GetActiveSignatureIndex(
        ImmutableArray<IMethodSymbol> methods,
        IMethodSymbol? selectedMethod,
        int argumentIndex)
    {
        if (selectedMethod is not null)
        {
            for (var i = 0; i < methods.Length; i++)
            {
                if (SymbolEqualityComparer.Default.Equals(methods[i], selectedMethod))
                    return i;
            }
        }

        var typedArgumentCount = argumentIndex + 1;
        var bestIndex = 0;
        var bestScore = int.MaxValue;

        for (var i = 0; i < methods.Length; i++)
        {
            var method = methods[i];
            var required = method.Parameters.Count(parameter => !parameter.IsOptional);
            var total = method.Parameters.Length;
            var applicable = typedArgumentCount >= required && typedArgumentCount <= total;
            var distance = Math.Abs(total - typedArgumentCount);
            var score = (applicable ? 0 : 10_000) + distance * 100 + total;

            if (score < bestScore)
            {
                bestScore = score;
                bestIndex = i;
            }
        }

        return bestIndex;
    }

    private static int GetActiveParameterIndex(
        IMethodSymbol method,
        ArgumentListSyntax argumentList,
        int offset,
        int fallbackArgumentIndex)
    {
        if (method.Parameters.Length == 0)
            return 0;

        foreach (var argument in argumentList.Arguments)
        {
            if (offset < argument.FullSpan.Start || offset > argument.FullSpan.End)
                continue;

            var name = argument.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrWhiteSpace(name))
                break;

            for (var i = 0; i < method.Parameters.Length; i++)
            {
                if (string.Equals(method.Parameters[i].Name, name, StringComparison.Ordinal))
                    return i;
            }

            break;
        }

        return Math.Clamp(fallbackArgumentIndex, 0, method.Parameters.Length - 1);
    }
}
