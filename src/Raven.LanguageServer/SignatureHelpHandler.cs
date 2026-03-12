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
            TriggerCharacters = new Container<string>("(", "[", ","),
            RetriggerCharacters = new Container<string>(")", "]")
        };

    public void SetCapability(SignatureHelpCapability capability)
    {
    }

    public async Task<SignatureHelp?> Handle(SignatureHelpParams request, CancellationToken cancellationToken)
    {
        try
        {
            using var _ = await _documents.EnterCompilerAccessAsync(cancellationToken).ConfigureAwait(false);
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
            var plainTypeFormat = SymbolDisplayFormat.RavenSignatureFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
                .WithKindOptions(SymbolDisplayKindOptions.None);

            if (invocation is not null)
            {
                var symbolInfo = semanticModel.GetSymbolInfo(invocation);
                var methods = GetCandidateMethods(symbolInfo, semanticModel, invocation);
                if (methods.IsDefaultOrEmpty)
                    return null;

                var argumentIndex = GetArgumentIndex(invocation.ArgumentList, offset);
                var activeSignature = GetActiveSignatureIndex(methods, symbolInfo.Symbol as IMethodSymbol, argumentIndex);
                var activeParameter = GetActiveParameterIndex(methods[activeSignature], invocation.ArgumentList, offset, argumentIndex);

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

            var elementAccessContext = TryGetElementAccessContextAtPosition(root, offset);
            if (elementAccessContext is null)
                return null;

            var indexers = GetCandidateIndexers(semanticModel, elementAccessContext);
            if (indexers.IsDefaultOrEmpty)
                return null;

            var bracketArgumentIndex = GetArgumentIndex(elementAccessContext.ArgumentList, offset);
            var selectedIndexer = semanticModel.GetSymbolInfo(elementAccessContext.SymbolInfoNode).Symbol as IPropertySymbol;
            var activeIndexerSignature = GetActiveSignatureIndex(indexers, selectedIndexer, bracketArgumentIndex);
            var activeIndexerParameter = GetActiveParameterIndex(indexers[activeIndexerSignature], elementAccessContext.ArgumentList, offset, bracketArgumentIndex);

            var indexerSignatures = indexers
                .Select(indexer => CreateSignatureInformation(indexer, plainTypeFormat))
                .ToArray();

            return new SignatureHelp
            {
                Signatures = new Container<SignatureInformation>(indexerSignatures),
                ActiveSignature = activeIndexerSignature,
                ActiveParameter = activeIndexerParameter
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
        string name;
        string typeParams;

        if (method.MethodKind == MethodKind.Constructor)
        {
            var containingType = method.ContainingType;
            name = containingType?.Name ?? method.Name;
            typeParams = containingType is not null && !containingType.TypeParameters.IsDefaultOrEmpty
                ? $"<{string.Join(", ", containingType.TypeParameters.Select(static tp => tp.Name))}>"
                : string.Empty;
        }
        else
        {
            name = method.Name;
            typeParams = !method.TypeParameters.IsDefaultOrEmpty
                ? $"<{string.Join(", ", method.TypeParameters.Select(static tp => tp.Name))}>"
                : string.Empty;
        }

        var parameterLabels = method.Parameters
            .Select(parameter => FormatParameter(parameter, plainTypeFormat))
            .ToArray();

        var signatureLabel = method.MethodKind == MethodKind.Constructor
            ? $"{name}{typeParams}({string.Join(", ", parameterLabels)})"
            : $"func {name}{typeParams}({string.Join(", ", parameterLabels)}) -> {method.ReturnType.ToDisplayString(plainTypeFormat)}";

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

    private static SignatureInformation CreateSignatureInformation(IPropertySymbol property, SymbolDisplayFormat plainTypeFormat)
    {
        var parameterLabels = property.Parameters
            .Select(parameter => FormatParameter(parameter, plainTypeFormat))
            .ToArray();

        var signatureLabel = $"this[{string.Join(", ", parameterLabels)}] -> {property.Type.ToDisplayString(plainTypeFormat)}";
        var parameterInfos = property.Parameters
            .Select(parameter => new ParameterInformation
            {
                Label = FormatParameter(parameter, plainTypeFormat)
            })
            .ToArray();

        return new SignatureInformation
        {
            Label = signatureLabel,
            Parameters = new Container<ParameterInformation>(parameterInfos),
            Documentation = FormatDocumentation(property.GetDocumentationComment())
        };
    }

    private static string FormatParameter(IParameterSymbol parameter, SymbolDisplayFormat plainTypeFormat)
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

        var typeDisplay = parameter.Type.ToDisplayString(plainTypeFormat);
        return $"{paramsPrefix}{refPrefix}{parameter.Name}: {typeDisplay}";
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
                if (expressionType.GetDelegateInvokeMethod() is { } invokeMethod)
                    AddIfNotPresent(invokeMethod);

                AddInvokeCandidatesFromType(expressionType, AddIfNotPresent);

                foreach (var constructor in expressionType.InstanceConstructors)
                    AddIfNotPresent(constructor);
            }
        }

        if (builder.Count == 0 &&
            invocation.Expression is ReceiverBindingExpressionSyntax &&
            invocation.Parent is ConditionalAccessExpressionSyntax conditionalAccess)
        {
            var receiverType = GetConditionalAccessLookupType(semanticModel.GetTypeInfo(conditionalAccess.Expression).Type);
            if (receiverType is INamedTypeSymbol receiverNamedType)
            {
                if (receiverNamedType.GetDelegateInvokeMethod() is { } invokeMethod)
                    AddIfNotPresent(invokeMethod);

                AddInvokeCandidatesFromType(receiverNamedType, AddIfNotPresent);
            }
        }

        return builder
            .OrderBy(method => method.Parameters.Length)
            .ThenBy(method => method.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat))
            .ToImmutableArray();
    }

    private static void AddInvokeCandidatesFromType(
        INamedTypeSymbol type,
        Action<IMethodSymbol> addIfNotPresent)
    {
        foreach (var invokeCandidate in type.GetMembers("Invoke").OfType<IMethodSymbol>())
        {
            if (invokeCandidate.IsStatic)
                continue;

            addIfNotPresent(invokeCandidate);
        }
    }

    private static ImmutableArray<IPropertySymbol> GetCandidateIndexers(
        SemanticModel semanticModel,
        ElementAccessContext context)
    {
        var builder = ImmutableArray.CreateBuilder<IPropertySymbol>();

        void AddIfNotPresent(IPropertySymbol property)
        {
            if (!property.IsIndexer)
                return;

            foreach (var existing in builder)
            {
                if (SymbolEqualityComparer.Default.Equals(existing, property))
                    return;
            }

            builder.Add(property);
        }

        var symbolInfo = semanticModel.GetSymbolInfo(context.SymbolInfoNode);
        if (symbolInfo.Symbol is IPropertySymbol selectedIndexer)
            AddIfNotPresent(selectedIndexer);

        if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            foreach (var candidate in symbolInfo.CandidateSymbols.OfType<IPropertySymbol>())
                AddIfNotPresent(candidate);
        }

        if (builder.Count == 0)
        {
            var receiverType = GetConditionalAccessLookupType(semanticModel.GetTypeInfo(context.ReceiverExpression).Type);
            if (receiverType is ITypeSymbol type)
            {
                foreach (var indexer in type.GetMembers().OfType<IPropertySymbol>().Where(static p => p.IsIndexer))
                    AddIfNotPresent(indexer);
            }
        }

        return builder
            .OrderBy(property => property.Parameters.Length)
            .ThenBy(property => property.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat))
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

    private static ElementAccessContext? TryGetElementAccessContextAtPosition(SyntaxNode root, int offset)
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

            var elementAccess = token.Parent?
                .AncestorsAndSelf()
                .OfType<ElementAccessExpressionSyntax>()
                .FirstOrDefault(candidate =>
                    IsWithinArgumentList(candidate.ArgumentList, normalizedOffset));

            if (elementAccess is not null)
                return new ElementAccessContext(elementAccess.Expression, elementAccess.ArgumentList, elementAccess);

            var conditionalElementBinding = token.Parent?
                .AncestorsAndSelf()
                .OfType<ConditionalAccessExpressionSyntax>()
                .Select(static conditional => (conditional, binding: conditional.WhenNotNull as ElementBindingExpressionSyntax))
                .FirstOrDefault(tuple =>
                    tuple.binding is not null &&
                    IsWithinArgumentList(tuple.binding.ArgumentList, normalizedOffset));

            if (conditionalElementBinding is not null)
            {
                var conditional = conditionalElementBinding.Value.conditional;
                var binding = conditionalElementBinding.Value.binding;
                if (binding is null)
                    return null;

                // ElementBindingExpressionSyntax does not carry the receiver expression; the receiver lives on
                // ConditionalAccessExpressionSyntax.Expression.
                return new ElementAccessContext(
                    conditional.Expression,
                    binding.ArgumentList,
                    binding);
            }
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

    private static bool IsWithinArgumentList(BracketedArgumentListSyntax argumentList, int offset)
    {
        var start = argumentList.OpenBracketToken.Span.Start;
        var end = argumentList.CloseBracketToken.Span.End;
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

    private static int GetArgumentIndex(BracketedArgumentListSyntax argumentList, int offset)
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

    private static int GetActiveSignatureIndex(
        ImmutableArray<IPropertySymbol> indexers,
        IPropertySymbol? selectedIndexer,
        int argumentIndex)
    {
        if (selectedIndexer is not null)
        {
            for (var i = 0; i < indexers.Length; i++)
            {
                if (SymbolEqualityComparer.Default.Equals(indexers[i], selectedIndexer))
                    return i;
            }
        }

        var typedArgumentCount = argumentIndex + 1;
        var bestIndex = 0;
        var bestScore = int.MaxValue;

        for (var i = 0; i < indexers.Length; i++)
        {
            var indexer = indexers[i];
            var required = indexer.Parameters.Count(parameter => !parameter.IsOptional);
            var total = indexer.Parameters.Length;
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

    private static int GetActiveParameterIndex(
        IPropertySymbol indexer,
        BracketedArgumentListSyntax argumentList,
        int offset,
        int fallbackArgumentIndex)
    {
        if (indexer.Parameters.Length == 0)
            return 0;

        foreach (var argument in argumentList.Arguments)
        {
            if (offset < argument.FullSpan.Start || offset > argument.FullSpan.End)
                continue;

            var name = argument.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrWhiteSpace(name))
                break;

            for (var i = 0; i < indexer.Parameters.Length; i++)
            {
                if (string.Equals(indexer.Parameters[i].Name, name, StringComparison.Ordinal))
                    return i;
            }

            break;
        }

        return Math.Clamp(fallbackArgumentIndex, 0, indexer.Parameters.Length - 1);
    }

    private static ITypeSymbol? GetConditionalAccessLookupType(ITypeSymbol? type)
    {
        if (type is null)
            return null;

        type = type.UnwrapLiteralType() ?? type;
        if (TryGetOptionPayloadType(type, out var optionPayload))
            return optionPayload.GetPlainType();

        if (TryGetResultPayloadType(type, out var resultPayload))
            return resultPayload.GetPlainType();

        return type.GetPlainType();
    }

    private static bool TryGetOptionPayloadType(ITypeSymbol? type, out ITypeSymbol payload)
    {
        payload = null!;
        if (type is null)
            return false;

        type = type.UnwrapLiteralType() ?? type;
        if (type is INamedTypeSymbol named &&
            named.Arity == 1 &&
            string.Equals(named.Name, "Option", StringComparison.Ordinal))
        {
            payload = named.TypeArguments[0];
            return true;
        }

        return false;
    }

    private static bool TryGetResultPayloadType(ITypeSymbol? type, out ITypeSymbol payload)
    {
        payload = null!;
        if (type is null)
            return false;

        type = type.UnwrapLiteralType() ?? type;
        if (type is INamedTypeSymbol named &&
            named.Arity == 2 &&
            string.Equals(named.Name, "Result", StringComparison.Ordinal))
        {
            payload = named.TypeArguments[0];
            return true;
        }

        return false;
    }

    private sealed record ElementAccessContext(
        ExpressionSyntax ReceiverExpression,
        BracketedArgumentListSyntax ArgumentList,
        SyntaxNode SymbolInfoNode);
}
