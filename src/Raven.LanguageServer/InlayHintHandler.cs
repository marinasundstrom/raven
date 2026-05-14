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
        var totalStopwatch = Stopwatch.StartNew();
        var stageStopwatch = Stopwatch.StartNew();
        var gateWaitMs = 0d;
        var analysisContextMs = 0d;
        var semanticModelMs = 0d;
        var collectMs = 0d;
        var resultCount = 0;
        var outcome = "Completed";

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
            var isLargeDocumentRequest = IsLargeDocumentRequest(sourceText, requestSpan);

            stageStopwatch.Restart();
            var hints = new List<InlayHint>();
            var collectionBudget = new InlayHintCollectionBudget(
                Stopwatch.StartNew(),
                cancellationToken,
                double.PositiveInfinity,
                includeTooltips: !isLargeDocumentRequest);
            AddLocalTypeHints(hints, semanticModel, root, sourceText, requestSpan, collectionBudget);
            AddPatternTypeHints(hints, semanticModel, root, sourceText, requestSpan, collectionBudget);
            AddForTargetTypeHints(hints, semanticModel, root, sourceText, requestSpan, collectionBudget);
            AddReturnTypeHints(hints, semanticModel, root, sourceText, requestSpan, collectionBudget);
            collectMs = stageStopwatch.Elapsed.TotalMilliseconds;
            resultCount = hints.Count;
            if (collectionBudget.IsExpired)
                outcome = "BudgetExpired";

            return new InlayHintContainer(hints);
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
                resultCount: resultCount,
                detail: $"{request.TextDocument.Uri} {request.Range.Start.Line}:{request.Range.Start.Character}-{request.Range.End.Line}:{request.Range.End.Character} outcome={outcome}",
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", analysisContextMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("semanticModel", semanticModelMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("collect", collectMs)
                ]);
        }
    }

    private static void AddLocalTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        InlayHintCollectionBudget budget)
    {
        foreach (var declarator in root.DescendantNodes().OfType<VariableDeclaratorSyntax>())
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

            if (semanticModel.GetDeclaredSymbol(declarator) is not ILocalSymbol local ||
                !TryFormatType(semanticModel, declarator, local.Type, out var typeDisplay))
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

    private static void AddForTargetTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        InlayHintCollectionBudget budget)
    {
        foreach (var forStatement in root.DescendantNodes().OfType<ForStatementSyntax>())
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
        foreach (var designation in root.DescendantNodes().OfType<SingleVariableDesignationSyntax>())
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

    private static void AddReturnTypeHints(
        List<InlayHint> hints,
        SemanticModel semanticModel,
        SyntaxNode root,
        SourceText sourceText,
        TextSpan requestSpan,
        InlayHintCollectionBudget budget)
    {
        foreach (var method in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
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
                budget);
        }

        foreach (var function in root.DescendantNodes().OfType<FunctionStatementSyntax>())
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
                budget);
        }

        foreach (var functionExpression in root.DescendantNodes().OfType<FunctionExpressionSyntax>())
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
        InlayHintCollectionBudget budget)
    {
        if (HasExplicitReturnType(returnType) || body is null)
            return;

        var insertionPosition = parameterList.Span.End;
        if (!ContainsPosition(requestSpan, insertionPosition))
            return;

        if (budget.ShouldStop())
            return;

        if (!TryGetInferredReturnType(semanticModel, body, out var inferredReturnType) ||
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

        var name = CanUseSimpleName(binder, type)
            ? type.Name
            : type.ToDisplayStringKeywordAware(QualifiedSourceTypeDisplayFormat.WithGenericsOptions(SymbolDisplayGenericsOptions.None));

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

        foreach (var candidate in GetVisibleTypeCandidates(binder, type.Name))
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

    private static IEnumerable<ITypeSymbol> GetVisibleTypeCandidates(Binder binder, string name)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);

        if (binder.LookupType(name) is { } lookupType &&
            seen.Add(GetTypeIdentity(lookupType)))
        {
            yield return lookupType;
        }

        foreach (var symbol in binder.LookupSymbols(name).OfType<ITypeSymbol>())
        {
            if (seen.Add(GetTypeIdentity(symbol)))
                yield return symbol;
        }
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

    private static bool TryGetInferredReturnType(SemanticModel semanticModel, SyntaxNode body, out ITypeSymbol? inferredReturnType)
    {
        inferredReturnType = ReturnTypeCollector.Infer(semanticModel.GetBoundNode(body));
        if (IsInlayHintReturnType(inferredReturnType))
            return true;

        return TryInferReturnTypeFromReturnSyntax(semanticModel, body, out inferredReturnType) &&
            IsInlayHintReturnType(inferredReturnType);
    }

    private static bool IsInlayHintReturnType(ITypeSymbol? type)
        => type is not null &&
           type.SpecialType is not (SpecialType.System_Unit or SpecialType.System_Void);

    private static bool TryInferReturnTypeFromReturnSyntax(
        SemanticModel semanticModel,
        SyntaxNode body,
        out ITypeSymbol? inferredReturnType)
    {
        var types = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var returnStatement in DescendantNodesExcludingNestedExecutableScopes(body).OfType<ReturnStatementSyntax>())
        {
            if (returnStatement.Expression is not { } expression)
                continue;

            var typeInfo = semanticModel.GetTypeInfo(expression);
            var type = typeInfo.Type ?? typeInfo.ConvertedType;
            if (type is null ||
                type.TypeKind == TypeKind.Error ||
                type.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
            {
                continue;
            }

            types.Add(TypeSymbolNormalization.NormalizeForInference(type));
        }

        inferredReturnType = types.Count switch
        {
            0 => null,
            1 => types.First(),
            _ => TypeSymbolNormalization.NormalizeUnion(types)
        };

        return inferredReturnType is not null;
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

        if (semanticModel.GetDeclaredSymbol(functionExpression) is IMethodSymbol method &&
            method.ReturnType.SpecialType is not (SpecialType.System_Unit or SpecialType.System_Void))
        {
            returnType = method.ReturnType;
            return true;
        }

        if (semanticModel.TryGetContextualBoundFunctionExpression(functionExpression, out var contextualFunction) &&
            IsInlayHintReturnType(contextualFunction.ReturnType))
        {
            returnType = contextualFunction.ReturnType;
            return true;
        }

        if (semanticModel.GetBoundNode(functionExpression) is BoundFunctionExpression boundFunction &&
            IsInlayHintReturnType(boundFunction.ReturnType))
        {
            returnType = boundFunction.ReturnType;
            return true;
        }

        return false;
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

    private static bool IsLargeDocumentRequest(SourceText sourceText, TextSpan requestSpan)
        => sourceText.Length > MaxUnboundedDocumentLength &&
           requestSpan.Length > MaxUnboundedDocumentLength;

    private readonly struct InlayHintCollectionBudget
    {
        private const double MinTooltipBudgetMilliseconds = 50;

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
