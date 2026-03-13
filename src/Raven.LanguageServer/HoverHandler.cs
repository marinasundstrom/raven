using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

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

            var macroHover = TryBuildMacroExpansionHover(sourceText, semanticModel, root, offset);
            if (macroHover is not null)
                return macroHover;

            var literalHover = TryBuildLiteralHover(sourceText, semanticModel, root, offset);
            if (literalHover is not null)
                return literalHover;

            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            if (resolution is null)
            {
                var patternHover = TryBuildPatternDeclarationHover(sourceText, semanticModel, root, offset);
                return patternHover;
            }

            var symbol = resolution.Value.Symbol;
            var signature = BuildSignatureForHover(symbol, resolution.Value.Node, semanticModel, root, offset);
            var containing = BuildContainingDisplay(symbol, semanticModel);
            var documentation = symbol.GetDocumentationComment();
            var functionCaptures = semanticModel.GetCapturedVariables(symbol);
            if (functionCaptures.IsDefaultOrEmpty)
                functionCaptures = semanticModel.GetCapturedVariables(resolution.Value.Node);
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

        var hoverText = string.Join(
            "\n\n",
            $"```raven\n{display.PreviewText}\n```",
            $"Macro `#[{display.MacroName}]` expansion preview.",
            "Use `Show macro expansion` to inspect the full expansion.");

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

            if (element.DotDotToken.Kind != SyntaxKind.None)
                return rightType;

            if (rightType is IArrayTypeSymbol arrayType)
                return arrayType.ElementType;

            if (rightType is INamedTypeSymbol named && named.TypeArguments.Length >= 1)
                return named.TypeArguments[0];
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
            var constructorName = containingType?.Name ?? constructor.Name;
            var typeParams = containingType is not null && !containingType.TypeParameters.IsDefaultOrEmpty
                ? $"<{string.Join(", ", containingType.TypeParameters.Select(static tp => tp.Name))}>"
                : string.Empty;
            var parameters = FormatParameters(constructor.Parameters, plainTypeFormat);
            var accessibilityPrefix = GetNonPublicAccessibilityPrefix(constructor);
            return $"{accessibilityPrefix}{constructorName}{typeParams}({parameters})";
        }

        if (symbol is IMethodSymbol method)
        {
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

        if (symbol is IDiscriminatedUnionCaseSymbol unionCase)
        {
            var parameters = FormatParameters(unionCase.ConstructorParameters, plainTypeFormat);
            return $"{unionCase.Name}({parameters})";
        }

        if (symbol is ITypeSymbol typeSymbol)
        {
            var declarationTypeFormat = CreatePlainTypeFormat();

            if (typeSymbol is INamedTypeSymbol delegateType &&
                delegateType.TypeKind == TypeKind.Delegate)
            {
                return delegateType.ToDisplayString(declarationTypeFormat);
            }

            var typeFormat = declarationTypeFormat.WithKindOptions(SymbolDisplayKindOptions.IncludeTypeKeyword);
            var text = FormatType(typeSymbol, typeFormat);

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

    private static string BuildSignatureForHover(
        ISymbol symbol,
        SyntaxNode contextNode,
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset)
    {
        if (TryBuildDeclaredTypeHoverSignatureOverride(symbol, semanticModel, root, offset, out var declaredTypeSignature))
            return declaredTypeSignature;

        var signature = BuildSignature(symbol, contextNode, semanticModel);

        if (!TryBuildReceiverErrorSignatureOverride(symbol, semanticModel, root, offset, out var overridden))
            return signature;

        return overridden;
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
            var memberSymbol = semanticModel.GetSymbolInfo(memberAccess.Name).Symbol
                ?? semanticModel.GetSymbolInfo(memberAccess).Symbol;
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
        var receiverSymbol = semanticModel.GetSymbolInfo(parentAccess.Expression).Symbol;
        if (receiverSymbol is ITypeSymbol or INamespaceSymbol)
            return false;

        return true;
    }

    private static string? BuildContainingDisplay(ISymbol symbol, SemanticModel semanticModel)
    {
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

    private static string BuildKindDisplay(ISymbol symbol)
    {
        if (symbol is IParameterSymbol parameterSymbol &&
            IsPromotedPrimaryConstructorParameter(parameterSymbol))
        {
            return "Property";
        }

        if (symbol is IMethodSymbol method && IsFunctionStatementSymbol(method))
            return "Function";

        if (symbol is IMethodSymbol { MethodKind: MethodKind.LambdaMethod })
            return "Function expression";

        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor })
            return "Constructor";

        return symbol.Kind.ToString();
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

        if (semanticModel.GetOperation(functionExpression) is ILambdaOperation lambdaOperation)
        {
            var operationParameterIndex = GetLambdaParameterIndex(functionExpression, parameterSyntax.Identifier.ValueText);
            if (operationParameterIndex >= 0 && operationParameterIndex < lambdaOperation.Parameters.Length)
            {
                var operationParameterType = lambdaOperation.Parameters[operationParameterIndex].Type;
                if (operationParameterType is not null && !operationParameterType.ContainsErrorType())
                {
                    inferredType = operationParameterType;
                    return true;
                }
            }
        }

        var functionType = semanticModel.GetTypeInfo(functionExpression).ConvertedType
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
            var typeSymbol = semanticModel.GetSymbolInfo(typeSyntax).Symbol;
            resolvedType = typeSymbol switch
            {
                ITypeSymbol resolved => resolved,
                IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasedType } => aliasedType,
                _ => resolvedType
            };
        }

        if (resolvedType is null || resolvedType.TypeKind == TypeKind.Error)
            return false;

        inferredType = resolvedType;
        return true;
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

        var functionInfo = semanticModel.GetSymbolInfo(functionExpression);
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
            if (ReferenceEquals(current, argument))
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

        var invocationInfo = semanticModel.GetSymbolInfo(invocation);
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
            ReferenceEquals(assignmentExpression.Right, functionExpression) &&
            assignmentExpression.Left is ExpressionSyntax assignmentExpressionLeft)
        {
            targetExpression = assignmentExpressionLeft;
        }
        else if (functionExpression.Parent is AssignmentStatementSyntax assignmentStatement &&
                 ReferenceEquals(assignmentStatement.Right, functionExpression) &&
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
                ? semanticModel.GetSymbolInfo(targetMemberAccess.Name).Symbol ?? semanticModel.GetSymbolInfo(targetExpression).Symbol
                : semanticModel.GetSymbolInfo(targetExpression).Symbol;

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
        var parameters = functionExpression switch
        {
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.ParameterList.Parameters,
            _ => default
        };

        if (parameters.Count == 0)
            return -1;

        for (var i = 0; i < parameters.Count; i++)
        {
            if (string.Equals(parameters[i].Identifier.ValueText, parameterName, StringComparison.Ordinal))
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
                     ReferenceEquals(memberAccess.Expression, identifier) &&
                     string.Equals(identifier.Identifier.ValueText, symbolName, StringComparison.Ordinal)
                => identifier,
            MemberAccessExpressionSyntax { Expression: IdentifierNameSyntax identifier }
                when string.Equals(identifier.Identifier.ValueText, symbolName, StringComparison.Ordinal)
                => identifier,
            _ => contextNode.AncestorsAndSelf()
                .OfType<IdentifierNameSyntax>()
                .FirstOrDefault(identifier =>
                    identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                    ReferenceEquals(memberAccess.Expression, identifier) &&
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

        var accessedMember = semanticModel.GetSymbolInfo(receiverMemberAccess.Name).Symbol;
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
                var parameterType = parameter.Type.ToDisplayString(format);
                return $"{paramsPrefix}{parameter.Name}: {parameterType}";
            }));
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
