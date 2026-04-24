using System.Collections.Concurrent;
using System.Diagnostics;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;
using VersionStamp = Raven.CodeAnalysis.VersionStamp;

namespace Raven.LanguageServer;

internal sealed class DocumentSymbolHandler : IDocumentSymbolHandler
{
    private const int MaxCachedDocumentSymbolEntries = 256;
    private const double SlowDocumentSymbolsThresholdMs = 100;

    private readonly DocumentStore _documents;
    private readonly ILogger<DocumentSymbolHandler> _logger;
    private readonly ConcurrentDictionary<DocumentSymbolCacheKey, SymbolInformationOrDocumentSymbol[]> _cache = new();

    public DocumentSymbolHandler(DocumentStore documents, ILogger<DocumentSymbolHandler> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    public DocumentSymbolRegistrationOptions GetRegistrationOptions(DocumentSymbolCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven")
        };

    public void SetCapability(DocumentSymbolCapability capability)
    {
    }

    public async Task<SymbolInformationOrDocumentSymbolContainer?> Handle(DocumentSymbolParams request, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();
        double analysisContextMs = 0;
        double rootMs = 0;
        double symbolBuildMs = 0;
        var cacheHit = false;
        var symbolCount = 0;

        try
        {
            var stageStopwatch = Stopwatch.StartNew();
            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            analysisContextMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (context is null)
                return new SymbolInformationOrDocumentSymbolContainer();

            var document = context.Value.Document;
            var syntaxTree = context.Value.SyntaxTree;
            var text = context.Value.SourceText;

            var cacheKey = new DocumentSymbolCacheKey(request.TextDocument.Uri.ToString(), document.Version);
            if (_cache.TryGetValue(cacheKey, out var cachedSymbols))
            {
                cacheHit = true;
                symbolCount = cachedSymbols.Length;
                return new SymbolInformationOrDocumentSymbolContainer(cachedSymbols);
            }

            var root = syntaxTree.GetRoot(cancellationToken);
            rootMs = stopwatch.Elapsed.TotalMilliseconds - analysisContextMs;

            var symbols = BuildMemberSymbols(root.Members, text)
                .Select(symbol => (SymbolInformationOrDocumentSymbol)symbol)
                .ToArray();
            symbolBuildMs = stopwatch.Elapsed.TotalMilliseconds - analysisContextMs - rootMs;
            symbolCount = symbols.Length;
            CacheSymbols(cacheKey, symbols);

            stopwatch.Stop();
            if (stopwatch.Elapsed.TotalMilliseconds >= SlowDocumentSymbolsThresholdMs)
            {
                _logger.LogInformation(
                    "Slow document symbols for {Uri}: total={TotalMs:F1}ms analysisContext={AnalysisContextMs:F1}ms root={RootMs:F1}ms build={BuildMs:F1}ms count={Count} cacheHit={CacheHit}.",
                    request.TextDocument.Uri,
                    stopwatch.Elapsed.TotalMilliseconds,
                    analysisContextMs,
                    rootMs,
                    symbolBuildMs,
                    symbols.Length,
                    cacheHit);
            }

            return new SymbolInformationOrDocumentSymbolContainer(symbols);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return new SymbolInformationOrDocumentSymbolContainer();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Document symbols request failed for {Uri}.", request.TextDocument.Uri);
            return new SymbolInformationOrDocumentSymbolContainer();
        }
        finally
        {
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "documentSymbols",
                request.TextDocument.Uri,
                null,
                stopwatch.Elapsed.TotalMilliseconds,
                cacheHit: cacheHit,
                resultCount: symbolCount,
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", analysisContextMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("root", rootMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("buildSymbols", symbolBuildMs)
                ]);
        }
    }

    private void CacheSymbols(DocumentSymbolCacheKey cacheKey, SymbolInformationOrDocumentSymbol[] symbols)
    {
        if (_cache.Count >= MaxCachedDocumentSymbolEntries)
            _cache.Clear();

        _cache[cacheKey] = symbols;
    }

    private static IEnumerable<DocumentSymbol> BuildMemberSymbols(SyntaxList<MemberDeclarationSyntax> members, SourceText text)
    {
        List<GlobalStatementSyntax>? topLevelStatements = null;

        foreach (var member in members)
        {
            if (member is GlobalStatementSyntax globalStatement &&
                globalStatement.Statement is not FunctionStatementSyntax)
            {
                topLevelStatements ??= [];
                topLevelStatements.Add(globalStatement);
                continue;
            }

            if (TryCreateSymbol(member, text, out var symbol))
                yield return symbol;
        }

        if (topLevelStatements is { Count: > 0 })
            yield return CreateTopLevelCodeSymbol(topLevelStatements, text);
    }

    private static bool TryCreateSymbol(MemberDeclarationSyntax member, SourceText text, out DocumentSymbol symbol)
    {
        switch (member)
        {
            case BaseNamespaceDeclarationSyntax namespaceDeclaration:
                {
                    var children = BuildMemberSymbols(namespaceDeclaration.Members, text).ToArray();
                    symbol = CreateSymbol(
                        namespaceDeclaration.Name.ToString(),
                        SymbolKind.Namespace,
                        namespaceDeclaration.Span,
                        namespaceDeclaration.Name.Span,
                        text,
                        children);
                    return true;
                }
            case GlobalStatementSyntax { Statement: FunctionStatementSyntax functionStatement }:
                symbol = CreateFunctionStatementSymbol(functionStatement, text);
                return true;
            case ClassDeclarationSyntax classDeclaration:
                symbol = CreateTypeSymbol(classDeclaration, SymbolKind.Class, text);
                return true;
            case StructDeclarationSyntax structDeclaration:
                symbol = CreateTypeSymbol(structDeclaration, SymbolKind.Struct, text);
                return true;
            case InterfaceDeclarationSyntax interfaceDeclaration:
                symbol = CreateTypeSymbol(interfaceDeclaration, SymbolKind.Interface, text);
                return true;
            case RecordDeclarationSyntax recordDeclaration:
                symbol = CreateTypeSymbol(recordDeclaration, SymbolKind.Struct, text);
                return true;
            case UnionDeclarationSyntax unionDeclaration:
                {
                    var unionChildren = BuildUnionChildSymbols(unionDeclaration, text).ToArray();
                    symbol = CreateSymbol(
                        unionDeclaration.Identifier.Text,
                        SymbolKind.Enum,
                        unionDeclaration.Span,
                        unionDeclaration.Identifier.Span,
                        text,
                        unionChildren);
                    return true;
                }
            case EnumDeclarationSyntax enumDeclaration:
                {
                    var enumChildren = enumDeclaration.Members
                        .Select(enumMember => CreateSymbol(
                            enumMember.Identifier.Text,
                            SymbolKind.EnumMember,
                            enumMember.Span,
                            enumMember.Identifier.Span,
                            text))
                        .ToArray();
                    symbol = CreateSymbol(
                        enumDeclaration.Identifier.Text,
                        SymbolKind.Enum,
                        enumDeclaration.Span,
                        enumDeclaration.Identifier.Span,
                        text,
                        enumChildren);
                    return true;
                }
            case MethodDeclarationSyntax methodDeclaration:
                symbol = CreateSymbol(
                    methodDeclaration.Identifier.Text,
                    SymbolKind.Method,
                    methodDeclaration.Span,
                    methodDeclaration.Identifier.Span,
                    text,
                    BuildNestedFunctionSymbols(GetCallableBodyRoots(methodDeclaration), text).ToArray());
                return true;
            case ConstructorDeclarationSyntax constructorDeclaration:
                symbol = CreateSymbol(
                    constructorDeclaration.InitKeyword.Text,
                    SymbolKind.Constructor,
                    constructorDeclaration.Span,
                    constructorDeclaration.InitKeyword.Span,
                    text,
                    BuildNestedFunctionSymbols(GetCallableBodyRoots(constructorDeclaration), text).ToArray());
                return true;
            case ParameterlessConstructorDeclarationSyntax parameterlessConstructorDeclaration:
                symbol = CreateSymbol(
                    parameterlessConstructorDeclaration.InitKeyword.Text,
                    SymbolKind.Constructor,
                    parameterlessConstructorDeclaration.Span,
                    parameterlessConstructorDeclaration.InitKeyword.Span,
                    text,
                    BuildNestedFunctionSymbols(GetCallableBodyRoots(parameterlessConstructorDeclaration), text).ToArray());
                return true;
            case DelegateDeclarationSyntax delegateDeclaration:
                symbol = CreateSymbol(
                    delegateDeclaration.Identifier.Text,
                    SymbolKind.Function,
                    delegateDeclaration.Span,
                    delegateDeclaration.Identifier.Span,
                    text);
                return true;
            case PropertyDeclarationSyntax propertyDeclaration:
                symbol = CreateSymbol(
                    propertyDeclaration.Identifier.Text,
                    SymbolKind.Property,
                    propertyDeclaration.Span,
                    propertyDeclaration.Identifier.Span,
                    text);
                return true;
            case IndexerDeclarationSyntax indexerDeclaration:
                symbol = CreateSymbol(
                    indexerDeclaration.Identifier.Text,
                    SymbolKind.Property,
                    indexerDeclaration.Span,
                    indexerDeclaration.Identifier.Span,
                    text);
                return true;
            case FieldDeclarationSyntax fieldDeclaration:
                {
                    var fieldName = string.Join(", ", fieldDeclaration.Declaration.Declarators.Select(x => x.Identifier.Text));
                    var selectionSpan = fieldDeclaration.Declaration.Declarators.FirstOrDefault()!.Identifier.Span;
                    if (selectionSpan.Length == 0)
                        selectionSpan = fieldDeclaration.Span;

                    symbol = CreateSymbol(fieldName, SymbolKind.Field, fieldDeclaration.Span, selectionSpan, text);
                    return true;
                }
            case EventDeclarationSyntax eventDeclaration:
                symbol = CreateSymbol(
                    eventDeclaration.Identifier.Text,
                    SymbolKind.Event,
                    eventDeclaration.Span,
                    eventDeclaration.Identifier.Span,
                    text);
                return true;
            default:
                symbol = null!;
                return false;
        }
    }

    private static DocumentSymbol CreateTopLevelCodeSymbol(
        IReadOnlyList<GlobalStatementSyntax> statements,
        SourceText text)
    {
        var first = statements[0];
        var last = statements[^1];
        var span = TextSpan.FromBounds(first.Span.Start, last.Span.End);
        var selectionSpan = first.Statement.Span.Length > 0
            ? first.Statement.Span
            : first.Span;

        return CreateSymbol(
            "<top-level code>",
            SymbolKind.Function,
            span,
            selectionSpan,
            text,
            BuildNestedFunctionSymbols(statements.Select(static statement => statement.Statement), text).ToArray());
    }

    private static DocumentSymbol CreateFunctionStatementSymbol(FunctionStatementSyntax declaration, SourceText text)
        => CreateSymbol(
            declaration.Identifier.Text,
            SymbolKind.Function,
            declaration.Span,
            declaration.Identifier.Span,
            text,
            BuildNestedFunctionSymbols(GetCallableBodyRoots(declaration), text).ToArray());

    private readonly record struct DocumentSymbolCacheKey(string Uri, VersionStamp Version);

    private static DocumentSymbol CreateTypeSymbol(TypeDeclarationSyntax declaration, SymbolKind kind, SourceText text)
    {
        var children = BuildTypeChildSymbols(declaration, text).ToArray();
        return CreateSymbol(
            declaration.Identifier.Text,
            kind,
            declaration.Span,
            declaration.Identifier.Span,
            text,
            children);
    }

    private static IEnumerable<DocumentSymbol> BuildTypeChildSymbols(TypeDeclarationSyntax declaration, SourceText text)
    {
        foreach (var symbol in BuildPromotedPrimaryConstructorPropertySymbols(declaration, text))
            yield return symbol;

        foreach (var symbol in BuildMemberSymbols(declaration.Members, text))
            yield return symbol;
    }

    private static IEnumerable<DocumentSymbol> BuildUnionChildSymbols(UnionDeclarationSyntax declaration, SourceText text)
    {
        foreach (var member in declaration.Members)
        {
            if (member is CaseDeclarationSyntax unionCase)
            {
                yield return CreateSymbol(
                    unionCase.Identifier.Text,
                    SymbolKind.EnumMember,
                    unionCase.Span,
                    unionCase.Identifier.Span,
                    text);
                continue;
            }

            if (TryCreateSymbol(member, text, out var symbol))
                yield return symbol;
        }
    }

    private static IEnumerable<DocumentSymbol> BuildPromotedPrimaryConstructorPropertySymbols(TypeDeclarationSyntax declaration, SourceText text)
    {
        var parameterList = declaration.ParameterList;
        if (parameterList is null)
            yield break;

        var isRecord = declaration is RecordDeclarationSyntax;
        foreach (var parameter in parameterList.Parameters)
        {
            if (!IsPromotedPropertyParameter(parameter, isRecord))
                continue;

            var outlineSpan = GetPromotedPropertyOutlineSpan(parameter);
            yield return CreateSymbol(
                parameter.Identifier.Text,
                SymbolKind.Property,
                outlineSpan,
                outlineSpan,
                text);
        }
    }

    private static bool IsPromotedPropertyParameter(ParameterSyntax parameter, bool isRecord)
    {
        var bindingKeyword = parameter.BindingKeyword;
        if (bindingKeyword.Kind != SyntaxKind.None)
            return bindingKeyword.Kind is SyntaxKind.ValKeyword or SyntaxKind.VarKeyword;

        return isRecord;
    }

    private static TextSpan GetPromotedPropertyOutlineSpan(ParameterSyntax parameter)
    {
        var start = parameter.AccessibilityKeyword.Kind != SyntaxKind.None
            ? parameter.AccessibilityKeyword.Span.Start
            : parameter.BindingKeyword.Kind != SyntaxKind.None
                ? parameter.BindingKeyword.Span.Start
                : parameter.Identifier.Span.Start;

        var end = parameter.Identifier.Span.End;
        return new TextSpan(start, end - start);
    }

    private static IEnumerable<DocumentSymbol> BuildNestedFunctionSymbols(IEnumerable<SyntaxNode> roots, SourceText text)
    {
        foreach (var root in roots)
        {
            foreach (var child in root.ChildNodes())
            {
                foreach (var symbol in BuildNestedFunctionSymbols(child, text))
                    yield return symbol;
            }
        }
    }

    private static IEnumerable<DocumentSymbol> BuildNestedFunctionSymbols(SyntaxNode node, SourceText text)
    {
        if (node is FunctionStatementSyntax functionStatement)
        {
            yield return CreateFunctionStatementSymbol(functionStatement, text);
            yield break;
        }

        foreach (var child in node.ChildNodes())
        {
            foreach (var symbol in BuildNestedFunctionSymbols(child, text))
                yield return symbol;
        }
    }

    private static IEnumerable<SyntaxNode> GetCallableBodyRoots(FunctionStatementSyntax declaration)
        => GetCallableBodyRoots(declaration.Body, declaration.ExpressionBody);

    private static IEnumerable<SyntaxNode> GetCallableBodyRoots(MethodDeclarationSyntax declaration)
        => GetCallableBodyRoots(declaration.Body, declaration.ExpressionBody);

    private static IEnumerable<SyntaxNode> GetCallableBodyRoots(ConstructorDeclarationSyntax declaration)
        => GetCallableBodyRoots(declaration.Body, declaration.ExpressionBody);

    private static IEnumerable<SyntaxNode> GetCallableBodyRoots(ParameterlessConstructorDeclarationSyntax declaration)
        => GetCallableBodyRoots(declaration.Body, declaration.ExpressionBody);

    private static IEnumerable<SyntaxNode> GetCallableBodyRoots(BlockStatementSyntax? body, ArrowExpressionClauseSyntax? expressionBody)
    {
        if (body is not null)
            yield return body;

        if (expressionBody is not null)
            yield return expressionBody;
    }

    private static DocumentSymbol CreateSymbol(
        string name,
        SymbolKind kind,
        TextSpan span,
        TextSpan selectionSpan,
        SourceText text,
        params DocumentSymbol[] children)
    {
        var resolvedName = string.IsNullOrWhiteSpace(name) ? "<unnamed>" : name;
        var range = PositionHelper.ToRange(text, span);
        var selectionRange = PositionHelper.ToRange(text, selectionSpan);

        return new DocumentSymbol
        {
            Name = resolvedName,
            Kind = kind,
            Range = range,
            SelectionRange = selectionRange,
            Children = children.Length > 0 ? new Container<DocumentSymbol>(children) : null
        };
    }
}
