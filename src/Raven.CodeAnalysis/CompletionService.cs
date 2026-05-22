using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides completion items for a given position in a syntax tree.
/// </summary>
public class CompletionService
{
    internal readonly record struct CompletionComputation(
        ImmutableArray<CompletionItem> Items,
        double SemanticModelMs,
        double ProviderMs,
        bool UsedFallback,
        string? FailureType);

    internal static readonly ImmutableArray<string> BasicKeywords =
    [
        "if", "else", "while", "for", "return", "let", "var", "const", "new", "true", "false", "null"
    ];

    /// <summary>
    /// Gets the completion items available at the specified position.
    /// </summary>
    /// <param name="compilation">The compilation that contains the syntax tree.</param>
    /// <param name="syntaxTree">The syntax tree being queried.</param>
    /// <param name="position">The position within the syntax tree.</param>
    /// <returns>A sequence of completion items applicable at the position.</returns>
    public IEnumerable<CompletionItem> GetCompletions(Compilation compilation, SyntaxTree syntaxTree, int position)
    {
        // Completion requests typically originate from the caret position which
        // lies *after* the character that triggered completion.  Because
        // FindToken looks for the token that contains the provided position and
        // token spans are end-exclusive, passing the caret position directly
        // results in the token to the right being returned.  Adjust the search
        // position to ensure the token to the left of the caret is used.
        var searchPosition = Math.Max(0, position - 1);
        var sourceText = syntaxTree.GetText();
        var content = sourceText.ToString();
        var isWhitespaceOnlyLinePosition = IsWhitespaceOnlyLinePosition(content, position);
        while (searchPosition > 0 &&
               searchPosition < content.Length &&
               char.IsWhiteSpace(content[searchPosition]))
            searchPosition--;
        var token = syntaxTree.GetRoot().FindToken(searchPosition);
        try
        {
            var semanticModel = compilation.GetSemanticModel(syntaxTree);

            return GetCompletions(token, semanticModel, position, isWhitespaceOnlyLinePosition);
        }
        catch
        {
            // Keep completion usable in lightweight/editor scenarios where semantic setup
            // may fail (for example, missing metadata references).
            return GetBasicKeywordCompletions(token, position);
        }
    }

    public IEnumerable<CompletionItem> GetCompletions(SemanticModel semanticModel, int position)
    {
        var syntaxTree = semanticModel.SyntaxTree;
        var searchPosition = Math.Max(0, position - 1);
        var sourceText = syntaxTree.GetText();
        var content = sourceText.ToString();
        var isWhitespaceOnlyLinePosition = IsWhitespaceOnlyLinePosition(content, position);
        while (searchPosition > 0 &&
               searchPosition < content.Length &&
               char.IsWhiteSpace(content[searchPosition]))
        {
            searchPosition--;
        }

        var token = syntaxTree.GetRoot().FindToken(searchPosition);
        try
        {
            return GetCompletions(token, semanticModel, position, isWhitespaceOnlyLinePosition);
        }
        catch
        {
            return GetBasicKeywordCompletions(token, position);
        }
    }

    private static bool IsWhitespaceOnlyLinePosition(string content, int position)
    {
        if ((uint)position > (uint)content.Length)
            return false;

        for (var i = position - 1; i >= 0; i--)
        {
            var ch = content[i];
            if (ch is ' ' or '\t')
                continue;

            if (ch is '\r' or '\n')
                return true;

            return false;
        }

        return true;
    }

    /// <summary>
    /// Gets the completion items available at the specified position asynchronously.
    /// </summary>
    /// <param name="compilation">The compilation that contains the syntax tree.</param>
    /// <param name="syntaxTree">The syntax tree being queried.</param>
    /// <param name="position">The position within the syntax tree.</param>
    /// <param name="cancellationToken">Token used to cancel the operation.</param>
    /// <returns>A materialized set of completion items applicable at the position.</returns>
    public async Task<ImmutableArray<CompletionItem>> GetCompletionsAsync(
        Compilation compilation,
        SyntaxTree syntaxTree,
        int position,
        CancellationToken cancellationToken = default)
    {
        return (await GetCompletionsWithMetricsAsync(compilation, syntaxTree, position, cancellationToken).ConfigureAwait(false)).Items;
    }

    public async Task<ImmutableArray<CompletionItem>> GetCompletionsAsync(
        SemanticModel semanticModel,
        int position,
        CancellationToken cancellationToken = default)
    {
        return (await GetCompletionsWithMetricsAsync(semanticModel, position, cancellationToken).ConfigureAwait(false)).Items;
    }

    internal async Task<CompletionComputation> GetCompletionsWithMetricsAsync(
        Compilation compilation,
        SyntaxTree syntaxTree,
        int position,
        CancellationToken cancellationToken = default)
    {
        return await Task.Run(
                () => GetCompletionsWithMetrics(compilation, syntaxTree, position),
                cancellationToken)
            .ConfigureAwait(false);
    }

    internal async Task<CompletionComputation> GetCompletionsWithMetricsAsync(
        SemanticModel semanticModel,
        int position,
        CancellationToken cancellationToken = default)
    {
        return await Task.Run(
                () => GetCompletionsWithMetrics(semanticModel, position),
                cancellationToken)
            .ConfigureAwait(false);
    }

    internal bool TryGetImportDirectiveCompletionsWithMetrics(
        Compilation compilation,
        SyntaxTree syntaxTree,
        int position,
        out CompletionComputation completion)
    {
        var providerStopwatch = System.Diagnostics.Stopwatch.StartNew();
        if (!TryGetImportDirectiveCompletions(compilation, syntaxTree, position, out var items))
        {
            providerStopwatch.Stop();
            completion = default;
            return false;
        }

        providerStopwatch.Stop();
        completion = new CompletionComputation(
            items,
            SemanticModelMs: 0,
            ProviderMs: providerStopwatch.Elapsed.TotalMilliseconds,
            UsedFallback: false,
            FailureType: null);
        return true;
    }

    internal CompletionComputation GetCompletionsWithMetrics(Compilation compilation, SyntaxTree syntaxTree, int position)
    {
        var semanticModelMs = 0d;
        var semanticModelStopwatch = System.Diagnostics.Stopwatch.StartNew();
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        semanticModelStopwatch.Stop();
        semanticModelMs = semanticModelStopwatch.Elapsed.TotalMilliseconds;

        return GetCompletionsWithMetrics(semanticModel, position, semanticModelMs);
    }

    internal CompletionComputation GetCompletionsWithMetrics(SemanticModel semanticModel, int position)
        => GetCompletionsWithMetrics(semanticModel, position, semanticModelMs: 0d);

    private CompletionComputation GetCompletionsWithMetrics(
        SemanticModel semanticModel,
        int position,
        double semanticModelMs)
    {
        var searchPosition = Math.Max(0, position - 1);
        var syntaxTree = semanticModel.SyntaxTree;
        var sourceText = syntaxTree.GetText();
        var content = sourceText.ToString();
        var isWhitespaceOnlyLinePosition = IsWhitespaceOnlyLinePosition(content, position);
        while (searchPosition > 0 &&
               searchPosition < content.Length &&
               char.IsWhiteSpace(content[searchPosition]))
        {
            searchPosition--;
        }

        var token = syntaxTree.GetRoot().FindToken(searchPosition);
        var providerMs = 0d;

        try
        {
            var providerStopwatch = System.Diagnostics.Stopwatch.StartNew();
            var items = GetCompletions(token, semanticModel, position, isWhitespaceOnlyLinePosition)
                .ToImmutableArray();
            providerStopwatch.Stop();
            providerMs = providerStopwatch.Elapsed.TotalMilliseconds;

            return new CompletionComputation(items, semanticModelMs, providerMs, UsedFallback: false, FailureType: null);
        }
        catch (Exception ex)
        {
            var fallbackStopwatch = System.Diagnostics.Stopwatch.StartNew();
            var items = GetBasicKeywordCompletions(token, position).ToImmutableArray();
            fallbackStopwatch.Stop();
            providerMs += fallbackStopwatch.Elapsed.TotalMilliseconds;
            return new CompletionComputation(items, semanticModelMs, providerMs, UsedFallback: true, ex.GetType().Name);
        }
    }

    private static bool TryGetImportDirectiveCompletions(
        Compilation compilation,
        SyntaxTree syntaxTree,
        int position,
        out ImmutableArray<CompletionItem> completions)
    {
        completions = default;

        var sourceText = syntaxTree.GetText();
        var content = sourceText.ToString();
        if (!IsImportLineCompletionContext(content, position))
            return false;

        var searchPosition = Math.Max(0, position - 1);
        while (searchPosition > 0 &&
               searchPosition < content.Length &&
               char.IsWhiteSpace(content[searchPosition]))
        {
            searchPosition--;
        }

        var token = syntaxTree.GetRoot().FindToken(searchPosition);
        if (!TryGetImportCompletionTarget(token, position, out var receiverName, out var prefix, out var replacementSpan))
            return false;

        var receiver = TryResolveImportNamespaceOrType(compilation, receiverName);
        if (receiver is null)
        {
            completions = ImmutableArray<CompletionItem>.Empty;
            return true;
        }

        completions = BuildImportDirectiveMemberCompletions(compilation, receiver, prefix, replacementSpan);
        return true;
    }

    private static bool IsImportLineCompletionContext(string content, int position)
    {
        if ((uint)position > (uint)content.Length)
            return false;

        var lineStart = position;
        while (lineStart > 0 && content[lineStart - 1] is not '\r' and not '\n')
            lineStart--;

        var lineEnd = position;
        while (lineEnd < content.Length && content[lineEnd] is not '\r' and not '\n')
            lineEnd++;

        var line = content.AsSpan(lineStart, lineEnd - lineStart).TrimStart();
        return line.StartsWith("import ", StringComparison.Ordinal);
    }

    private static bool TryGetImportCompletionTarget(
        SyntaxToken token,
        int position,
        out NameSyntax receiverName,
        out string prefix,
        out TextSpan replacementSpan)
    {
        if (token.GetAncestor<QualifiedNameSyntax>() is { Right: SimpleNameSyntax simple } qualified &&
            IsAtImportCompletionName(qualified, simple.Identifier, position))
        {
            receiverName = qualified.Left;
            prefix = simple.Identifier.ValueText;
            replacementSpan = GetImportCompletionReplacementSpan(simple.Identifier, position);
            return true;
        }

        if (token.GetAncestor<MemberAccessExpressionSyntax>() is { Expression: NameSyntax memberReceiver } memberAccess &&
            position >= memberAccess.OperatorToken.Span.End)
        {
            var hasNameAtCaret = !memberAccess.Name.Identifier.IsMissing &&
                position > memberAccess.Name.Identifier.Span.Start;
            receiverName = memberReceiver;
            prefix = hasNameAtCaret
                ? memberAccess.Name.Identifier.ValueText
                : string.Empty;
            replacementSpan = hasNameAtCaret
                ? memberAccess.Name.Identifier.Span
                : new TextSpan(position, 0);
            return true;
        }

        receiverName = null!;
        prefix = string.Empty;
        replacementSpan = default;
        return false;
    }

    private static TextSpan GetImportCompletionReplacementSpan(SyntaxToken token, int position)
        => token.IsMissing
            ? new TextSpan(position, 0)
            : token.Span;

    private static bool IsAtImportCompletionName(QualifiedNameSyntax qualified, SyntaxToken token, int position)
        => position >= token.Position ||
           (token.IsMissing && position >= qualified.DotToken.Span.End);

    private static INamespaceOrTypeSymbol? TryResolveImportNamespaceOrType(Compilation compilation, NameSyntax name)
    {
        return name switch
        {
            IdentifierNameSyntax identifier => TryResolveRootNamespaceOrType(compilation, identifier),
            QualifiedNameSyntax qualified => TryResolveImportQualifiedNamespaceOrType(compilation, qualified),
            GenericNameSyntax generic => TryResolveRootNamespaceOrType(compilation, generic),
            _ => null
        };
    }

    private static INamespaceOrTypeSymbol? TryResolveImportQualifiedNamespaceOrType(Compilation compilation, QualifiedNameSyntax qualified)
    {
        var left = TryResolveImportNamespaceOrType(compilation, qualified.Left);
        if (left is null)
            return null;

        return qualified.Right switch
        {
            IdentifierNameSyntax identifier => LookupQualifiedMember(compilation, left, identifier.Identifier.ValueText, arity: 0),
            GenericNameSyntax generic => LookupQualifiedMember(compilation, left, generic.Identifier.ValueText, generic.TypeArgumentList.Arguments.Count),
            _ => null
        };
    }

    private static INamespaceOrTypeSymbol? TryResolveRootNamespaceOrType(Compilation compilation, IdentifierNameSyntax identifier)
        => LookupRootMember(compilation, identifier.Identifier.ValueText, arity: 0);

    private static INamespaceOrTypeSymbol? TryResolveRootNamespaceOrType(Compilation compilation, GenericNameSyntax generic)
        => LookupRootMember(compilation, generic.Identifier.ValueText, generic.TypeArgumentList.Arguments.Count);

    private static INamespaceOrTypeSymbol? LookupRootMember(
        Compilation compilation,
        string name,
        int arity)
    {
        if (string.IsNullOrWhiteSpace(name))
            return null;

        compilation.EnsureSetup();

        List<INamespaceSymbol>? namespaces = null;
        if (LookupQualifiedMember(compilation, compilation.SourceGlobalNamespace, name, arity) is { } sourceMember)
        {
            if (sourceMember is not INamespaceSymbol sourceNamespace)
                return sourceMember;

            namespaces = [sourceNamespace];
        }

        foreach (var referencedAssembly in compilation.ReferencedAssemblySymbols)
        {
            var member = LookupQualifiedMember(compilation, referencedAssembly.GlobalNamespace, name, arity);
            if (member is null)
                continue;

            if (member is not INamespaceSymbol namespaceMember)
            {
                if (namespaces is null)
                    return member;

                continue;
            }

            namespaces ??= [];
            namespaces.Add(namespaceMember);
        }

        return namespaces?.Count switch
        {
            null or 0 => null,
            1 => namespaces[0],
            _ => new MergedNamespaceSymbol(namespaces, null!)
        };
    }

    private static INamespaceOrTypeSymbol? LookupQualifiedMember(
        Compilation compilation,
        INamespaceOrTypeSymbol? container,
        string name,
        int arity)
    {
        if (container is null || string.IsNullOrWhiteSpace(name))
            return null;

        if (container is INamespaceSymbol namespaceSymbol)
        {
            var directMember = (INamespaceOrTypeSymbol?)namespaceSymbol.LookupNamespace(name)
                ?? SelectTypeMember(namespaceSymbol.GetMembers(name).OfType<INamedTypeSymbol>(), arity)
                ?? (INamespaceOrTypeSymbol?)namespaceSymbol.LookupType(name);
            if (directMember is not null)
                return directMember;

            foreach (var member in GetNamespaceCompletionMembers(compilation, namespaceSymbol).OfType<INamespaceOrTypeSymbol>())
            {
                if (!string.Equals(member.Name, name, StringComparison.Ordinal))
                    continue;

                if (member is INamedTypeSymbol namedType)
                    return SelectTypeMember(new[] { namedType }, arity);

                return member;
            }

            return null;
        }

        if (container is ITypeSymbol typeSymbol)
            return SelectTypeMember(typeSymbol.GetMembers(name).OfType<INamedTypeSymbol>(), arity);

        return null;
    }

    private static INamedTypeSymbol? SelectTypeMember(IEnumerable<INamedTypeSymbol> candidates, int arity)
    {
        foreach (var candidate in candidates)
        {
            if (candidate.Arity == arity)
                return candidate;
        }

        return null;
    }

    private static ImmutableArray<CompletionItem> BuildImportDirectiveMemberCompletions(
        Compilation compilation,
        INamespaceOrTypeSymbol nsOrType,
        string prefix,
        TextSpan replacementSpan)
    {
        var completions = ImmutableArray.CreateBuilder<CompletionItem>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        if (NameMatchesPrefix("*", prefix))
        {
            seen.Add("__import_wildcard");
            completions.Add(new CompletionItem(
                DisplayText: "*",
                InsertionText: "*",
                ReplacementSpan: replacementSpan,
                Description: "Import all accessible members"));
        }

        var members = nsOrType is INamespaceSymbol importNamespace
            ? GetNamespaceCompletionMembers(compilation, importNamespace)
            : nsOrType.GetMembers()
                .Where(IsAccessibleForImportCompletion)
                .Where(IsImportDirectiveTypeMemberCompletion);

        foreach (var member in members.Where(member => NameMatchesPrefix(member.Name, prefix)))
        {
            var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
            var cursorOffset = member is ITypeSymbol ? insertText.Length : (int?)null;

            if (!seen.Add(dedupKey))
                continue;

            completions.Add(new CompletionItem(
                DisplayText: displayText,
                InsertionText: insertText,
                ReplacementSpan: replacementSpan,
                CursorOffset: cursorOffset,
                Description: SafeToDisplayString(member),
                Symbol: member));
        }

        return completions.ToImmutable();
    }

    private static IEnumerable<ISymbol> GetNamespaceCompletionMembers(Compilation compilation, INamespaceSymbol namespaceSymbol)
        => namespaceSymbol.GetMembers()
            .Concat(compilation.GetNamespaceMembers(namespaceSymbol, compilation.Options.AllowNamespaceMemberImports))
            .Where(IsAccessibleForImportCompletion);

    private static bool IsAccessibleForImportCompletion(ISymbol symbol)
        => symbol.DeclaredAccessibility is Accessibility.NotApplicable
            or Accessibility.Public
            or Accessibility.Internal
            or Accessibility.ProtectedOrInternal;

    private static bool IsImportDirectiveTypeMemberCompletion(ISymbol member)
        => member is INamespaceOrTypeSymbol or IFieldSymbol { IsConst: true };

    private static string SafeToDisplayString(ISymbol symbol)
    {
        try
        {
            return symbol.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat);
        }
        catch
        {
            return symbol.Name;
        }
    }

    private static (string displayText, string insertionText, string dedupKey) CreateCompletionParts(ISymbol symbol)
    {
        var escapedName = EscapeIdentifierForInsertion(symbol.Name);
        var insertionText = symbol is IMethodSymbol
            ? escapedName + "()"
            : escapedName;
        var displayText = symbol is IUnionCaseTypeSymbol unionCase
            ? ((INamedTypeSymbol)unionCase).FormatUnionCaseForDiagnostic()
            : escapedName;
        var dedupKey = symbol is IUnionCaseTypeSymbol
            ? displayText
            : symbol.Name;

        return (displayText, insertionText, dedupKey);
    }

    private static string EscapeIdentifierForInsertion(string identifier)
    {
        if (string.IsNullOrEmpty(identifier))
            return identifier;

        if (identifier[0] == '@')
            return identifier;

        return SyntaxFacts.TryParseKeyword(identifier, out _)
            ? "@" + identifier
            : identifier;
    }

    private static bool NameMatchesPrefix(string symbolName, string prefix)
    {
        if (string.IsNullOrEmpty(prefix))
            return true;

        if (symbolName.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
            return true;

        return symbolName.Length > 1 &&
            symbolName[0] == '@' &&
            symbolName.AsSpan(1).StartsWith(prefix.AsSpan(), StringComparison.OrdinalIgnoreCase);
    }

    private static IEnumerable<CompletionItem> GetCompletions(
        SyntaxToken token,
        SemanticModel semanticModel,
        int position,
        bool forceInsertionAtCaret)
    {
        return CompletionProvider.GetCompletions(
            token,
            semanticModel,
            position,
            forceInsertionAtCaret: forceInsertionAtCaret);
    }

    internal static IEnumerable<CompletionItem> GetBasicKeywordCompletions(SyntaxToken token, int position)
    {
        var prefix = token.IsKind(SyntaxKind.IdentifierToken)
            ? token.ValueText
            : token.Parent is IdentifierNameSyntax { Identifier.IsMissing: false } identifierName
                ? identifierName.Identifier.ValueText
                : string.Empty;

        var replacementSpan = token.IsKind(SyntaxKind.IdentifierToken)
            ? token.Span
            : token.Parent is IdentifierNameSyntax { Identifier.IsMissing: false } identifier
                ? identifier.Identifier.Span
                : new TextSpan(position, 0);

        return BasicKeywords
            .Where(k => string.IsNullOrEmpty(prefix) || k.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
            .Select(keyword => new CompletionItem(
                DisplayText: keyword,
                InsertionText: keyword,
                ReplacementSpan: replacementSpan));
    }
}
