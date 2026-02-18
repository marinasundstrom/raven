using System;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Diagnostics;

public static class SuggestionsDiagnosticProperties
{
    public const string OriginalCodeKey = "raven.suggestion.originalCode";
    public const string RewrittenCodeKey = "raven.suggestion.rewrittenCode";

    public static ImmutableDictionary<string, string?> CreateRewriteSuggestion(string originalCode, string rewrittenCode)
    {
        return ImmutableDictionary<string, string?>.Empty
            .Add(OriginalCodeKey, originalCode)
            .Add(RewrittenCodeKey, rewrittenCode);
    }

    public static bool TryGetRewriteSuggestion(Diagnostic diagnostic, out string originalCode, out string rewrittenCode)
    {
        originalCode = string.Empty;
        rewrittenCode = string.Empty;

        if (!diagnostic.Properties.TryGetValue(OriginalCodeKey, out var original) || string.IsNullOrWhiteSpace(original))
            return false;

        if (!diagnostic.Properties.TryGetValue(RewrittenCodeKey, out var rewritten) || string.IsNullOrWhiteSpace(rewritten))
            return false;

        originalCode = original;
        rewrittenCode = rewritten;
        return true;
    }

    public static bool IsSuggestionModeEnabled(Compilation compilation)
    {
        ArgumentNullException.ThrowIfNull(compilation);
        return compilation.Options.EnableSuggestions;
    }
}
