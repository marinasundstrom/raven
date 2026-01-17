using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public readonly struct MatchExhaustivenessInfo
{
    public MatchExhaustivenessInfo(bool isExhaustive, ImmutableArray<string> missingCases, bool hasCatchAll)
    {
        IsExhaustive = isExhaustive;
        MissingCases = missingCases;
        HasCatchAll = hasCatchAll;
    }

    public bool IsExhaustive { get; }

    public ImmutableArray<string> MissingCases { get; }

    public bool HasCatchAll { get; }
}

public readonly struct MatchExhaustivenessOptions
{
    public MatchExhaustivenessOptions(bool ignoreCatchAllPatterns)
    {
        IgnoreCatchAllPatterns = ignoreCatchAllPatterns;
    }

    public bool IgnoreCatchAllPatterns { get; }
}
