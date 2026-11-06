using System;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

public readonly record struct MatchExhaustivenessInfo
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

    public bool Equals(MatchExhaustivenessInfo other)
        => IsExhaustive == other.IsExhaustive && HasCatchAll == other.HasCatchAll &&
            (MissingCases == other.MissingCases || !MissingCases.IsDefault && !other.MissingCases.IsDefault &&
                MissingCases.SequenceEqual(other.MissingCases, StringComparer.Ordinal));

    public override int GetHashCode()
    {
        var hash = new HashCode();
        hash.Add(IsExhaustive);
        hash.Add(HasCatchAll);
        if (!MissingCases.IsDefault)
        {
            foreach (var missing in MissingCases)
                hash.Add(missing, StringComparer.Ordinal);
        }
        return hash.ToHashCode();
    }
}

public readonly record struct MatchExhaustivenessOptions
{
    public MatchExhaustivenessOptions(bool ignoreCatchAllPatterns)
    {
        IgnoreCatchAllPatterns = ignoreCatchAllPatterns;
    }

    public bool IgnoreCatchAllPatterns { get; }
}
