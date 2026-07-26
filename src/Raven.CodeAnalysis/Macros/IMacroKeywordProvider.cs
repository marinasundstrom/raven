using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

public interface IMacroKeywordProvider
{
    ImmutableArray<MacroKeyword> Keywords { get; }
}
