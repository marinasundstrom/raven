namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies the optional presentation category of a token in a macro body.
/// </summary>
public enum MacroTokenClassification
{
    Default = 0,
    Keyword = 1,
    ReservedWord = 2,
    Identifier = 3,
    Literal = 4,
    Operator = 5,
    Punctuation = 6,
    Comment = 7,
}
