using Microsoft.CodeAnalysis.CSharp;

public static class HelperExtensions
{
    public static string FixIdentifier(string name)
    {
        name = name.ToCamelCase();

        var keywordKind = SyntaxFacts.GetKeywordKind(name);
        if (SyntaxFacts.IsKeywordKind(keywordKind))
        {
            return $"@{name}";
        }
        return name;
    }
}