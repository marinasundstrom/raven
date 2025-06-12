using Microsoft.CodeAnalysis.CSharp;

public static class HelperExtensions
{
    public static string FixIdentifier(string name)
    {
        name = name.ToCamelCase();

        var x = SyntaxFacts.IsKeywordKind(SyntaxFacts.GetKeywordKind(name));
        if (x)
        {
            return $"@{name}";
        }
        return name;
    }
}