using System.Text;
using System.Linq;

internal static class TokenGenerator
{
    private static string Escape(string text)
    {
        return text
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\r", "\\r")
            .Replace("\n", "\\n")
            .Replace("\t", "\\t");
    }

    public static string GenerateInternalFactory(IEnumerable<TokenKindModel> tokens)
    {
        var sb = new StringBuilder();
        sb.AppendLine("namespace Raven.CodeAnalysis.Syntax.InternalSyntax;");
        sb.AppendLine();
        sb.AppendLine("internal static partial class SyntaxFactory");
        sb.AppendLine("{");
        foreach (var t in tokens.Where(t => t.Text != null && !t.IsTrivia))
        {
            var v = t.Text ?? t.Value;
            var literal = v!.Length == 0 ? "string.Empty" : $"\"{Escape(v)}\"";
            sb.AppendLine($"    public static readonly SyntaxToken {t.Name} = new SyntaxToken(SyntaxKind.{t.Name}, {literal});");
        }
        sb.AppendLine("}");
        return sb.ToString();
    }

    public static string GenerateRedFactory(IEnumerable<TokenKindModel> tokens)
    {
        var sb = new StringBuilder();
        sb.AppendLine("namespace Raven.CodeAnalysis.Syntax;");
        sb.AppendLine();
        sb.AppendLine("public static partial class SyntaxFactory");
        sb.AppendLine("{");
        foreach (var t in tokens.Where(t => t.Text != null && !t.IsTrivia))
        {
            sb.AppendLine($"    public static readonly SyntaxToken {t.Name} = (SyntaxToken)InternalSyntax.SyntaxFactory.{t.Name};");
        }
        sb.AppendLine("}");
        return sb.ToString();
    }

    public static string GenerateSyntaxFacts(IEnumerable<TokenKindModel> tokens)
    {
        var sb = new StringBuilder();
        sb.AppendLine("using System.Collections.Generic;");
        sb.AppendLine();
        sb.AppendLine("namespace Raven.CodeAnalysis.Syntax;");
        sb.AppendLine();
        sb.AppendLine("public static partial class SyntaxFacts");
        sb.AppendLine("{");
        var keywords = tokens.Where(t => t.Text != null && t.Text.All(char.IsLetter));
        sb.AppendLine("    private static readonly IDictionary<string, SyntaxKind> _keywordStrings = new Dictionary<string, SyntaxKind>");
        sb.AppendLine("    {");
        foreach (var t in keywords)
        {
            sb.AppendLine($"        {{ \"{t.Text}\", SyntaxKind.{t.Name} }},");
        }
        sb.AppendLine("    };");
        sb.AppendLine("    private static readonly HashSet<SyntaxKind> _keywordKinds = [.. _keywordStrings.Values];");
        sb.AppendLine("    public static bool IsKeywordKind(SyntaxKind kind) => _keywordKinds.Contains(kind);");
        sb.AppendLine("    public static bool TryParseKeyword(string text, out SyntaxKind kind) => _keywordStrings.TryGetValue(text, out kind);");

        sb.AppendLine("    private static readonly IDictionary<string, SyntaxKind> _reservedWordStrings = new Dictionary<string, SyntaxKind>");
        sb.AppendLine("    {");
        foreach (var t in keywords.Where(t => t.IsReservedWord))
        {
            sb.AppendLine($"        {{ \"{t.Text}\", SyntaxKind.{t.Name} }},");
        }
        sb.AppendLine("    };");
        sb.AppendLine("    private static readonly HashSet<SyntaxKind> _reservedWordKinds = [.. _reservedWordStrings.Values];");
        sb.AppendLine("    public static bool IsReservedWordKind(SyntaxKind kind) => _reservedWordKinds.Contains(kind);");
        sb.AppendLine("    public static bool TryParseReservedWord(string text, out SyntaxKind kind) => _reservedWordStrings.TryGetValue(text, out kind);");

        var binaryOps = tokens.Where(t => t.IsBinaryOperator).GroupBy(t => t.Precedence).OrderByDescending(g => g.Key);
        sb.AppendLine("    public static bool TryResolveOperatorPrecedence(SyntaxKind kind, out int precedence)");
        sb.AppendLine("    {");
        sb.AppendLine("        switch (kind)");
        sb.AppendLine("        {");
        foreach (var group in binaryOps)
        {
            foreach (var t in group)
                sb.AppendLine($"            case SyntaxKind.{t.Name}:");
            sb.AppendLine($"                precedence = {group.Key};");
            sb.AppendLine("                return true;");
            sb.AppendLine();
        }
        sb.AppendLine("            default:");
        sb.AppendLine("                precedence = -1;");
        sb.AppendLine("                return false;");
        sb.AppendLine("        }");
        sb.AppendLine("    }");

        var unaryOps = tokens.Where(t => t.IsUnaryOperator);
        sb.AppendLine("    public static bool IsUnaryOperatorToken(SyntaxKind kind) => kind switch");
        sb.AppendLine("    {");
        foreach (var t in unaryOps)
        {
            sb.AppendLine($"        SyntaxKind.{t.Name} => true,");
        }
        sb.AppendLine("        _ => false,");
        sb.AppendLine("    };");

        sb.AppendLine("    public static string? GetSyntaxTokenText(this SyntaxKind kind)");
        sb.AppendLine("    {");
        sb.AppendLine("        return kind switch");
        sb.AppendLine("        {");
        foreach (var t in tokens.Where(t => t.Text != null))
        {
            var literal = t.Text!.Length == 0 ? "string.Empty" : $"\"{Escape(t.Text)}\"";
            sb.AppendLine($"            SyntaxKind.{t.Name} => {literal},");
        }
        sb.AppendLine("            _ => null,");
        sb.AppendLine("        };");
        sb.AppendLine("    }");
        sb.AppendLine("}");
        return sb.ToString();
    }
}
