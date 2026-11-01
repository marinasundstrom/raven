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

    private static string EscapeRaven(string text) => Escape(text).Replace("$", "\\$");

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
            sb.AppendLine($"    public static SyntaxToken {t.Name} => Token(SyntaxKind.{t.Name});");
        }
        sb.AppendLine("}");
        return sb.ToString();
    }

    public static string GenerateRavenFactory(IEnumerable<TokenKindModel> tokens, bool green)
    {
        var sb = new StringBuilder();
        sb.AppendLine("namespace Raven.CodeAnalysis.Syntax" + (green ? ".InternalSyntax" : ""));
        if (green)
            sb.AppendLine("alias SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind");
        sb.AppendLine((green ? "internal" : "public") + " static partial class SyntaxFactory {");
        foreach (var token in tokens.Where(t => t.Text != null && !t.IsTrivia))
        {
            if (green)
            {
                var literal = token.Text!.Length == 0 ? "string.Empty" : "\"" + EscapeRaven(token.Text!) + "\"";
                sb.AppendLine("    static val " + token.Name + ": SyntaxToken = SyntaxToken(SyntaxKind." + token.Name + ", " + literal + ")");
            }
            else
                sb.AppendLine("    static val " + token.Name + ": SyntaxToken => Token(SyntaxKind." + token.Name + ")");
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
    public static string GenerateRavenSyntaxFacts(IEnumerable<TokenKindModel> tokens)
    {
        var items = tokens.ToList();
        var sb = new StringBuilder();
        sb.AppendLine("// <auto-generated />");
        sb.AppendLine("namespace Raven.CodeAnalysis.Syntax");
        sb.AppendLine("import System.*");
        sb.AppendLine("import System.Collections.Generic.*");
        sb.AppendLine("import System.Runtime.CompilerServices.*");
        sb.AppendLine("public static partial class SyntaxFacts {");
        var keywords = items.Where(t => t.Text != null && t.Text.All(char.IsLetter)).ToList();
        void AppendLookup(string prefix, string method, IEnumerable<TokenKindModel> kinds)
        {
            sb.AppendLine($"    private static readonly field _{prefix}Strings: IDictionary<string, SyntaxKind> = Create{method}Lookup()");
            sb.AppendLine($"    private static readonly field _{prefix}Kinds: HashSet<SyntaxKind> = HashSet<SyntaxKind>(_{prefix}Strings.Values)");
            sb.AppendLine($"    private static func Create{method}Lookup() -> IDictionary<string, SyntaxKind> {{");
            sb.AppendLine("        let result = Dictionary<string, SyntaxKind>()");
            foreach (var token in kinds)
                sb.AppendLine($"        result.Add(\"{EscapeRaven(token.Text!)}\", SyntaxKind.{token.Name})");
            sb.AppendLine("        return result");
            sb.AppendLine("    }");
            sb.AppendLine($"    static func Is{method}Kind(kind: SyntaxKind) -> bool => _{prefix}Kinds.Contains(kind)");
            sb.AppendLine($"    static func TryParse{method}(text: string, out kind: SyntaxKind) -> bool => _{prefix}Strings.TryGetValue(text, out kind)");
        }
        AppendLookup("keyword", "Keyword", keywords);
        AppendLookup("reservedWord", "ReservedWord", keywords.Where(t => t.IsReservedWord));
        sb.AppendLine("    static func TryResolveOperatorPrecedence(kind: SyntaxKind, out precedence: int) -> bool {");
        sb.AppendLine("        precedence = kind match {");
        foreach (var token in items.Where(t => t.IsBinaryOperator))
            sb.AppendLine($"            SyntaxKind.{token.Name} => {token.Precedence}");
        sb.AppendLine("            _ => -1");
        sb.AppendLine("        }");
        sb.AppendLine("        return precedence != -1");
        sb.AppendLine("    }");
        sb.AppendLine("    static func IsUnaryOperatorToken(kind: SyntaxKind) -> bool => kind match {");
        foreach (var token in items.Where(t => t.IsUnaryOperator))
            sb.AppendLine($"        SyntaxKind.{token.Name} => true");
        sb.AppendLine("        _ => false");
        sb.AppendLine("    }");
        sb.AppendLine("    [ExtensionAttribute]");
        sb.AppendLine("    static func GetSyntaxTokenText(kind: SyntaxKind) -> string? => kind match {");
        foreach (var token in items.Where(t => t.Text != null))
            sb.AppendLine($"        SyntaxKind.{token.Name} => \"{EscapeRaven(token.Text!)}\"");
        sb.AppendLine("        _ => null");
        sb.AppendLine("    }");
        sb.AppendLine("}");
        return sb.ToString();
    }

}
