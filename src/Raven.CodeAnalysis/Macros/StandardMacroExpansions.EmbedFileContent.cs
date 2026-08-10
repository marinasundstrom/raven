using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Implements low-level expansion mechanics used by the standard Raven macro library.
/// </summary>
/// <remarks>
/// This is a transitional compiler-side implementation. As the Raven macro
/// API gains the required file-observation and syntax-construction capabilities,
/// this behavior should move wholly or partly into Raven.Macros.
/// </remarks>
public static partial class StandardMacroExpansions
{
    private const string FileNotFoundCode = "EMBEDFILE001";
    private const string FileReadFailedCode = "EMBEDFILE002";

    /// <summary>
    /// Embeds a UTF-8 text file as a Raven string literal.
    /// </summary>
    public static InvocableMacroExpansionResult ExpandEmbedFileContent(
        InvocableMacroContext context,
        string path)
    {
        var file = context.ReadFile(path);
        if (file.Status == MacroFileReadStatus.Missing)
        {
            return WithFileDependencies(
                EmbedFileError(
                    context,
                    $"The file '{file.Path}' does not exist.",
                    FileNotFoundCode),
                context);
        }

        if (file.Status == MacroFileReadStatus.Failed)
        {
            return WithFileDependencies(
                EmbedFileError(
                    context,
                    $"The file '{file.Path}' could not be read: {file.Error}",
                    FileReadFailedCode),
                context);
        }

        var content = file.Content!;
        var expression = SyntaxFactory.LiteralExpression(
            SyntaxKind.StringLiteralExpression,
            SyntaxFactory.Literal(FormatStringLiteral(content), content));
        return WithFileDependencies(
            InvocableMacroExpansionResult.FromExpression(expression),
            context);
    }

    private static string FormatStringLiteral(string value)
    {
        var builder = new StringBuilder(value.Length + 2);
        builder.Append('"');
        foreach (var character in value)
        {
            switch (character)
            {
                case '\\':
                    builder.Append(@"\\");
                    break;
                case '"':
                    builder.Append("\\\"");
                    break;
                case '\0':
                    builder.Append(@"\0");
                    break;
                case '\a':
                    builder.Append(@"\a");
                    break;
                case '\b':
                    builder.Append(@"\b");
                    break;
                case '\f':
                    builder.Append(@"\f");
                    break;
                case '\n':
                    builder.Append(@"\n");
                    break;
                case '\r':
                    builder.Append(@"\r");
                    break;
                case '\t':
                    builder.Append(@"\t");
                    break;
                case '\v':
                    builder.Append(@"\v");
                    break;
                default:
                    if (char.IsControl(character))
                        builder.Append(@"\u").Append(((int)character).ToString("X4"));
                    else
                        builder.Append(character);
                    break;
            }
        }

        return builder.Append('"').ToString();
    }

    private static InvocableMacroExpansionResult EmbedFileError(
        InvocableMacroContext context,
        string message,
        string code)
        => InvocableMacroExpansionResult.FromDiagnostic(
            context.CreateDiagnostic(
                message,
                syntax: context.Arguments.FirstOrDefault()?.Expression,
                code: code));

    private static InvocableMacroExpansionResult WithFileDependencies(
        InvocableMacroExpansionResult result,
        InvocableMacroContext context)
    {
        result.FileDependencies = context.GetFileDependencies();
        return result;
    }
}
