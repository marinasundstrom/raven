using System.Globalization;
using System.Security.Cryptography;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Implements low-level expansion mechanics used by the standard Raven macro library.
/// </summary>
/// <remarks>
/// This is a transitional compiler-side implementation. As the Raven macro
/// API gains the required constant-value and syntax-construction capabilities,
/// this behavior should move wholly or partly into Raven.Macros.
/// </remarks>
public static partial class StandardMacroExpansions
{
    private const string Sha256LiteralRequiredCode = "SHA256001";

    /// <summary>
    /// Computes the SHA-256 digest of a literal value during compilation.
    /// </summary>
    public static InvocableMacroExpansionResult ExpandSha256Digest(
        InvocableMacroContext context,
        ExpressionSyntax expression)
    {
        if (!ConstantValueEvaluator.TryEvaluate(expression, out var value) ||
            !TryGetCanonicalBytes(value, out var bytes))
        {
            return InvocableMacroExpansionResult.FromDiagnostic(
                context.CreateDiagnostic(
                    "The sha256Digest macro requires a literal string, character, Boolean, numeric, or null value.",
                    syntax: expression,
                    code: Sha256LiteralRequiredCode));
        }

        var digest = Convert.ToHexStringLower(SHA256.HashData(bytes));
        var expansion = SyntaxFactory.LiteralExpression(
            SyntaxKind.StringLiteralExpression,
            SyntaxFactory.Literal($"\"{digest}\"", digest));
        return InvocableMacroExpansionResult.FromExpression(expansion);
    }

    private static bool TryGetCanonicalBytes(object? value, out byte[] bytes)
    {
        string text;
        switch (value)
        {
            case null:
                bytes = [];
                return true;
            case string stringValue:
                text = stringValue;
                break;
            case char character:
                text = character.ToString();
                break;
            case bool boolean:
                text = boolean ? "true" : "false";
                break;
            case float single:
                text = single.ToString("R", CultureInfo.InvariantCulture);
                break;
            case double doubleValue:
                text = doubleValue.ToString("R", CultureInfo.InvariantCulture);
                break;
            case decimal decimalValue:
                text = decimalValue.ToString("G29", CultureInfo.InvariantCulture);
                break;
            case sbyte or byte or short or ushort or int or uint or long or ulong:
                text = ((IFormattable)value).ToString(format: null, CultureInfo.InvariantCulture);
                break;
            default:
                bytes = [];
                return false;
        }

        bytes = Encoding.UTF8.GetBytes(text);
        return true;
    }
}
