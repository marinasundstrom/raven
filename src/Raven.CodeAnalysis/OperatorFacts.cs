using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal enum OperatorArity
{
    Unary,
    Binary,
}

internal readonly record struct UserDefinedOperatorInfo(string MetadataName, OperatorArity Arity);

internal static class OperatorFacts
{
    public static string GetDisplayText(SyntaxKind operatorToken)
        => SyntaxFacts.GetSyntaxTokenText(operatorToken) ?? operatorToken.ToString();

    public static string GetConversionOperatorDisplayText(SyntaxKind conversionKindKeyword)
    {
        var keywordText = SyntaxFacts.GetSyntaxTokenText(conversionKindKeyword) ?? conversionKindKeyword.ToString();
        return $"{keywordText} operator";
    }

    public static bool TryGetConversionOperatorMetadataName(SyntaxKind conversionKindKeyword, out string metadataName)
    {
        metadataName = conversionKindKeyword switch
        {
            SyntaxKind.ExplicitKeyword => "op_Explicit",
            SyntaxKind.ImplicitKeyword => "op_Implicit",
            _ => string.Empty,
        };

        return metadataName.Length > 0;
    }

    public static bool TryGetUserDefinedOperatorInfo(
        SyntaxKind operatorToken,
        int parameterCount,
        out UserDefinedOperatorInfo info)
    {
        info = default;

        var arity = parameterCount switch
        {
            1 => OperatorArity.Unary,
            2 => OperatorArity.Binary,
            _ => (OperatorArity?)null,
        };

        if (arity is null)
            return false;

        var metadataName = arity == OperatorArity.Unary
            ? GetUnaryMetadataName(operatorToken)
            : GetBinaryMetadataName(operatorToken);

        if (metadataName is null)
            return false;

        info = new UserDefinedOperatorInfo(metadataName, arity.Value);
        return true;
    }

    public static string GetExpectedParameterCountDescription(SyntaxKind operatorToken)
    {
        return operatorToken switch
        {
            SyntaxKind.PlusToken or SyntaxKind.MinusToken => "1 or 2",
            SyntaxKind.PlusPlusToken or SyntaxKind.MinusMinusToken or SyntaxKind.ExclamationToken or SyntaxKind.TildeToken => "1",
            _ => "2",
        };
    }

    private static string? GetUnaryMetadataName(SyntaxKind operatorToken) => operatorToken switch
    {
        SyntaxKind.PlusToken => "op_UnaryPlus",
        SyntaxKind.MinusToken => "op_UnaryNegation",
        SyntaxKind.PlusPlusToken => "op_Increment",
        SyntaxKind.MinusMinusToken => "op_Decrement",
        SyntaxKind.ExclamationToken => "op_LogicalNot",
        SyntaxKind.TildeToken => "op_OnesComplement",
        _ => null,
    };

    private static string? GetBinaryMetadataName(SyntaxKind operatorToken) => operatorToken switch
    {
        SyntaxKind.PlusToken => "op_Addition",
        SyntaxKind.MinusToken => "op_Subtraction",
        SyntaxKind.StarToken => "op_Multiply",
        SyntaxKind.SlashToken => "op_Division",
        SyntaxKind.PercentToken => "op_Modulus",
        SyntaxKind.CaretToken => "op_ExclusiveOr",
        SyntaxKind.AmpersandToken => "op_BitwiseAnd",
        SyntaxKind.AmpersandAmpersandToken or SyntaxKind.AndToken => "op_LogicalAnd",
        SyntaxKind.BarToken => "op_BitwiseOr",
        SyntaxKind.BarBarToken or SyntaxKind.OrToken => "op_LogicalOr",
        SyntaxKind.LessThanLessThanToken => "op_LeftShift",
        SyntaxKind.GreaterThanGreaterThanToken => "op_RightShift",
        SyntaxKind.EqualsEqualsToken => "op_Equality",
        SyntaxKind.NotEqualsToken => "op_Inequality",
        SyntaxKind.LessThanToken => "op_LessThan",
        SyntaxKind.LessThanOrEqualsToken => "op_LessThanOrEqual",
        SyntaxKind.GreaterThanToken => "op_GreaterThan",
        SyntaxKind.GreaterThanOrEqualsToken => "op_GreaterThanOrEqual",
        _ => null,
    };
}
