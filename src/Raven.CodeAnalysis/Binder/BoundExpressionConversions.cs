using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class BoundExpressionConversions
{
    internal static Conversion Classify(
        Compilation compilation,
        BoundExpression expression,
        TypeSymbol destination,
        Binder? binder = null,
        bool includeUserDefined = true)
    {
        if (expression.Type is not { } source)
            return Conversion.None;

        var conversion = compilation.ClassifyConversion(source, destination, includeUserDefined);
        if (conversion.IsImplicit || source.UnwrapLiteralType().SpecialType != SpecialType.System_Int32)
            return conversion;

        // Constant conversions depend on the expression's value, not just its type.
        // Keep the public semantic type of an ordinary integer literal as int.
        var status = BoundNumericConstantEvaluator.Evaluate(expression,
            node => BlockBinder.IsCheckedContext(binder?.SemanticModel.GetSyntax(node), defaultValue: true),
            out var value);
        if (status != BoundNumericConstantEvaluator.Status.Constant || value is not int)
            return conversion;

        var constantConversion = compilation.ClassifyConversion(
            new LiteralTypeSymbol(source.UnwrapLiteralType(), value, compilation), destination, includeUserDefined: false);
        return constantConversion.IsImplicit ? constantConversion : conversion;
    }
}
