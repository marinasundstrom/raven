namespace Raven.CodeAnalysis;

internal static class BoundReceiverFacts
{
    public static bool IsBaseReceiver(BoundExpression? receiver)
    {
        while (true)
        {
            switch (receiver)
            {
                case BoundBaseExpression:
                    return true;
                case BoundConversionExpression conversion when conversion.Conversion.IsIdentity:
                    receiver = conversion.Expression;
                    continue;
                case BoundRequiredResultExpression required:
                    receiver = required.Operand;
                    continue;
                default:
                    return false;
            }
        }
    }
}
