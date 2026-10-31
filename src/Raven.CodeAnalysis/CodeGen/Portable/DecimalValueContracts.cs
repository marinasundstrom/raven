namespace Raven.CodeAnalysis.CodeGen.Portable;

// Decimal is a library value type. Built-in syntax calls its declared contracts,
// just as CLI decimal operations call System.Decimal methods rather than opcodes.
internal static class DecimalValueContracts
{
    internal static BoundExpression? Rewrite(Compilation compilation, BoundExpression expression)
    {
        var owner = compilation.GetSpecialType(SpecialType.System_Decimal) as INamedTypeSymbol;
        if (owner is null) return null;
        IMethodSymbol? Method(string? name, ITypeSymbol result, params ITypeSymbol[] arguments) =>
            name is null ? null : owner.GetMembers().OfType<IMethodSymbol>().SingleOrDefault(m =>
                m.IsStatic && m.DeclaredAccessibility == Accessibility.Public && m.MetadataName == name &&
                m.Arity == 0 && m.Parameters.All(p => p.RefKind == RefKind.None) && SymbolEqualityComparer.Default.Equals(m.ReturnType, result) &&
                m.Parameters.Length == arguments.Length && m.Parameters.Select(p => p.Type).SequenceEqual(arguments, SymbolEqualityComparer.Default));
        BoundExpression? Constant(decimal value)
        {
            var bits = decimal.GetBits(value);
            var intType = compilation.GetSpecialType(SpecialType.System_Int32);
            var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);
            var byteType = compilation.GetSpecialType(SpecialType.System_Byte);
            var arguments = new ITypeSymbol[] { intType, intType, intType, boolType, byteType };
            var ctor = owner.Constructors.SingleOrDefault(m => !m.IsStatic && m.DeclaredAccessibility == Accessibility.Public &&
                m.Parameters.Select(p => p.Type).SequenceEqual(arguments, SymbolEqualityComparer.Default));
            return ctor is null ? null : new BoundObjectCreationExpression(ctor,
                [new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, bits[0], intType),
                 new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, bits[1], intType),
                 new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, bits[2], intType),
                 new BoundLiteralExpression(bits[3] < 0 ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral, bits[3] < 0, boolType),
                 new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, (byte)(bits[3] >> 16), byteType)]);
        }
        switch (expression)
        {
            case BoundLiteralExpression { Value: decimal value }:
                return Constant(value);
            case BoundLocalAccess { Local.IsConst: true } local when local.Local.ConstantValue is decimal value:
                return Constant(value);
            case BoundBinaryExpression { Operator.MethodSymbol: null } binary when
                binary.Left.Type.SpecialType == SpecialType.System_Decimal && binary.Right.Type.SpecialType == SpecialType.System_Decimal:
                var name = (binary.Operator.OperatorKind & ~BinaryOperatorKind.Checked) switch
                {
                    BinaryOperatorKind.Addition => "op_Addition",
                    BinaryOperatorKind.Subtraction => "op_Subtraction",
                    BinaryOperatorKind.Multiplication => "op_Multiply",
                    BinaryOperatorKind.Division => "op_Division",
                    BinaryOperatorKind.Modulo => "op_Modulus",
                    BinaryOperatorKind.Equality => "op_Equality",
                    BinaryOperatorKind.Inequality => "op_Inequality",
                    BinaryOperatorKind.GreaterThan => "op_GreaterThan",
                    BinaryOperatorKind.LessThan => "op_LessThan",
                    BinaryOperatorKind.GreaterThanOrEqual => "op_GreaterThanOrEqual",
                    BinaryOperatorKind.LessThanOrEqual => "op_LessThanOrEqual",
                    _ => null,
                };
                return Method(name, binary.Type, owner, owner) is { } method
                    ? new BoundInvocationExpression(method, [binary.Left, binary.Right]) : null;
            case BoundUnaryExpression unary when unary.Operand.Type.SpecialType == SpecialType.System_Decimal:
                if (unary.Operator.OperatorKind == BoundUnaryOperatorKind.UnaryPlus) return unary.Operand;
                return unary.Operator.OperatorKind is BoundUnaryOperatorKind.UnaryMinus or BoundUnaryOperatorKind.CheckedUnaryMinus &&
                    Method("op_UnaryNegation", owner, owner) is { } negate
                    ? new BoundInvocationExpression(negate, [unary.Operand]) : null;
            case BoundConversionExpression conversion when !conversion.IsUserDefined && conversion.Conversion.IsNumeric &&
                conversion.Type.SpecialType == SpecialType.System_Decimal:
                var source = conversion.Expression.Type.SpecialType;
                var parameter = compilation.GetSpecialType(source switch
                {
                    SpecialType.System_SByte or SpecialType.System_Byte or SpecialType.System_Int16 or SpecialType.System_UInt16 => SpecialType.System_Int32,
                    _ => source,
                });
                if (Method("op_Implicit", owner, parameter) is not { } convert) return null;
                var operand = SymbolEqualityComparer.Default.Equals(parameter, conversion.Expression.Type) ? conversion.Expression :
                    new BoundConversionExpression(conversion.Expression, parameter,
                        compilation.ClassifyConversion(conversion.Expression.Type, parameter, includeUserDefined: false));
                return new BoundInvocationExpression(convert, [operand]);
        }
        return null;
    }
}
