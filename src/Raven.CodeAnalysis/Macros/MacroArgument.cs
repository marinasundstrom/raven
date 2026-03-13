using System;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroArgument
{
    private readonly Lazy<TypedConstant> _constant;

    public MacroArgument(ArgumentSyntax syntax, SemanticModel semanticModel)
    {
        Syntax = syntax ?? throw new ArgumentNullException(nameof(syntax));
        ArgumentNullException.ThrowIfNull(semanticModel);
        Name = syntax.NameColon?.Name.Identifier.ValueText;
        Expression = syntax.Expression;
        _constant = new Lazy<TypedConstant>(() => TryCreateConstant(Expression, semanticModel.Compilation, out var constant)
            ? constant
            : TypedConstant.CreateError(type: null));
    }

    public ArgumentSyntax Syntax { get; }

    public string? Name { get; }

    public bool IsNamed => Name is not null;

    public ExpressionSyntax Expression { get; }

    public TypedConstant Constant => _constant.Value;

    public object? Value => Constant.Value;

    public ITypeSymbol? Type => Constant.Type;

    public TypedConstantKind ValueKind => Constant.Kind;

    public bool HasValue => ValueKind != TypedConstantKind.Error;

    private static bool TryCreateConstant(ExpressionSyntax expression, Compilation compilation, out TypedConstant constant)
    {
        if (ConstantValueEvaluator.TryEvaluate(expression, out var value))
        {
            constant = value is null
                ? TypedConstant.CreateNull(type: null)
                : TypedConstant.CreatePrimitive(GetPrimitiveType(compilation, value), value);
            return true;
        }

        constant = TypedConstant.CreateError(type: null);
        return false;
    }

    private static ITypeSymbol? GetPrimitiveType(Compilation compilation, object value)
    {
        return value switch
        {
            bool => compilation.GetSpecialType(SpecialType.System_Boolean),
            char => compilation.GetSpecialType(SpecialType.System_Char),
            sbyte => compilation.GetSpecialType(SpecialType.System_SByte),
            byte => compilation.GetSpecialType(SpecialType.System_Byte),
            short => compilation.GetSpecialType(SpecialType.System_Int16),
            ushort => compilation.GetSpecialType(SpecialType.System_UInt16),
            int => compilation.GetSpecialType(SpecialType.System_Int32),
            uint => compilation.GetSpecialType(SpecialType.System_UInt32),
            long => compilation.GetSpecialType(SpecialType.System_Int64),
            ulong => compilation.GetSpecialType(SpecialType.System_UInt64),
            float => compilation.GetSpecialType(SpecialType.System_Single),
            double => compilation.GetSpecialType(SpecialType.System_Double),
            decimal => compilation.GetSpecialType(SpecialType.System_Decimal),
            string => compilation.GetSpecialType(SpecialType.System_String),
            DateTime => compilation.GetSpecialType(SpecialType.System_DateTime),
            _ => compilation.GetSpecialType(SpecialType.System_Object)
        };
    }
}
