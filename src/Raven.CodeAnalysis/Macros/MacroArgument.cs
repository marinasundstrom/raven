using System;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed partial class MacroArgument
{
    private readonly Lazy<TypedConstant> _constant;
    private readonly Lazy<TypeSymbol?> _semanticType;

    internal MacroArgument(ArgumentSyntax syntax, MacroArgumentServices services)
        : this(syntax, syntax?.Expression, services)
    {
    }

    internal MacroArgument(ExpressionSyntax syntax, MacroArgumentServices services)
        : this(syntax, syntax, services)
    {
    }

    private MacroArgument(SyntaxNode? syntax, ExpressionSyntax? expression, MacroArgumentServices services)
    {
        Syntax = syntax ?? throw new ArgumentNullException(nameof(syntax));
        ArgumentNullException.ThrowIfNull(services);
        Name = (syntax as ArgumentSyntax)?.NameColon?.Name.Identifier.ValueText;
        Expression = expression ?? throw new ArgumentNullException(nameof(expression));
        Services = services;
        _constant = new Lazy<TypedConstant>(() => TryCreateConstant(Expression, services, out var constant)
            ? constant
            : TypedConstant.CreateError(type: null));
        _semanticType = new Lazy<TypeSymbol?>(() => services.GetArgumentType(Expression));
    }

    public SyntaxNode Syntax { get; }

    public string? Name { get; }

    public bool IsNamed => Name is not null;

    public ExpressionSyntax Expression { get; }

    public TypedConstant Constant => _constant.Value;

    public object? Value => Constant.Value;

    public TypeSymbol? Type => Constant.Type;

    /// <summary>Gets the ordinary bound type of the authored expression.</summary>
    public TypeSymbol? SemanticType => _semanticType.Value;

    internal MacroArgumentServices Services { get; }

    public TypedConstantKind ValueKind => Constant.Kind;

    public bool HasValue => ValueKind != TypedConstantKind.Error;

    private static bool TryCreateConstant(ExpressionSyntax expression, MacroArgumentServices services, out TypedConstant constant)
    {
        if (ConstantValueEvaluator.TryEvaluate(expression, out var value))
        {
            constant = value is null
                ? TypedConstant.CreateNull(type: null)
                : TypedConstant.CreatePrimitive(GetPrimitiveType(services, value), value);
            return true;
        }

        constant = TypedConstant.CreateError(type: null);
        return false;
    }

    private static TypeSymbol? GetPrimitiveType(MacroArgumentServices services, object value)
    {
        return value switch
        {
            bool => services.GetSpecialType(SpecialType.System_Boolean),
            char => services.GetSpecialType(SpecialType.System_Char),
            sbyte => services.GetSpecialType(SpecialType.System_SByte),
            byte => services.GetSpecialType(SpecialType.System_Byte),
            short => services.GetSpecialType(SpecialType.System_Int16),
            ushort => services.GetSpecialType(SpecialType.System_UInt16),
            int => services.GetSpecialType(SpecialType.System_Int32),
            uint => services.GetSpecialType(SpecialType.System_UInt32),
            long => services.GetSpecialType(SpecialType.System_Int64),
            ulong => services.GetSpecialType(SpecialType.System_UInt64),
            float => services.GetSpecialType(SpecialType.System_Single),
            double => services.GetSpecialType(SpecialType.System_Double),
            decimal => services.GetSpecialType(SpecialType.System_Decimal),
            string => services.GetSpecialType(SpecialType.System_String),
            DateTime => services.GetSpecialType(SpecialType.System_DateTime),
            _ => services.GetSpecialType(SpecialType.System_Object)
        };
    }
}
