using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

[Trait("Target", "DotNet")]
public sealed class BinaryOperatorServiceTests : CompilationTestBase
{
    [Fact]
    public void NumericPromotionUsesCanonicalTypesFromServices()
    {
        var compilation = CreateCompilation();
        var services = new RecordingServices(compilation);
        var left = compilation.GetSpecialType(SpecialType.System_Int32);
        var right = compilation.GetSpecialType(SpecialType.System_Int64);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, left, right, out var op));
        Assert.Same(right, op.ResultType);
        Assert.True(services.SpecialTypeLookups > 0);
    }

    [Fact]
    public void LiftedArithmeticUsesNullableTypeFactory()
    {
        var compilation = CreateCompilation();
        var services = new RecordingServices(compilation);
        var operand = compilation.GetSpecialType(SpecialType.System_Int32).GetNullableType();
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, operand, operand, out var op));
        Assert.Equal(BinaryOperatorKind.Addition | BinaryOperatorKind.Lifted, op.OperatorKind);
        Assert.Same(services.NullableResult, op.ResultType);
        Assert.True(op.ResultType.IsNullable);
        Assert.Equal(1, services.NullableCreations);
    }

    [Fact]
    public void UserDefinedApplicabilityUsesStandardConversions()
    {
        var (compilation, _) = CreateCompilation("""
            public class Number {
                public static func +(left: Number, right: Number) -> Number => left
            }
            """);
        var operand = compilation.GetTypeByMetadataName("Number")!;
        var services = new RecordingServices(compilation);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, operand, operand, out var op));
        Assert.NotNull(op.MethodSymbol);
        Assert.Equal(new[] { false, false }, services.UserDefinedConversionFlags);
    }

    private sealed class RecordingServices(Compilation compilation) : BinaryOperatorServices
    {
        public int SpecialTypeLookups { get; private set; }
        public int NullableCreations { get; private set; }
        public TypeSymbol? NullableResult { get; private set; }
        public List<bool> UserDefinedConversionFlags { get; } = [];
        public TypeSymbol ErrorTypeSymbol => compilation.ErrorTypeSymbol;
        public bool HasRuntimeSelfContract => ((BinaryOperatorServices)compilation).HasRuntimeSelfContract;

        public NamedTypeSymbol GetSpecialType(SpecialType specialType)
        {
            SpecialTypeLookups++;
            return compilation.GetSpecialType(specialType);
        }

        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined)
        {
            UserDefinedConversionFlags.Add(includeUserDefined);
            return compilation.ClassifyConversion(source, destination, includeUserDefined);
        }

        public TypeSymbol CreateNullableType(TypeSymbol type)
        {
            NullableCreations++;
            return NullableResult = ((BinaryOperatorServices)compilation).CreateNullableType(type);
        }

        public MethodSymbol? ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method)
            => ((BinaryOperatorServices)compilation).ProjectRuntimeSelfOperator(receiver, method);
    }
}
