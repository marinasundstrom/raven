using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class TypeInfoConversionTests : CompilationTestBase
{
    [Fact]
    public void GetTypeInfo_LiteralInTypedInitializer_ReportsImplicitNumericConversion()
    {
        const string source = """
val x: double = 1
""";

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var literal = tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().Single();
        var typeInfo = model.GetTypeInfo(literal);

        Assert.NotNull(typeInfo.Type);
        Assert.Equal(SpecialType.System_Double, typeInfo.ConvertedType?.SpecialType);
        Assert.True(typeInfo.Conversion.Exists);
        Assert.True(typeInfo.Conversion.IsImplicit);
        Assert.True(typeInfo.Conversion.IsNumeric);
    }

    [Fact]
    public void GetTypeInfo_ExplicitCast_ReportsNumericConversionAndResultType()
    {
        const string source = """
val x = (double)1
""";

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var cast = tree.GetRoot().DescendantNodes().OfType<CastExpressionSyntax>().Single();
        var typeInfo = model.GetTypeInfo(cast);

        Assert.Equal(SpecialType.System_Double, typeInfo.Type?.SpecialType);
        Assert.Equal(SpecialType.System_Double, typeInfo.ConvertedType?.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(typeInfo.Type, typeInfo.ConvertedType));
        Assert.True(typeInfo.Conversion.Exists);
        Assert.True(typeInfo.Conversion.IsNumeric);
        Assert.False(typeInfo.Conversion.IsIdentity);
    }

    [Fact]
    public void GetTypeInfo_AsExpression_ReportsReferenceConversionAndNullableResultType()
    {
        const string source = """
val obj: object = ""
val s = obj as string
""";

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var asExpression = tree.GetRoot().DescendantNodes().OfType<AsExpressionSyntax>().Single();
        var typeInfo = model.GetTypeInfo(asExpression);

        var nullableType = Assert.IsType<NullableTypeSymbol>(typeInfo.Type);
        Assert.True(SymbolEqualityComparer.Default.Equals(typeInfo.Type, typeInfo.ConvertedType));
        Assert.Equal(SpecialType.System_String, nullableType.UnderlyingType.SpecialType);
        Assert.True(typeInfo.Conversion.Exists);
        Assert.True(typeInfo.Conversion.IsReference);
    }

    [Fact]
    public void GetTypeInfo_UnconvertedLiteral_ReportsIdentityConversion()
    {
        const string source = """
val x = 1
""";

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var literal = tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().Single();
        var typeInfo = model.GetTypeInfo(literal);

        Assert.NotNull(typeInfo.Type);
        Assert.True(SymbolEqualityComparer.Default.Equals(typeInfo.Type, typeInfo.ConvertedType));
        Assert.True(typeInfo.Conversion.Exists);
        Assert.True(typeInfo.Conversion.IsIdentity);
        Assert.True(typeInfo.Conversion.IsImplicit);
    }
}
