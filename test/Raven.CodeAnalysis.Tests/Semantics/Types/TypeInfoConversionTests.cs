using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class TypeInfoConversionTests : CompilationTestBase
{
    [Fact]
    public void GetTypeInfo_NumericLiteralSuffixesAndRadixPrefixes_ReportLiteralTypes()
    {
        const string source = """
val defaultInt = 2
val retryCount = 2b
val basePrice = 19.95m
val serviceFee = 1m
val binaryMask = 0b1111_0000
val colorMask = 0xFF
""";

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var locals = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .ToDictionary(static declarator => declarator.Identifier.ValueText);

        AssertLiteralLocal(locals["defaultInt"], SpecialType.System_Int32, 2);
        AssertLiteralLocal(locals["retryCount"], SpecialType.System_Byte, (byte)2);
        AssertLiteralLocal(locals["basePrice"], SpecialType.System_Decimal, 19.95m);
        AssertLiteralLocal(locals["serviceFee"], SpecialType.System_Decimal, 1m);
        AssertLiteralLocal(locals["binaryMask"], SpecialType.System_Int32, 240);
        AssertLiteralLocal(locals["colorMask"], SpecialType.System_Int32, 255);

        void AssertLiteralLocal(VariableDeclaratorSyntax declarator, SpecialType expectedType, object expectedValue)
        {
            var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
            Assert.Equal(expectedType, local.Type.SpecialType);

            var literal = Assert.IsType<LiteralExpressionSyntax>(declarator.Initializer!.Value);
            var typeInfo = model.GetTypeInfo(literal);
            Assert.Equal(expectedType, typeInfo.Type?.SpecialType);
            Assert.Equal(expectedType, typeInfo.ConvertedType?.SpecialType);
            Assert.Equal(expectedValue, literal.Token.Value);
        }
    }

    [Fact]
    public void GetTypeInfo_InterpolatedStrings_ReportStringAndExpressionTypes()
    {
        const string source = """"
val count = 3
val message = "Next: ${count + 1}"
val multiline = """
    Value $message
    """
"""";

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var locals = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .ToDictionary(static declarator => declarator.Identifier.ValueText);
        var interpolatedStrings = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().ToArray();
        var interpolationExpressions = interpolatedStrings
            .SelectMany(static interpolated => interpolated.Contents.OfType<InterpolationSyntax>())
            .Select(static interpolation => interpolation.Expression)
            .ToArray();

        Assert.Equal(SpecialType.System_String, Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["message"])).Type.SpecialType);
        Assert.Equal(SpecialType.System_String, Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["multiline"])).Type.SpecialType);

        Assert.Collection(
            interpolatedStrings,
            interpolated => Assert.Equal(SpecialType.System_String, model.GetTypeInfo(interpolated).Type?.SpecialType),
            interpolated => Assert.Equal(SpecialType.System_String, model.GetTypeInfo(interpolated).Type?.SpecialType));
        Assert.Collection(
            interpolationExpressions,
            expression => Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(expression).Type?.SpecialType),
            expression => Assert.Equal(SpecialType.System_String, model.GetTypeInfo(expression).Type?.SpecialType));
    }

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
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

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
