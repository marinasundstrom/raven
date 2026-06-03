using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class EncodedStringLiteralSemanticTests : CompilationTestBase
{
    [Fact]
    public void EncodedStringLiterals_BindAsByteArrays()
    {
        const string source = """
val utf8 = "Hi"u8
val ascii = "!"ascii
val combined: byte[] = [|...utf8, ...ascii|]
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var locals = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .ToDictionary(static declarator => declarator.Identifier.ValueText);

        AssertByteArray(Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["utf8"])).Type);
        AssertByteArray(Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["ascii"])).Type);
        AssertByteArray(Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["combined"])).Type);

        var literals = root.DescendantNodes().OfType<LiteralExpressionSyntax>()
            .Where(static literal => literal.Token.Text.EndsWith("u8", StringComparison.Ordinal) ||
                                     literal.Token.Text.EndsWith("ascii", StringComparison.Ordinal))
            .ToArray();

        Assert.Collection(
            literals,
            literal => AssertByteArray(model.GetTypeInfo(literal).Type),
            literal => AssertByteArray(model.GetTypeInfo(literal).Type));

        var arrayExpression = root.DescendantNodes().OfType<ArrayExpressionSyntax>().Single();
        AssertByteArray(model.GetTypeInfo(arrayExpression).Type);
    }

    private static void AssertByteArray(ITypeSymbol? type)
    {
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(type);
        Assert.Equal(1, arrayType.Rank);
        Assert.Equal(SpecialType.System_Byte, arrayType.ElementType.SpecialType);
    }
}
