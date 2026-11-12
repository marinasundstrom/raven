using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class TypeParameterRecoveryContractTests
{
    [Theory]
    [InlineData("class C<T + U> {}")]
    [InlineData("class C<T + + U> {}")]
    [InlineData("class C<+ T> {}")]
    [InlineData("class C<T, + > {}")]
    public void UnexpectedTokensPreserveSeparatedListShapeAndSource(string source)
    {
        var parser = new TypeDeclarationParser(new BaseParseContext(new Lexer(new StringReader(source))));
        var declaration = Assert.IsType<ClassDeclarationSyntax>(parser.Parse().CreateRed());
        var parameters = declaration.TypeParameterList!.Parameters;
        Assert.Equal(parameters.Count, parameters.Count());
        for (var i = 0; i < parameters.Count; i++)
        {
            Assert.NotNull(parameters[i]);
            if (i < parameters.Count - 1)
                Assert.Equal(SyntaxKind.CommaToken, parameters.GetSeparator(i).Kind);
        }
        Assert.Equal(source, declaration.ToFullString());
        Assert.NotEmpty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }
}
