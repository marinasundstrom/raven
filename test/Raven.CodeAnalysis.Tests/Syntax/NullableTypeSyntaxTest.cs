using System;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class NullableTypeSyntaxTest
{
    [Theory]
    [InlineData("string", typeof(PredefinedTypeSyntax))]
    [InlineData("Foo", typeof(IdentifierNameSyntax))]
    [InlineData("Foo.Bar", typeof(QualifiedNameSyntax))]
    [InlineData("Foo<int>", typeof(GenericNameSyntax))]
    public void NullableType_AllowsAnyElementType(string typeText, Type expectedSyntax)
    {
        var code = $"val x: {typeText}? = null";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var declaration = local.Declaration;
        var typeSyntax = declaration.Declarators[0].TypeAnnotation!.Type;
        var nullable = Assert.IsType<NullableTypeSyntax>(typeSyntax);
        Assert.IsType(expectedSyntax, nullable.ElementType);
    }
}
