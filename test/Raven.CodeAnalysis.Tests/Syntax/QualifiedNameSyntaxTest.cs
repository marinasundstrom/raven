using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class QualifiedNameSyntaxTest
{
    [Fact]
    public void QualifiedName_ToString_WithMissingRightIdentifier_DoesNotThrow()
    {
        var code = """
class SessionsStorage(
    private val file: System.IO.
) {
}
""";

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var @class = Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        var parameter = Assert.Single(@class.ParameterList!.Parameters);
        var qualified = Assert.IsType<QualifiedNameSyntax>(parameter.TypeAnnotation!.Type);

        var text = qualified.ToString();

        Assert.Equal("System.IO.", text);
    }
}
