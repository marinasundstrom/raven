using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class DirectiveOrderTests
{
    [Fact]
    public void CompilationUnit_ImportAfterMember_ProducesDiagnostic()
    {
        var code = """
        struct S {}
        import Foo;
        """;

        var tree = SyntaxTree.ParseText(code);
        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.ImportDirectiveOutOfOrder, diagnostic.Descriptor);
    }

    [Fact]
    public void Namespace_AliasAfterMember_ProducesDiagnostic()
    {
        var code = """
        namespace NS {
            struct S {}
            alias A = B;
        };
        """;

        var tree = SyntaxTree.ParseText(code);
        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AliasDirectiveOutOfOrder, diagnostic.Descriptor);
    }

    [Fact]
    public void FileScopedNamespace_AliasAfterGlobalStatement_ProducesDiagnostic()
    {
        var code = """
        namespace NS;
        let x = 0
        alias A = B;
        """;

        var tree = SyntaxTree.ParseText(code);
        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AliasDirectiveOutOfOrder, diagnostic.Descriptor);
    }

    [Theory]
    [InlineData("class Item {}\nimport System.*")]
    [InlineData("class Item {}\nalias A = System.String;")]
    [InlineData("alias A = System.String;\nimport System.*\nalias B = System.Int32;")]
    [InlineData("namespace N { class Item {}\nalias A = System.String; }")]
    [InlineData("namespace N;\nclass Item {}\nalias A = System.String;")]
    public void MisplacedDirectivesPreserveOriginalSourceOrder(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.Equal(source, tree.GetRoot().ToFullString());
        Assert.NotEmpty(tree.GetDiagnostics());
    }

}
