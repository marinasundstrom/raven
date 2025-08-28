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
}
