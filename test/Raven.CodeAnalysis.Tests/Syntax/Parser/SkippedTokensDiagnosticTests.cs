using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class SkippedTokensDiagnosticTests
{
    [Fact]
    public void CompilationUnit_SkipsUnexpectedTokens_WithDiagnostic()
    {
        var tree = SyntaxTree.ParseText(";");

        var diagnostic = Assert.Single(tree.GetDiagnostics());

        Assert.Equal(CompilerDiagnostics.SkippedTokens, diagnostic.Descriptor);
        var message = diagnostic.GetMessage();
        Assert.Contains("EndOfFileToken", message);
        Assert.Contains("ClassKeyword", message);
    }

    [Fact]
    public void TypeMembers_SkipUnexpectedTokens_WithDiagnostic()
    {
        var code = "class C { 123 func Foo() {} }";
        var tree = SyntaxTree.ParseText(code);

        var diagnostic = Assert.Single(tree.GetDiagnostics());

        Assert.Equal(CompilerDiagnostics.SkippedTokens, diagnostic.Descriptor);
        var message = diagnostic.GetMessage();
        Assert.Contains("FuncKeyword", message);
        Assert.Contains("CloseBraceToken", message);
    }
}
