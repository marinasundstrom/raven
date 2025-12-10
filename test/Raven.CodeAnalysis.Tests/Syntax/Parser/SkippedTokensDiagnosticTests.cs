using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class SkippedTokensDiagnosticTests
{
    [Fact]
    public void CompilationUnit_SkipsUnexpectedTokens_WithoutUserDiagnostic()
    {
        var tree = SyntaxTree.ParseText(";");

        Assert.DoesNotContain(
            tree.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.SkippedTokens);
    }

    [Fact]
    public void TypeMembers_SkipUnexpectedTokens_WithoutUserDiagnostic()
    {
        var code = "class C { 123 func Foo() {} }";
        var tree = SyntaxTree.ParseText(code);

        Assert.DoesNotContain(
            tree.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.SkippedTokens);
    }
}
