using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroInvocationInputContractTests
{
    [Theory]
    [InlineData("let value = Make!(1)", "ParenthesizedMacroCarrierSyntax")]
    [InlineData("class Model { Make!(1) }", "ParenthesizedMacroCarrierSyntax")]
    [InlineData("Make! Widget() { }", "DeclarationMacroCarrierSyntax")]
    [InlineData("let value = Make! 1 { 2 }", "ExpressionHeaderMacroCarrierSyntax")]
    [InlineData("let value = Make! { 1 }", "TokenTreeMacroCarrierSyntax")]
    public void InvocationPreservesAuthoredCarrierAndProjections(string source, string carrierType)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.DoesNotContain(tree.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().First(n => n is FreestandingMacroExpressionSyntax
            or FreestandingMacroMemberDeclarationSyntax or FreestandingMacroDeclarationSyntax);
        Assert.True(FreestandingMacroInvocation.TryCreate(syntax, out var invocation));
        Assert.Same(syntax, invocation.Syntax);
        Assert.Equal(carrierType, invocation.Carrier.GetType().Name);
        Assert.True(invocation.TryGetMacroName(out var name));
        Assert.Equal("Make", name);
        Assert.Equal(SyntaxKind.ExclamationToken, invocation.ExclamationToken.Kind);
        Assert.Equal(invocation, new FreestandingMacroInvocation(syntax, invocation.Name, invocation.ExclamationToken, invocation.Carrier));
        switch (invocation.Carrier)
        {
            case ParenthesizedMacroCarrierSyntax parenthesized:
                Assert.Same(parenthesized.ArgumentList, invocation.ArgumentList);
                Assert.Same(parenthesized.TokenTree, invocation.TokenTree);
                break;
            case ExpressionHeaderMacroCarrierSyntax expression:
                Assert.Same(expression.Expression, invocation.ExpressionArgument);
                Assert.Same(expression.TokenTree, invocation.TokenTree);
                break;
            case DeclarationMacroCarrierSyntax declaration:
                Assert.Same(declaration.Header, invocation.DeclarationHeader);
                Assert.Same(declaration.TokenTree, invocation.TokenTree);
                break;
            case TokenTreeMacroCarrierSyntax tokens:
                Assert.Same(tokens.TokenTree, invocation.TokenTree);
                break;
        }
    }

    [Fact]
    public void UnsupportedSyntaxReturnsDefaultAndEmptyName()
    {
        Assert.False(FreestandingMacroInvocation.TryCreate(SyntaxFactory.ParseExpression("1"), out var invocation));
        Assert.Equal(default, invocation);
        Assert.False(invocation.TryGetMacroName(out var name));
        Assert.Equal(string.Empty, name);
    }

    [Fact]
    public void CreateRejectsNullForEveryInvocationKind()
    {
        Assert.Throws<ArgumentNullException>(() => FreestandingMacroInvocation.Create((FreestandingMacroExpressionSyntax)null!));
        Assert.Throws<ArgumentNullException>(() => FreestandingMacroInvocation.Create((FreestandingMacroMemberDeclarationSyntax)null!));
        Assert.Throws<ArgumentNullException>(() => FreestandingMacroInvocation.Create((FreestandingMacroDeclarationSyntax)null!));
    }

    [Fact]
    public void FileDependenciesDetectCreationLengthChangesAndDeletion()
    {
        var directory = Directory.CreateTempSubdirectory("raven-macro-input-");
        try
        {
            var path = Path.Combine(directory.FullName, "input.txt");
            var missing = MacroFileDependency.Capture(path);
            Assert.False(missing.Exists);
            Assert.True(missing.IsCurrent());
            File.WriteAllText(path, "one");
            Assert.False(missing.IsCurrent());
            var present = MacroFileDependency.Capture(path);
            Assert.True(present.Exists);
            Assert.Equal(3, present.Length);
            Assert.True(present.IsCurrent());
            File.WriteAllText(path, "longer text");
            Assert.False(present.IsCurrent());
            File.Delete(path);
            Assert.False(present.IsCurrent());
            Assert.Equal(missing, MacroFileDependency.Capture(path));
        }
        finally
        {
            directory.Delete(recursive: true);
        }
    }

    [Fact]
    public void ReaderResolvesRelativeToSourceAndRecordsMissingInputs()
    {
        var directory = Directory.CreateTempSubdirectory("raven-macro-reader-");
        try
        {
            File.WriteAllText(Path.Combine(directory.FullName, "data.txt"), "hello");
            var syntax = SyntaxTree.ParseText("1", path: Path.Combine(directory.FullName, "source.rvn")).GetRoot();
            var dependencies = ImmutableArray.CreateBuilder<MacroFileDependency>();
            var read = MacroFileReader.Read(syntax, "data.txt", dependencies);
            Assert.Equal(MacroFileReadResult.Succeeded(Path.Combine(directory.FullName, "data.txt"), "hello"), read);
            Assert.True(Assert.Single(dependencies).IsCurrent());
            var missing = MacroFileReader.Read(syntax, "missing.txt", dependencies);
            Assert.Equal(MacroFileReadStatus.Missing, missing.Status);
            Assert.Null(missing.Content);
            Assert.Null(missing.Error);
            Assert.Equal(2, dependencies.Count);
            Assert.False(dependencies[1].Exists);
        }
        finally
        {
            directory.Delete(recursive: true);
        }
    }

    [Fact]
    public void InvalidPathProducesFailureWithoutRecordingAnUnresolvedDependency()
    {
        var syntax = SyntaxFactory.ParseExpression("1");
        var dependencies = ImmutableArray.CreateBuilder<MacroFileDependency>();
        var result = MacroFileReader.Read(syntax, "invalid\0path", dependencies);
        Assert.Equal(MacroFileReadStatus.Failed, result.Status);
        Assert.Equal("invalid\0path", result.Path);
        Assert.Null(result.Content);
        Assert.False(string.IsNullOrEmpty(result.Error));
        Assert.Empty(dependencies);
        Assert.Throws<ArgumentNullException>(() => MacroFileReader.Read(syntax, null!, dependencies));
    }

    [Fact]
    public void FileResultFactoriesPreserveValueSemantics()
    {
        Assert.Equal(new MacroFileReadResult("path", "text", MacroFileReadStatus.Success, null), MacroFileReadResult.Succeeded("path", "text"));
        Assert.Equal(new MacroFileReadResult("path", null, MacroFileReadStatus.Missing, null), MacroFileReadResult.Missing("path"));
        var failed = MacroFileReadResult.Failed("path", "reason");
        Assert.True(failed == new MacroFileReadResult("path", null, MacroFileReadStatus.Failed, "reason"));
        Assert.True(failed != MacroFileReadResult.Failed("path", "different"));
    }
}
