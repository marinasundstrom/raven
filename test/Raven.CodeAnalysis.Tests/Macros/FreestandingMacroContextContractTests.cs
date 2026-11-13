using System;
using System.IO;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class FreestandingMacroContextContractTests
{
    [Theory]
    [InlineData("let value = Make!(1)", 1)]
    [InlineData("class Model { Make!(1) }", 1)]
    [InlineData("Make! Widget() { }", 0)]
    [InlineData("let value = Make! 1 { 2 }", 1)]
    [InlineData("let value = Make! { 1 }", 0)]
    public void ContextPreservesEveryInvocationCarrier(string source, int arguments)
    {
        var invocation = Parse(source);
        using var cancellation = new CancellationTokenSource();
        var context = new FreestandingMacroContext(new NoSemanticQueries(), invocation, cancellation.Token);
        Assert.Same(invocation.Syntax, context.Syntax);
        Assert.Same(invocation.Carrier, context.Carrier);
        Assert.Same(invocation.Name, context.Name);
        Assert.Equal(invocation.ExclamationToken, context.ExclamationToken);
        Assert.Same(invocation.ArgumentList, context.ArgumentList);
        Assert.Same(invocation.ExpressionArgument, context.ExpressionArgument);
        Assert.Same(invocation.DeclarationHeader, context.DeclarationHeader);
        Assert.Same(invocation.TokenTree, context.TokenTree);
        Assert.Equal(invocation, context.Invocation);
        Assert.Equal(cancellation.Token, context.CancellationToken);
        Assert.Equal(arguments, context.Arguments.Length);
        if (invocation.ExpressionArgument is { } expression)
            Assert.Same(expression, Assert.Single(context.Arguments).Expression);
    }

    [Fact]
    public void DiagnosticsRetainAuthoredLocationsAndNames()
    {
        var invocation = Parse("let value = Make!(1, named: 2)");
        var context = new FreestandingMacroContext(new NoSemanticQueries(), invocation);
        Assert.False(context.Arguments[0].IsNamed);
        Assert.Equal("named", context.Arguments[1].Name);
        Assert.Equal(invocation.Name.Span, context.CreateDiagnostic("message").Location!.SourceSpan);
        var argument = context.Arguments[1];
        var diagnostic = context.CreateArgumentDiagnostic(argument, "argument", DiagnosticSeverity.Info, "CODE");
        Assert.Equal(argument.Syntax.Span, diagnostic.Location!.SourceSpan);
        Assert.Equal("CODE", diagnostic.Code);
        Assert.Equal(DiagnosticSeverity.Info, diagnostic.Severity);
        Assert.Equal(argument.Expression.Span, context.CreateDiagnostic("specific", syntax: argument.Expression).Location!.SourceSpan);
        Assert.Throws<ArgumentNullException>(() => context.CreateArgumentDiagnostic(null!, "bad"));
        Assert.Throws<ArgumentNullException>(() => new FreestandingMacroContext(null!, invocation));
    }

    [Fact]
    public void FileReadsTrackRelativeDependenciesAndImmutableSnapshots()
    {
        var directory = Directory.CreateTempSubdirectory("raven-context-");
        try
        {
            File.WriteAllText(Path.Combine(directory.FullName, "input.txt"), "contents");
            var invocation = Parse("let value = Make!()", Path.Combine(directory.FullName, "source.rvn"));
            var context = new FreestandingMacroContext(new NoSemanticQueries(), invocation);
            var before = context.GetFileDependencies();
            var read = context.ReadFile("input.txt");
            Assert.Equal(MacroFileReadStatus.Success, read.Status);
            Assert.Equal("contents", read.Content);
            Assert.Equal(Path.Combine(directory.FullName, "input.txt"), read.Path);
            Assert.Empty(before);
            var first = context.GetFileDependencies();
            Assert.True(Assert.Single(first).Exists);
            Assert.Equal(MacroFileReadStatus.Missing, context.ReadFile("missing.txt").Status);
            Assert.Single(first);
            Assert.Equal(2, context.GetFileDependencies().Length);
            context.AddFileDependencies(first);
            Assert.Equal(3, context.GetFileDependencies().Length);
            Assert.Throws<ArgumentNullException>(() => context.ReadFile(null!));
        }
        finally
        {
            directory.Delete(recursive: true);
        }
    }

    private static FreestandingMacroInvocation Parse(string source, string path = "")
    {
        var tree = SyntaxTree.ParseText(source, path: path);
        Assert.DoesNotContain(tree.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().First(n => n is FreestandingMacroExpressionSyntax
            or FreestandingMacroMemberDeclarationSyntax or FreestandingMacroDeclarationSyntax);
        Assert.True(FreestandingMacroInvocation.TryCreate(syntax, out var invocation));
        return invocation;
    }

    private sealed class NoSemanticQueries : MacroArgumentServices
    {
        public TypeSymbol GetSpecialType(SpecialType specialType) => throw new InvalidOperationException("Unexpected semantic query");
        public TypeSymbol? GetArgumentType(Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => throw new InvalidOperationException("Unexpected semantic query");
        public TypeSymbol? ResolveConstraint(Type runtimeType) => throw new InvalidOperationException("Unexpected semantic query");
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => throw new InvalidOperationException("Unexpected semantic query");
    }
}
