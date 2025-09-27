using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExtensionMethodSemanticTests : CompilationTestBase
{
    [Fact]
    public void MemberAccess_WithNamespaceImport_BindsExtensionInvocation()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.*

class Query {
    Run() -> int {
        let items = List<int>()
        items.Add(1)
        items.Add(2)
        return items.Where(func (value) => value > 0).Count()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var whereInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Where");

        var boundWhere = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(whereInvocation));
        Assert.True(boundWhere.Method.IsExtensionMethod);
        Assert.Equal(
            "global::System.Linq.Enumerable",
            boundWhere.Method.ContainingType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var whereArguments = boundWhere.Arguments.ToArray();
        Assert.Equal(boundWhere.Method.Parameters.Length - 1, whereArguments.Length);
        Assert.NotNull(boundWhere.ExtensionReceiver);
        Assert.Equal(boundWhere.Method.Parameters[0].Type, boundWhere.ExtensionReceiver!.Type);
        Assert.IsType<BoundLambdaExpression>(whereArguments[0]);

        var countInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Count");

        var boundCount = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(countInvocation));
        Assert.True(boundCount.Method.IsExtensionMethod);
        Assert.Equal(
            "global::System.Linq.Enumerable",
            boundCount.Method.ContainingType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.NotNull(boundCount.ExtensionReceiver);
        Assert.Equal(boundCount.Method.Parameters[0].Type, boundCount.ExtensionReceiver!.Type);
    }

    [Fact]
    public void MemberAccess_WithTypeImport_BindsExtensionInvocation()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.Enumerable

let items = List<int>()
items.Add(1)
let hasItems = items.Any()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var anyInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Any");

        var boundAny = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(anyInvocation));
        Assert.True(boundAny.Method.IsExtensionMethod);
        Assert.Equal(
            "global::System.Linq.Enumerable",
            boundAny.Method.ContainingType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.NotNull(boundAny.ExtensionReceiver);
        Assert.Equal(boundAny.Method.Parameters[0].Type, boundAny.ExtensionReceiver!.Type);
    }

    [Fact]
    public void MemberAccess_WithSourceExtension_BindsExtensionInvocation()
    {
        const string source = """
import System.Runtime.CompilerServices.*

let x = "test"
let result = x.Test()

public static class Extensions {
    [ExtensionAttribute]
    public static Test(x: string) -> int {
        return 42
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("Test", boundInvocation.Method.Name);
        Assert.Equal("Extensions", boundInvocation.Method.ContainingType?.Name);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal(boundInvocation.Method.Parameters[0].Type, boundInvocation.ExtensionReceiver!.Type);
    }

    [Fact]
    public void MemberAccess_WithSourceExtensionAndBaseTypeReceiver_BindsExtensionInvocation()
    {
        const string source = """
import System.Runtime.CompilerServices.*

let x = "test"
let result = x.Test()

public static class Extensions {
    [ExtensionAttribute]
    public static Test(x: object) -> int {
        return 42
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("Test", boundInvocation.Method.Name);
        Assert.Equal("Extensions", boundInvocation.Method.ContainingType?.Name);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal(boundInvocation.Method.Parameters[0].Type, boundInvocation.ExtensionReceiver!.Type);
    }

    [Fact]
    public void MemberAccess_WithSourceExtensionAndAdditionalParameters_BindsInvocationArguments()
    {
        const string source = """
import System.Runtime.CompilerServices.*

let text = "value"
let suffix = "!"
let count = 3
let result = text.AddSuffix(suffix, count)

public static class Extensions {
    [ExtensionAttribute]
    public static AddSuffix(x: string, suffix: string, count: int) -> string {
        return x
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "AddSuffix");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("AddSuffix", boundInvocation.Method.Name);
        Assert.Equal("Extensions", boundInvocation.Method.ContainingType?.Name);

        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal(boundInvocation.Method.Parameters[0].Type, boundInvocation.ExtensionReceiver!.Type);

        var arguments = boundInvocation.Arguments.ToArray();
        Assert.Equal(boundInvocation.Method.Parameters.Length - 1, arguments.Length);

        Assert.Collection(
            arguments,
            argument => Assert.Equal(boundInvocation.Method.Parameters[1].Type, argument.Type),
            argument => Assert.Equal(boundInvocation.Method.Parameters[2].Type, argument.Type));
    }

    [Fact]
    public void MemberAccess_WithMultipleSourceExtensions_ResolvesMatchingReceiver()
    {
        const string source = """
import System.Runtime.CompilerServices.*

let text = "value"
let number = 5

let textResult = text.Describe()
let numberResult = number.Describe()

public static class TextExtensions {
    [ExtensionAttribute]
    public static Describe(x: string) -> string {
        return x
    }
}

public static class NumberExtensions {
    [ExtensionAttribute]
    public static Describe(x: int) -> string {
        return "number"
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocations = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .ToArray();

        Assert.Equal(2, invocations.Length);

        var textInvocationSyntax = invocations.Single(
            node => node.Expression is MemberAccessExpressionSyntax member
                    && member.Expression is IdentifierNameSyntax identifier
                    && identifier.Identifier.Text == "text");

        var textInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(textInvocationSyntax));
        Assert.True(textInvocation.Method.IsExtensionMethod);
        Assert.Equal("Describe", textInvocation.Method.Name);
        Assert.Equal(SpecialType.System_String, textInvocation.Method.Parameters[0].Type.SpecialType);
        Assert.NotNull(textInvocation.ExtensionReceiver);
        Assert.Equal(textInvocation.Method.Parameters[0].Type, textInvocation.ExtensionReceiver!.Type);

        var numberInvocationSyntax = invocations.Single(
            node => node.Expression is MemberAccessExpressionSyntax member
                    && member.Expression is IdentifierNameSyntax identifier
                    && identifier.Identifier.Text == "number");

        var numberInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(numberInvocationSyntax));
        Assert.True(numberInvocation.Method.IsExtensionMethod);
        Assert.Equal("Describe", numberInvocation.Method.Name);
        Assert.Equal(SpecialType.System_Int32, numberInvocation.Method.Parameters[0].Type.SpecialType);
        Assert.NotNull(numberInvocation.ExtensionReceiver);
        Assert.Equal(numberInvocation.Method.Parameters[0].Type, numberInvocation.ExtensionReceiver!.Type);
    }

    [Fact]
    public void Lowerer_RewritesExtensionInvocation()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.*

class Query {
    Run() -> int {
        let items = List<int>()
        return items.Count()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == "Run");

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        var boundBody = (BoundBlockStatement)model.GetBoundNode(methodSyntax.Body!)!;

        var lowered = Lowerer.LowerBlock(methodSymbol, boundBody);

        var returnStatement = Assert.IsType<BoundReturnStatement>(lowered.Statements.Last());
        var invocation = Assert.IsType<BoundInvocationExpression>(returnStatement.Expression);

        Assert.True(invocation.Method.IsExtensionMethod);
        Assert.Null(invocation.Receiver);

        var arguments = invocation.Arguments.ToArray();
        Assert.Equal(invocation.Method.Parameters.Length, arguments.Length);
        Assert.IsType<BoundLocalAccess>(arguments[0]);
    }
}
