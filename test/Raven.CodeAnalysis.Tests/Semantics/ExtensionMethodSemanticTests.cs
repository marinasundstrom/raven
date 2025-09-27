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
        const string mainSource = """
import Sample.Extensions.*

let value = 5
let result = value.Double()
""";

        const string extensionSource = """
import System.Runtime.CompilerServices.*

namespace Sample.Extensions {
    public static class NumberExtensions {
        [ExtensionAttribute]
        public static Double(x: int) -> int {
            return x + x
        }
    }
}
""";

        var extensionTree = SyntaxTree.ParseText(extensionSource);
        var extensionCompilation = CreateCompilation(
            extensionTree,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            assemblyName: "Sample.Extensions");
        extensionCompilation.EnsureSetup();
        var extensionDiagnostics = extensionCompilation.GetDiagnostics();
        Assert.True(extensionDiagnostics.IsEmpty, string.Join(Environment.NewLine, extensionDiagnostics.Select(d => d.ToString())));

        var references = GetMetadataReferences()
            .Concat(new[] { new CompilationReference(extensionCompilation) })
            .ToArray();

        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(mainTree, references: references);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var extensionNamespace = compilation.GetNamespaceSymbol("Sample.Extensions");
        Assert.NotNull(extensionNamespace);

        var model = compilation.GetSemanticModel(mainTree);
        var whereInvocation = mainTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Double");

        var boundWhere = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(whereInvocation));
        Assert.True(boundWhere.Method.IsExtensionMethod);
        Assert.Equal("NumberExtensions", boundWhere.Method.ContainingType?.Name);
        var containingNamespace = boundWhere.Method.ContainingType?.ContainingNamespace;
        Assert.Equal("Extensions", containingNamespace?.Name);
        Assert.Equal("Sample", containingNamespace?.ContainingNamespace?.Name);

        Assert.NotNull(boundWhere.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            boundWhere.Method.Parameters[0].Type,
            boundWhere.ExtensionReceiver!.Type));
    }

    [Fact]
    public void MemberAccess_WithTypeImport_BindsExtensionInvocation()
    {
        const string mainSource = """
import Sample.Extensions.NumberExtensions

let value = 10
let doubled = value.Double()
""";

        const string extensionSource = """
import System.Runtime.CompilerServices.*

namespace Sample.Extensions {
    public static class NumberExtensions {
        [ExtensionAttribute]
        public static Double(x: int) -> int {
            return x * 2
        }
    }
}
""";

        var extensionTree = SyntaxTree.ParseText(extensionSource);
        var extensionCompilation = CreateCompilation(
            extensionTree,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            assemblyName: "Sample.Extensions");
        extensionCompilation.EnsureSetup();
        var extensionDiagnostics = extensionCompilation.GetDiagnostics();
        Assert.True(extensionDiagnostics.IsEmpty, string.Join(Environment.NewLine, extensionDiagnostics.Select(d => d.ToString())));

        var references = GetMetadataReferences()
            .Concat(new[] { new CompilationReference(extensionCompilation) })
            .ToArray();

        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(mainTree, references: references);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(mainTree);
        var whereInvocation = mainTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Double");

        var boundWhere = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(whereInvocation));
        Assert.True(boundWhere.Method.IsExtensionMethod);
        Assert.Equal("NumberExtensions", boundWhere.Method.ContainingType?.Name);
        var containingNamespace = boundWhere.Method.ContainingType?.ContainingNamespace;
        Assert.Equal("Extensions", containingNamespace?.Name);
        Assert.Equal("Sample", containingNamespace?.ContainingNamespace?.Name);

        Assert.NotNull(boundWhere.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            boundWhere.Method.Parameters[0].Type,
            boundWhere.ExtensionReceiver!.Type));
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
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Test");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("Test", boundInvocation.Method.Name);
        Assert.Equal("Extensions", boundInvocation.Method.ContainingType?.Name);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            boundInvocation.Method.Parameters[0].Type,
            boundInvocation.ExtensionReceiver!.Type));
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
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Test");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("Test", boundInvocation.Method.Name);
        Assert.Equal("Extensions", boundInvocation.Method.ContainingType?.Name);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            boundInvocation.Method.Parameters[0].Type,
            boundInvocation.ExtensionReceiver!.Type));
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
        Assert.True(SymbolEqualityComparer.Default.Equals(
            boundInvocation.Method.Parameters[0].Type,
            boundInvocation.ExtensionReceiver!.Type));

        var arguments = boundInvocation.Arguments.ToArray();
        Assert.Equal(boundInvocation.Method.Parameters.Length - 1, arguments.Length);

        Assert.Collection(
            arguments,
            argument => Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method.Parameters[1].Type, argument.Type)),
            argument => Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method.Parameters[2].Type, argument.Type)));
    }

    [Fact]
    public void MemberAccess_WithGenericExtension_InferReceiverTypeArgument()
    {
        const string source = """
import System.Runtime.CompilerServices.*

let box = Box<int>()
let result = box.Test(2)

class Box<T>
{
}

public static class Extensions
{
    [ExtensionAttribute]
    public static Test<T>(items: Box<T>, value: T) -> T
    {
        return value
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
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Test");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("Test", boundInvocation.Method.Name);
        Assert.Equal("Extensions", boundInvocation.Method.ContainingType?.Name);

        Assert.True(boundInvocation.Method.IsGenericMethod);
        Assert.Single(boundInvocation.Method.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, boundInvocation.Method.TypeArguments[0].SpecialType);

        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            boundInvocation.Method.Parameters[0].Type,
            boundInvocation.ExtensionReceiver!.Type));

        var arguments = boundInvocation.Arguments.ToArray();
        Assert.Single(arguments);
        var argumentType = arguments[0].Type is LiteralTypeSymbol literal
            ? literal.UnderlyingType
            : arguments[0].Type;

        Assert.True(SymbolEqualityComparer.Default.Equals(
            boundInvocation.Method.Parameters[1].Type,
            argumentType));
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
        const string mainSource = """
import Sample.Extensions.NumberExtensions

class Query {
    Run() -> int {
        let value = 3
        return value.Double()
    }
}
""";

        const string extensionSource = """
import System.Runtime.CompilerServices.*

namespace Sample.Extensions {
    public static class NumberExtensions {
        [ExtensionAttribute]
        public static Double(x: int) -> int {
            return x + x
        }
    }
}
""";

        var extensionTree = SyntaxTree.ParseText(extensionSource);
        var extensionCompilation = CreateCompilation(
            extensionTree,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            assemblyName: "Sample.Extensions");
        extensionCompilation.EnsureSetup();
        var extensionDiagnostics = extensionCompilation.GetDiagnostics();
        Assert.True(extensionDiagnostics.IsEmpty, string.Join(Environment.NewLine, extensionDiagnostics.Select(d => d.ToString())));

        var references = GetMetadataReferences()
            .Concat(new[] { new CompilationReference(extensionCompilation) })
            .ToArray();

        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(mainTree, references: references);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(mainTree);
        var methodSyntax = mainTree.GetRoot()
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
