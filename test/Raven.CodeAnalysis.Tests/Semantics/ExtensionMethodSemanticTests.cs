using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExtensionMethodSemanticTests : CompilationTestBase
{
    [Fact]
    public void MemberAccess_WithInaccessibleProperty_PrefersExtensionInvocation()
    {
        const string source = """
func Main() {
    val widget = Widget()
    val total = widget.Count()
}

class Widget {
    protected Count: int { get { return 0; } }
}

extension WidgetExtensions for Widget {
    Count() -> int {
        return 1
    }
}
""";

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Count");

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);

        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("Count", selected.Name);
        Assert.Equal("WidgetExtensions", selected.ContainingType?.Name);
    }

    [Fact]
    public void MemberAccess_WithNamespaceImport_BindsExtensionInvocation()
    {
        const string mainSource = """
import Sample.Extensions.*

val value = 5
val result = value.Double()
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

val value = 10
val doubled = value.Double()
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

val x = "test"
val result = x.Test()

public static class Extensions {
    [ExtensionAttribute]
    public static Test(x: string) -> int {
        return 42
    }
}
""";

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
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

val x = "test"
val result = x.Test()

public static class Extensions {
    [ExtensionAttribute]
    public static Test(x: object) -> int {
        return 42
    }
}
""";

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
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
    public void MemberAccess_LinqWhereOnCollectionExpression_BindsSuccessfully()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Linq.*

val numbers = [1, 2, 3]
val result = numbers.Where(value => value == 2)
""";

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("Where", boundInvocation.Method.Name);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal(boundInvocation.ExtensionReceiver!.Type, boundInvocation.Method.Parameters[0].Type);
    }

    [Fact]
    public void MemberAccess_LinqWhereWithImplicitLambda_BindsSuccessfully()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Linq.*

val numbers = [1, 2, 3]
val result = numbers.Where(value => value == 2)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("Where", boundInvocation.Method.Name);
        Assert.Equal("Enumerable", boundInvocation.Method.ContainingType?.Name);
        var containingNamespace = boundInvocation.Method.ContainingType?.ContainingNamespace;
        Assert.Equal("Linq", containingNamespace?.Name);
        Assert.Equal("System", containingNamespace?.ContainingNamespace?.Name);

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.Contains(
            boundLambda.CandidateDelegates,
            candidate => candidate.Name == "Func" &&
                candidate.Arity == 2 &&
                candidate.TypeArguments[0] is { SpecialType: SpecialType.System_Int32 } &&
                candidate.TypeArguments[1] is { SpecialType: SpecialType.System_Boolean });
    }

    [Fact]
    public void MemberAccess_LinqWhereWithExplicitLambdaType_BindsSuccessfully()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Linq.*

val numbers = [1, 2, 3]
val result = numbers.Where((value: int) -> bool => value == 2)
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MemberAccess_WithSourceExtensionAndAdditionalParameters_BindsInvocationArguments()
    {
        const string source = """
import System.Runtime.CompilerServices.*

val text = "value"
val suffix = "!"
val count = 3
val result = text.AddSuffix(suffix, count)

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

val box = Box<int>()
val result = box.Test(2)

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

val text = "value"
val number = 5

val textResult = text.Describe()
val numberResult = number.Describe()

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
    public void MemberAccess_WithShadowingExtensions_PrefersMostSpecificReceiver()
    {
        const string source = """
import System.Runtime.CompilerServices.*

val text = "value"
val textAsObject: object = text

val stringResult = text.Describe()
val objectResult = textAsObject.Describe()

public static class DescribeExtensions {
    [ExtensionAttribute]
    public static Describe(x: object) -> string {
        return "object"
    }

    [ExtensionAttribute]
    public static Describe(x: string) -> string {
        return x
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

        var stringInvocationSyntax = invocations.Single(
            node => node.Expression is MemberAccessExpressionSyntax member
                    && member.Expression is IdentifierNameSyntax identifier
                    && identifier.Identifier.Text == "text");

        var stringInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(stringInvocationSyntax));
        Assert.True(stringInvocation.Method.IsExtensionMethod);
        Assert.Equal("Describe", stringInvocation.Method.Name);
        Assert.Equal(SpecialType.System_String, stringInvocation.Method.Parameters[0].Type.SpecialType);
        Assert.NotNull(stringInvocation.ExtensionReceiver);
        Assert.Equal(stringInvocation.Method.Parameters[0].Type, stringInvocation.ExtensionReceiver!.Type);

        var objectInvocationSyntax = invocations.Single(
            node => node.Expression is MemberAccessExpressionSyntax member
                    && member.Expression is IdentifierNameSyntax identifier
                    && identifier.Identifier.Text == "textAsObject");

        var objectInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(objectInvocationSyntax));
        Assert.True(objectInvocation.Method.IsExtensionMethod);
        Assert.Equal("Describe", objectInvocation.Method.Name);
        Assert.Equal(SpecialType.System_Object, objectInvocation.Method.Parameters[0].Type.SpecialType);
        Assert.NotNull(objectInvocation.ExtensionReceiver);
        Assert.Equal(objectInvocation.Method.Parameters[0].Type, objectInvocation.ExtensionReceiver!.Type);
    }

    [Fact]
    public void MemberAccess_WithGenericReceiverExtension_BindsTypeParameterReceiver()
    {
        const string source = """
import System.Runtime.CompilerServices.*

val text = "value"
val number = 42

val textResult = text.Identity()
val numberResult = number.Identity()

public static class GenericExtensions {
    [ExtensionAttribute]
    public static Identity<T>(value: T) -> T {
        return value
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
        Assert.True(textInvocation.Method.IsGenericMethod);
        Assert.Equal("Identity", textInvocation.Method.Name);
        Assert.Single(textInvocation.Method.TypeArguments);
        Assert.Equal(SpecialType.System_String, textInvocation.Method.TypeArguments[0].SpecialType);
        Assert.NotNull(textInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            textInvocation.Method.Parameters[0].Type,
            textInvocation.ExtensionReceiver!.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(
            textInvocation.Method.ReturnType,
            textInvocation.ExtensionReceiver!.Type));

        var numberInvocationSyntax = invocations.Single(
            node => node.Expression is MemberAccessExpressionSyntax member
                    && member.Expression is IdentifierNameSyntax identifier
                    && identifier.Identifier.Text == "number");

        var numberInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(numberInvocationSyntax));
        Assert.True(numberInvocation.Method.IsExtensionMethod);
        Assert.True(numberInvocation.Method.IsGenericMethod);
        Assert.Equal("Identity", numberInvocation.Method.Name);
        Assert.Single(numberInvocation.Method.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, numberInvocation.Method.TypeArguments[0].SpecialType);
        Assert.NotNull(numberInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            numberInvocation.Method.Parameters[0].Type,
            numberInvocation.ExtensionReceiver!.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(
            numberInvocation.Method.ReturnType,
            numberInvocation.ExtensionReceiver!.Type));
    }

    [Fact]
    public void Lowerer_RewritesExtensionInvocation()
    {
        const string mainSource = """
import Sample.Extensions.NumberExtensions

class Query {
    Run() -> int {
        val value = 3
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
        Assert.Null(invocation.ExtensionReceiver);

        var arguments = invocation.Arguments.ToArray();
        Assert.Equal(invocation.Method.Parameters.Length, arguments.Length);
        Assert.IsType<BoundLocalAccess>(arguments[0]);
    }

    [Fact]
    public void Lowerer_RewritesSourceExtensionInvocation()
    {
        const string source = """
import System.Runtime.CompilerServices.*

class Query {
    Run() -> int {
        val value = 3
        return value.Double()
    }
}

public static class NumberExtensions {
    [ExtensionAttribute]
    public static Double(x: int) -> int {
        return x + x
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
        Assert.Null(invocation.ExtensionReceiver);

        var arguments = invocation.Arguments.ToArray();
        Assert.Equal(invocation.Method.Parameters.Length, arguments.Length);
        Assert.IsType<BoundLocalAccess>(arguments[0]);
    }

    [Fact]
    public void Lowerer_ExtensionInvocation_BoxesValueTypeReceiverWhenRequired()
    {
        const string source = """
import System.Runtime.CompilerServices.*

class Query {
    Run() -> object {
        val value = 3
        return value.ToObject()
    }
}

public static class BoxingExtensions {
    [ExtensionAttribute]
    public static ToObject(x: object) -> object {
        return x
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

        var boxedReceiver = Assert.IsType<BoundConversionExpression>(arguments[0]);
        Assert.Equal(invocation.Method.Parameters[0].Type, boxedReceiver.Type);

        var local = Assert.IsType<BoundLocalAccess>(boxedReceiver.Expression);
        Assert.Equal("value", local.Local.Name);
    }

    [Fact]
    public void Lowerer_WithLoweringTrace_RecordsNestedExtensionInvocations()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Linq.*
import System.Runtime.CompilerServices.*
import Raven.MetadataFixtures.Linq.*

class Query {
    Run() -> IEnumerable<int> {
        val numbers = [1, 2, 3, 4]
        return numbers
            .Where(value => value > 1)
            .ProjectSquares(value => value * value)
            .Select(value => value + 1)
    }
}

public static class RavenPipelineExtensions {
    [ExtensionAttribute]
    public static ProjectSquares(values: IEnumerable<int>, projector: Func<int, int>) -> IEnumerable<int> {
        return values.Select(projector)
    }
}
""";

        var traceLog = new LoweringTraceLog();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            loweringTrace: traceLog);

        var (compilation, tree) = CreateCompilation(
            source,
            options: options,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);

        var runMethodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(node => node.Identifier.Text == "Run");

        var runMethodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(runMethodSyntax)!;
        var runBody = (BoundBlockStatement)model.GetBoundNode(runMethodSyntax.Body!)!;
        Lowerer.LowerBlock(runMethodSymbol, runBody);

        var projectMethodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(node => node.Identifier.Text == "ProjectSquares");

        var projectMethodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(projectMethodSyntax)!;
        var projectBody = (BoundBlockStatement)model.GetBoundNode(projectMethodSyntax.Body!)!;
        Lowerer.LowerBlock(projectMethodSymbol, projectBody);

        var entries = traceLog.ExtensionInvocations;
        Assert.NotEmpty(entries);

        var runEntries = entries
            .Where(entry => SymbolEqualityComparer.Default.Equals(entry.ContainingSymbol, runMethodSymbol))
            .ToArray();

        Assert.True(runEntries.Length >= 3, "Expected extension invocations for Where, ProjectSquares, and Select.");

        Assert.Contains(
            runEntries,
            entry => entry.Method.Name == "Where" &&
                entry.Method.ContainingType?.Name == "Enumerable" &&
                entry.ReceiverCameFromInvocation);

        Assert.Contains(
            runEntries,
            entry => entry.Method.Name == "ProjectSquares" &&
                entry.Method.ContainingType?.Name == "RavenPipelineExtensions" &&
                entry.ReceiverCameFromInvocation);

        Assert.Contains(
            runEntries,
            entry => entry.Method.Name == "Select" &&
                entry.Method.ContainingType?.Name == "Enumerable" &&
                entry.ReceiverCameFromInvocation);

        foreach (var entry in runEntries)
        {
            Assert.Equal(entry.Method.Parameters.Length, entry.ArgumentTypes.Length);
            Assert.True(SymbolEqualityComparer.Default.Equals(entry.Method.Parameters[0].Type, entry.ArgumentTypes[0]));
        }

        var nestedEntries = entries
            .Where(entry => SymbolEqualityComparer.Default.Equals(entry.ContainingSymbol, projectMethodSymbol))
            .ToArray();

        var nestedSelect = Assert.Single(nestedEntries);
        Assert.Equal("Select", nestedSelect.Method.Name);
        Assert.Equal("Enumerable", nestedSelect.Method.ContainingType?.Name);
        Assert.True(nestedSelect.ReceiverCameFromInvocation);
        Assert.Equal(nestedSelect.Method.Parameters.Length, nestedSelect.ArgumentTypes.Length);
        Assert.True(SymbolEqualityComparer.Default.Equals(nestedSelect.Method.Parameters[0].Type, nestedSelect.ArgumentTypes[0]));
    }

    [Fact]
    public void Lowerer_WithLoweringTrace_RecordsNestedQueryPipelinesAcrossExtensionSources()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Linq.*
import System.Runtime.CompilerServices.*

class Query {
    Run() -> IEnumerable<int> {
        val numbers = [1, 2, 3, 4]
        return numbers
            .Where(value => value > 1)
            .ProjectSquaresAndFilter(value => value * value)
            .Select(value => value + 1)
    }
}

public static class RavenPipelineExtensions {
    [ExtensionAttribute]
    public static ProjectSquaresAndFilter(values: IEnumerable<int>, projector: Func<int, int>) -> IEnumerable<int> {
        return values
            .Select(projector)
            .Where(result => result.IsEven())
    }
}

public static class IntExtensions {
    [ExtensionAttribute]
    public static IsEven(value: int) -> bool {
        return value % 2 == 0
    }
}
""";

        var traceLog = new LoweringTraceLog();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            loweringTrace: traceLog);

        var (compilation, tree) = CreateCompilation(
            source,
            options: options,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);

        var methodDeclarations = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .ToArray();

        var runMethodSyntax = methodDeclarations.Single(node => node.Identifier.Text == "Run");
        var runMethodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(runMethodSyntax)!;
        var runBody = (BoundBlockStatement)model.GetBoundNode(runMethodSyntax.Body!)!;
        Lowerer.LowerBlock(runMethodSymbol, runBody);

        var projectMethodSyntax = methodDeclarations.Single(node => node.Identifier.Text == "ProjectSquaresAndFilter");
        var projectMethodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(projectMethodSyntax)!;
        var projectBody = (BoundBlockStatement)model.GetBoundNode(projectMethodSyntax.Body!)!;
        Lowerer.LowerBlock(projectMethodSymbol, projectBody);

        var entries = traceLog.ExtensionInvocations;
        Assert.True(entries.Length >= 6, "Expected extension invocations for Where/Select and ProjectSquaresAndFilter pipelines.");

        var runEntries = entries
            .Where(entry => SymbolEqualityComparer.Default.Equals(entry.ContainingSymbol, runMethodSymbol))
            .ToArray();

        Assert.Contains(
            runEntries,
            entry => entry.Method.Name == "Where" &&
                entry.Method.ContainingType?.Name == "Enumerable" &&
                !entry.ReceiverCameFromInvocation);

        Assert.Contains(
            runEntries,
            entry => entry.Method.Name == "ProjectSquaresAndFilter" &&
                entry.Method.ContainingType?.Name == "RavenPipelineExtensions" &&
                entry.ReceiverCameFromInvocation);

        Assert.Contains(
            runEntries,
            entry => entry.Method.Name == "Select" &&
                entry.Method.ContainingType?.Name == "Enumerable" &&
                entry.ReceiverCameFromInvocation);

        foreach (var entry in runEntries)
        {
            Assert.Equal(entry.Method.Parameters.Length, entry.ArgumentTypes.Length);
        }

        var projectEntries = entries
            .Where(entry => SymbolEqualityComparer.Default.Equals(entry.ContainingSymbol, projectMethodSymbol))
            .ToArray();

        Assert.Contains(
            projectEntries,
            entry => entry.Method.Name == "Select" &&
                entry.Method.ContainingType?.Name == "Enumerable" &&
                !entry.ReceiverCameFromInvocation);

        Assert.Contains(
            projectEntries,
            entry => entry.Method.Name == "Where" &&
                entry.Method.ContainingType?.Name == "Enumerable" &&
                entry.ReceiverCameFromInvocation);

        foreach (var entry in projectEntries)
        {
            Assert.Equal(entry.Method.Parameters.Length, entry.ArgumentTypes.Length);
        }

        var isEvenEntry = Assert.Single(entries.Where(entry => entry.Method.Name == "IsEven"));
        var lambdaSymbol = Assert.IsAssignableFrom<IMethodSymbol>(isEvenEntry.ContainingSymbol);
        Assert.True(SymbolEqualityComparer.Default.Equals(lambdaSymbol.ContainingSymbol, projectMethodSymbol));
        Assert.False(isEvenEntry.ReceiverCameFromInvocation);
        Assert.Equal(isEvenEntry.Method.Parameters.Length, isEvenEntry.ArgumentTypes.Length);
        Assert.Equal("IntExtensions", isEvenEntry.Method.ContainingType?.Name);
    }

    [Fact]
    public void ExtensionInvocation_WithNullableValueReceiver_RequiresNonNullableParameter()
    {
        const string source = """
import System.Runtime.CompilerServices.*

class Query {
    Run() -> int {
        val value: int? = null
        return value.Double()
    }
}

public static class NumberExtensions {
    [ExtensionAttribute]
    public static Double(x: int) -> int {
        return x + x
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext &&
                diagnostic.GetMessage().Contains("Double", StringComparison.Ordinal));
    }

    [Fact]
    public void ExtensionInvocation_WithUnsatisfiedGenericConstraint_ReportsDiagnostic()
    {
        const string source = """
import System.Runtime.CompilerServices.*

public class NonComparable { }

public static class Extensions {
    [ExtensionAttribute]
    public static RequiresComparison<T: System.IComparable>(value: T) -> int {
        return 0
    }
}

class Query {
    Run() -> int {
        val receiver = NonComparable()
        return receiver.RequiresComparison()
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
    }

    [Fact]
    public void SourceExtension_InSeparateNamespace_RequiresImport()
    {
        const string mainSource = """
val number = 4
val doubled = number.Double()
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

        var mainTree = SyntaxTree.ParseText(mainSource);
        var extensionTree = SyntaxTree.ParseText(extensionSource);

        var compilation = CreateCompilation([mainTree, extensionTree]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
    }

    [Fact]
    public void SourceExtension_InstanceMethodsTakePrecedenceOverExtensions()
    {
        const string source = """
import System.Runtime.CompilerServices.*

val holder = Container()
val description = holder.Describe()

public class Container {
    Describe() -> string {
        return "instance"
    }
}

namespace Sample.Extensions {
    public static class ContainerExtensions {
        [ExtensionAttribute]
        public static Describe(x: Container) -> string {
            return "extension"
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);

        var memberAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.Text == "Describe");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        Assert.All(methodGroup.Methods, method => Assert.True(method.IsExtensionMethod));

        var invocationSyntax = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));

        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal("ContainerExtensions", boundInvocation.Method.ContainingType?.Name);
    }

    [Fact]
    public void LambdaArgument_WithMultipleExtensionCandidates_RecordsAllDelegateTargets()
    {
        const string source = """
import System.*
import System.Runtime.CompilerServices.*

val number = 42
val result = number.Apply(value => value > 0)

namespace Sample.Extensions {
    public static class NumberExtensions {
        [ExtensionAttribute]
        public static Apply(value: int, predicate: System.Predicate<int>) -> bool {
            return predicate(value)
        }

        [ExtensionAttribute]
        public static Apply(value: int, predicate: System.Func<int, bool>) -> bool {
            return predicate(value)
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Apply");

        var argument = invocation.ArgumentList.Arguments.Single();
        var lambda = Assert.IsAssignableFrom<LambdaExpressionSyntax>(argument.Expression);
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambda));

        Assert.NotEmpty(boundLambda.CandidateDelegates);
        Assert.Contains(
            boundLambda.CandidateDelegates,
            candidate => candidate.Name == "Func" &&
                candidate.Arity == 2 &&
                candidate.TypeArguments[0] is { SpecialType: SpecialType.System_Int32 } &&
                candidate.TypeArguments[1] is { SpecialType: SpecialType.System_Boolean });
    }

    [Fact]
    public void LambdaReplayInstrumentation_WithAmbiguousExtensionCandidates_TracksRebinds()
    {
        const string source = """
import System.*
import System.Runtime.CompilerServices.*

namespace Sample.Extensions {
    public static class NumberExtensions {
        [ExtensionAttribute]
        public static Apply(value: int, predicate: System.Predicate<int>) -> bool {
            return predicate(value)
        }

        [ExtensionAttribute]
        public static Apply(value: int, predicate: System.Func<int, bool>) -> bool {
            return predicate(value)
        }
    }
}

val number = 42
val result = number.Apply(value => value > 0)
""";

        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            performanceInstrumentation: instrumentation);

        var (compilation, _) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CallIsAmbiguous);

        var counters = instrumentation.LambdaReplay;
        Assert.Equal(2, counters.ReplayAttempts);
        Assert.Equal(2, counters.ReplaySuccesses);
        Assert.Equal(0, counters.ReplayFailures);
        Assert.Equal(0, counters.CacheHits);
        Assert.Equal(2, counters.CacheMisses);
        Assert.Equal(2, counters.BindingInvocations);
        Assert.Equal(2, counters.BindingSuccesses);
        Assert.Equal(0, counters.BindingFailures);
    }

    [Fact]
    public void TopLevelAwait_WithExtensionMethodInvocation_BindsExtension()
    {
        const string source = """
import System.Runtime.CompilerServices.*
import System.Threading.Tasks.*

val value = 3
val doubled = await value.DoubleAsync()

public static class Extensions {
    [ExtensionAttribute]
    public static DoubleAsync(x: int) -> Task<int> {
        return Task.FromResult(x + x)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "DoubleAsync");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            boundInvocation.Method.Parameters[0].Type,
            boundInvocation.ExtensionReceiver!.Type));
    }

    [Fact]
    public void PipeOperator_WithExtensionMethod_BindsInvocation()
    {
        const string source = """
import System.Runtime.CompilerServices.*

val value = 10
val doubled = value |> Double()

public static class NumberExtensions {
    [ExtensionAttribute]
    public static Double(x: int) -> int {
        return x * 2
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.True(boundPipeline.Method.IsExtensionMethod);
        Assert.NotNull(boundPipeline.ExtensionReceiver);
        Assert.Empty(boundPipeline.Arguments);
    }

    [Fact]
    public void PipeOperator_WithSourceExtensionDeclaration_UnqualifiedCall_BindsInvocation()
    {
        const string source = """
extension IntExt for int {
    public Inc(amount: int) -> int {
        return self + amount
    }
}

val result = 5 |> Inc(2)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV0103" && d.GetMessage().Contains("'Inc'"));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.True(boundPipeline.Method.IsExtensionMethod);
        Assert.NotNull(boundPipeline.ExtensionReceiver);
        Assert.Single(boundPipeline.Arguments);
        Assert.Equal("Inc", boundPipeline.Method.Name);
    }

    [Fact]
    public void PipeOperator_WithSourceExtensionDeclaration_ImplicitInvocation_BindsInvocation()
    {
        const string source = """
extension IntExt for int {
    public Inc() -> int {
        return self + 1
    }
}

val result = 5 |> Inc
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV0103" && d.GetMessage().Contains("'Inc'"));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.True(boundPipeline.Method.IsExtensionMethod);
        Assert.NotNull(boundPipeline.ExtensionReceiver);
        Assert.Empty(boundPipeline.Arguments);
        Assert.Equal("Inc", boundPipeline.Method.Name);
    }

    [Fact]
    public void PipeOperator_WithStaticMethod_PrependsArgument()
    {
        const string source = """
val start = 3
val result = start |> MathHelpers.Increment(2)

public static class MathHelpers {
    public static Increment(x: int, amount: int) -> int {
        return x + amount
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.False(boundPipeline.Method.IsExtensionMethod);
        Assert.Null(boundPipeline.ExtensionReceiver);

        var arguments = boundPipeline.Arguments.ToArray();
        Assert.Equal(2, arguments.Length);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[1].Type.SpecialType);
    }

    [Fact]
    public void PipeOperator_WithStaticMethod_ImplicitInvocation_PrependsArgument()
    {
        const string source = """
val start = 3
val result = start |> MathHelpers.Increment

public static class MathHelpers {
    public static Increment(x: int) -> int {
        return x + 1
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV0103" && d.GetMessage().Contains("'Increment'"));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.False(boundPipeline.Method.IsExtensionMethod);
        Assert.Null(boundPipeline.ExtensionReceiver);
        Assert.Equal("Increment", boundPipeline.Method.Name);
        Assert.Single(boundPipeline.Arguments);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[0].Type.SpecialType);
    }

    [Fact]
    public void PipeOperator_WithOptionalParameter_SupportsImplicitAndExplicitInvocationForms()
    {
        const string source = """
func Inc(x: int, n: int = 1) -> int {
    return x + n
}

val a = 5 |> Inc
val b = 5 |> Inc()
val c = 5 |> Inc(2)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV0103" && d.GetMessage().Contains("'Inc'"));

        var model = compilation.GetSemanticModel(tree);
        var pipelines = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Where(node => node.OperatorToken.Kind == SyntaxKind.PipeToken)
            .ToArray();

        Assert.Equal(3, pipelines.Length);

        var boundA = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipelines[0]));
        var boundB = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipelines[1]));
        var boundC = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipelines[2]));

        Assert.Equal("Inc", boundA.Method.Name);
        Assert.Equal("Inc", boundB.Method.Name);
        Assert.Equal("Inc", boundC.Method.Name);

        var argsA = boundA.Arguments.ToArray();
        var argsB = boundB.Arguments.ToArray();
        var argsC = boundC.Arguments.ToArray();

        Assert.Equal(2, argsA.Length);
        Assert.Equal(2, argsB.Length);
        Assert.Equal(2, argsC.Length);

        var defaultArgA = Assert.IsType<BoundLiteralExpression>(argsA[1]);
        var defaultArgB = Assert.IsType<BoundLiteralExpression>(argsB[1]);
        var explicitArgC = Assert.IsType<BoundLiteralExpression>(argsC[1]);

        Assert.Equal(1, defaultArgA.Value);
        Assert.Equal(1, defaultArgB.Value);
        Assert.Equal(2, explicitArgC.Value);
    }

    [Fact]
    public void PipeOperator_WithStaticImport_ResolvesImportedMember()
    {
        const string source = """
import System.Math.*

val value = -5
val result = value |> Abs()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.False(boundPipeline.Method.IsExtensionMethod);
        Assert.Null(boundPipeline.ExtensionReceiver);
        Assert.Equal("Abs", boundPipeline.Method.Name);
        Assert.Equal("Math", boundPipeline.Method.ContainingType?.Name);

        var arguments = boundPipeline.Arguments.ToArray();
        Assert.Single(arguments);
        Assert.Equal(SpecialType.System_Int32, arguments[0].Type.SpecialType);
    }

    [Fact]
    public void PipeOperator_WithStaticImportFromSourceType_ResolvesImportedMember()
    {
        const string source = """
import MathHelpers.*

val value = 5
val result = value |> Increment(2)

public static class MathHelpers {
    public static Increment(x: int, amount: int) -> int {
        return x + amount
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.False(boundPipeline.Method.IsExtensionMethod);
        Assert.Null(boundPipeline.ExtensionReceiver);
        Assert.Equal("Increment", boundPipeline.Method.Name);

        var arguments = boundPipeline.Arguments.ToArray();
        Assert.Equal(2, arguments.Length);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[1].Type.SpecialType);
    }

    [Fact]
    public void PipeOperator_WithFunction_ResolvesTopLevelFunction()
    {
        const string source = """
val start = 5
val result = start |> Increment(2)

func Increment(x: int, amount: int) -> int {
    return x + amount
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.False(boundPipeline.Method.IsExtensionMethod);
        Assert.Null(boundPipeline.ExtensionReceiver);
        Assert.Equal("Increment", boundPipeline.Method.Name);
        Assert.True(boundPipeline.Method.IsStatic);

        var arguments = boundPipeline.Arguments.ToArray();
        Assert.Equal(2, arguments.Length);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[1].Type.SpecialType);
    }

    [Fact]
    public void PipeOperator_WithLambdaLocal_InvokesDelegate()
    {
        const string source = """
val increment = (x: int, amount: int) -> int => x + amount
val result = 5 |> increment(2)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.Equal("Invoke", boundPipeline.Method.Name);

        var arguments = boundPipeline.Arguments.ToArray();
        Assert.Equal(2, arguments.Length);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[1].Type.SpecialType);
    }

    [Fact]
    public void PipeOperator_WithGenericStaticMethod_InfersTypeArgumentFromPipeline()
    {
        const string source = """
val result = 5 |> MathHelpers.Increment(2)

public static class MathHelpers {
    public static Increment<T>(x: T, amount: int) -> int {
        return 42 + amount
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
        Assert.False(boundPipeline.Method.IsExtensionMethod);
        Assert.Null(boundPipeline.ExtensionReceiver);
        Assert.Equal("Increment", boundPipeline.Method.Name);

        var typeArguments = boundPipeline.Method.TypeArguments;
        Assert.Single(typeArguments);
        Assert.Equal(SpecialType.System_Int32, typeArguments[0].SpecialType);

        var arguments = boundPipeline.Arguments.ToArray();
        Assert.Equal(2, arguments.Length);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, boundPipeline.Method.Parameters[1].Type.SpecialType);
    }

    [Fact]
    public void PipeOperator_WithInstanceProperty_AssignsThroughSetter()
    {
        const string source = """
val holder = Container()
val assigned = 5 |> holder.Value

public class Container {
    public Value: int { get; set; }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundPropertyAssignmentExpression>(model.GetBoundNode(pipeline));
        Assert.False(boundPipeline.Property.IsStatic);
        Assert.Equal("Value", boundPipeline.Property.Name);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundPipeline.Property.Type, intType));
        Assert.True(SymbolEqualityComparer.Default.Equals(boundPipeline.Type, intType));
        Assert.IsType<BoundLiteralExpression>(boundPipeline.Right);
    }

    [Fact]
    public void PipeOperator_WithStaticProperty_AssignsThroughSetter()
    {
        const string source = """
val assigned = 5 |> Container.Count

public class Container {
    public static Count: int { get; set; }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var pipeline = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(node => node.OperatorToken.Kind == SyntaxKind.PipeToken);

        var boundPipeline = Assert.IsType<BoundPropertyAssignmentExpression>(model.GetBoundNode(pipeline));
        Assert.True(boundPipeline.Property.IsStatic);
        Assert.Equal("Count", boundPipeline.Property.Name);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundPipeline.Property.Type, intType));
        Assert.True(
            SymbolEqualityComparer.Default.Equals(boundPipeline.Type, intType) ||
            SymbolEqualityComparer.Default.Equals(boundPipeline.Type, unitType));
        Assert.IsType<BoundLiteralExpression>(boundPipeline.Right);
    }

    [Fact]
    public void PipeOperator_WithNonInvocationTarget_ReportsDiagnostic()
    {
        const string source = """
val value = 10
val result = value |> 5
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV2800", diagnostic.Descriptor.Id);
    }

    [Fact]
    public void MemberAccess_WithNestedOptionExtension_Flatten_BindsWithoutComparerRecursion()
    {
        const string source = """
public union Option<T> {
    Some(value: T)
    None
}

public extension OptionExtensionsNested<T> for Option<Option<T>> {
    public Flatten() -> Option<T> {
        return self match {
            .Some(val inner) => inner
            .None => .None
        }
    }
}

class Container {
    Test() -> Option<int> {
        val nested: Option<Option<int>> = .Some(.Some(42))
        return nested.Flatten()
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
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Flatten");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.Equal("Flatten", boundInvocation.Method.Name);
        Assert.Equal("OptionExtensionsNested", boundInvocation.Method.ContainingType?.Name);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
    }

    [Fact]
    public void MemberAccess_WithCompetingGenericExtensionMaps_InfersLambdaFromReceiver()
    {
        const string source = """
public union Option<T> {
    Some(value: T)
    None
}

public union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

extension OptionExtensions<T> for Option<T> {
    Map<TResult>(mapper: T -> TResult) -> Option<TResult> {
        return self match {
            .Some(val value) => .Some(mapper(value))
            .None => .None
        }
    }
}

extension ResultExtensions<T, E> for Result<T, E> {
    Map<TResult>(mapper: T -> TResult) -> Result<TResult, E> {
        return self match {
            .Ok(val value) => .Ok(mapper(value))
            .Error(val error) => .Error(error)
        }
    }
}

class Container {
    Test() -> Option<int> {
        val option: Option<int> = .Some(42)
        return option.Map(x => x * 2)
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
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Map");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.Equal("Map", boundInvocation.Method.Name);
        Assert.Equal("OptionExtensions", boundInvocation.Method.ContainingType?.Name);
        Assert.True(boundInvocation.Method.IsGenericMethod);
        Assert.Contains(boundInvocation.Method.TypeArguments, t => t.SpecialType == SpecialType.System_Int32);
    }

    [Fact]
    public void MemberAccess_WithCompetingGenericExtensionMaps_InfersLambdaReturnFromConstantBody()
    {
        const string source = """
public union Option<T> {
    Some(value: T)
    None
}

public union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

extension OptionExtensions<T> for Option<T> {
    Map<TResult>(mapper: T -> TResult) -> Option<TResult> {
        return self match {
            .Some(val value) => .Some(mapper(value))
            .None => .None
        }
    }
}

extension ResultExtensions<T, E> for Result<T, E> {
    Map<TResult>(mapper: T -> TResult) -> Result<TResult, E> {
        return self match {
            .Ok(val value) => .Ok(mapper(value))
            .Error(val error) => .Error(error)
        }
    }
}

class Container {
    Test() -> Option<int> {
        val option: Option<int> = .Some(42)
        return option.Map(x => 42)
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
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Map");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.Equal("OptionExtensions", boundInvocation.Method.ContainingType?.Name);
        Assert.Contains(boundInvocation.Method.TypeArguments, t => t.SpecialType == SpecialType.System_Int32);
    }

    [Fact]
    public void MemberAccess_WithCompetingGenericExtensionMaps_AllowsExplicitLambdaParameterType()
    {
        const string source = """
public union Option<T> {
    Some(value: T)
    None
}

public union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

extension OptionExtensions<T> for Option<T> {
    Map<TResult>(mapper: T -> TResult) -> Option<TResult> {
        return self match {
            .Some(val value) => .Some(mapper(value))
            .None => .None
        }
    }
}

extension ResultExtensions<T, E> for Result<T, E> {
    Map<TResult>(mapper: T -> TResult) -> Result<TResult, E> {
        return self match {
            .Ok(val value) => .Ok(mapper(value))
            .Error(val error) => .Error(error)
        }
    }
}

class Container {
    Test() -> Option<int> {
        val option: Option<int> = .Some(42)
        return option.Map((x: int) -> int => x * 2)
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
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Map");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.Equal("OptionExtensions", boundInvocation.Method.ContainingType?.Name);
        Assert.Contains(boundInvocation.Method.TypeArguments, t => t.SpecialType == SpecialType.System_Int32);
    }
}
