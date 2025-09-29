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
    public void MemberAccess_LinqWhereOnCollectionExpression_BindsSuccessfully()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Linq.*

let numbers = [1, 2, 3]
let result = numbers.Where(func (value) => value == 2)
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

let numbers = [1, 2, 3]
let result = numbers.Where(value => value == 2)
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

let numbers = [1, 2, 3]
let result = numbers.Where(func (value: int) -> bool => value == 2)
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
        let value = 3
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
        let value = 3
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

        var boxedReceiver = Assert.IsType<BoundCastExpression>(arguments[0]);
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
        let numbers = [1, 2, 3, 4]
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
    public void ExtensionInvocation_WithNullableValueReceiver_RequiresNonNullableParameter()
    {
        const string source = """
import System.Runtime.CompilerServices.*

class Query {
    Run() -> int {
        let value: int? = null
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

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, diagnostic.Descriptor);
        Assert.Contains("Double", diagnostic.GetMessage(), StringComparison.Ordinal);
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

let receiver = NonComparable()
let result = receiver.RequiresComparison()
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
let number = 4
let doubled = number.Double()
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

let holder = Container()
let description = holder.Describe()

class Container {
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
        Assert.Contains(methodGroup.Methods, method => !method.IsExtensionMethod);
        Assert.Contains(methodGroup.Methods, method => method.IsExtensionMethod);

        var invocationSyntax = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));

        Assert.False(boundInvocation.Method.IsExtensionMethod);
        Assert.Null(boundInvocation.ExtensionReceiver);
        Assert.Equal("Container", boundInvocation.Method.ContainingType?.Name);
    }

    [Fact]
    public void LambdaArgument_WithMultipleExtensionCandidates_RecordsAllDelegateTargets()
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

let number = 42
let result = number.Apply(value => value > 0)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CallIsAmbiguous);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var argument = invocation.ArgumentList.Arguments.Single();
        var lambda = Assert.IsAssignableFrom<LambdaExpressionSyntax>(argument.Expression);
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambda));

        Assert.Equal(2, boundLambda.CandidateDelegates.Length);
        Assert.Contains(
            boundLambda.CandidateDelegates,
            candidate => candidate.Name == "Predicate" &&
                candidate.Arity == 1 &&
                candidate.TypeArguments[0] is { SpecialType: SpecialType.System_Int32 });
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

let number = 42
let result = number.Apply(value => value > 0)
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
}
