using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExtensionMemberLookupApiTests : CompilationTestBase
{
    [Fact]
    public void LookupApplicableExtensionMembers_ReturnsAllExtensionMemberKinds()
    {
        const string source = """
class Counter { }

extension CounterExtensions for Counter {
    func Increment() -> int {
        return 1
    }

    val Total: int {
        get { return 42; }
    }

    static func Create() -> Counter {
        return Counter()
    }

    static val Name: string {
        get { return "Counter"; }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var counterDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var counterType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(counterDeclaration));

        var result = model.LookupApplicableExtensionMembers(counterType);

        Assert.Contains(result.InstanceMethods, m => m.Name == "Increment");
        Assert.Contains(result.InstanceProperties, p => p.Name == "Total");
        Assert.Contains(result.StaticMethods, m => m.Name == "Create");
        Assert.Contains(result.StaticProperties, p => p.Name == "Name");
    }

    [Fact]
    public void LookupApplicableExtensionMembers_UsesContextNodeToIncludeImports()
    {
        const string extensionSource = """
import System.Runtime.CompilerServices.*

namespace Sample.Extensions {
    static class NumberExtensions {
        [ExtensionAttribute]
        static func Double(x: int) -> int {
            return x + x
        }
    }
}
""";

        const string mainSource = """
import Sample.Extensions.*

val value = 5
val doubled = value.Double()
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
            .Concat([new CompilationReference(extensionCompilation)])
            .ToArray();

        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(
            mainTree,
            new CompilationOptions(OutputKind.ConsoleApplication),
            references,
            assemblyName: "Main");

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(mainTree);
        var invocation = mainTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var result = model.LookupApplicableExtensionMembers(intType, invocation, name: "Double");

        var extensionMethod = Assert.Single(result.InstanceMethods);
        Assert.Equal("Double", extensionMethod.Name);
        Assert.Equal("NumberExtensions", extensionMethod.ContainingType?.Name);
    }

    [Fact]
    public void LookupApplicableExtensionMembers_ExcludesCandidatesWhoseReceiverConstraintsDoNotMatch()
    {
        const string source = """
interface ITagged { }

class Sample { }

extension TaggedExtensions<T: ITagged> for T {
    func Mark() -> int => 1
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var sampleDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(d => d.Identifier.ValueText == "Sample");
        var sampleType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(sampleDeclaration));

        var result = model.LookupApplicableExtensionMembers(sampleType, name: "Mark");

        Assert.DoesNotContain(result.InstanceMethods, m => m.Name == "Mark");
    }

    [Fact]
    public void LookupApplicableExtensionMembers_DeduplicatesCurrentNamespaceImportExtensionsForTypeParameters()
    {
        var helperTree = SyntaxTree.ParseText("""
namespace System
import System.*

interface IError {
    val Message: string
}

record ContextError<TError: IError>(
    val Message: string,
    val InnerError: TError
) : IError

extension ErrorExtensions<TError: IError> for TError {
    func WithMessage(message: string) -> ContextError<TError> {
        return ContextError<TError>(message, self)
    }
}
""");

        var consumerTree = SyntaxTree.ParseText("""
namespace System
import System.*

record MyError(val Message: string) : IError

class Helpers {
    static func Wrap<E: IError>(error: E, message: string) -> ContextError<E> {
        return error.WithMessage(message)
    }
}
""");

        var compilation = CreateCompilation(
            [helperTree, consumerTree],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(consumerTree);
        var wrapDeclaration = consumerTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var wrapMethod = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(wrapDeclaration));
        var invocation = consumerTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();

        var result = model.LookupApplicableExtensionMembers(
            wrapMethod.TypeParameters.Single(),
            invocation,
            name: "WithMessage",
            kinds: ExtensionMemberKinds.InstanceMethods);

        var method = Assert.Single(result.InstanceMethods);
        Assert.Equal("WithMessage", method.Name);
    }
}
