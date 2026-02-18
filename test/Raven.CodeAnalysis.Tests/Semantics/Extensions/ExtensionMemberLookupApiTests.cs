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
    Increment() -> int {
        return 1
    }

    Total: int {
        get { return 42; }
    }

    public static Create() -> Counter {
        return Counter()
    }

    public static Name: string {
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
    public static class NumberExtensions {
        [ExtensionAttribute]
        public static Double(x: int) -> int {
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
}
