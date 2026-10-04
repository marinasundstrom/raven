using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ConstructorChainingTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void DerivedConstructorPreservesBaseAndFieldInitialization(bool derivedFirst, bool separateTrees)
    {
        var derived = """
            public class Derived : Base {
                private var extra: int = 2
                init(number: int): base(number) {}
                func Read() -> int { return Number + extra }
            }
            """;
        var parent = """
            public open class Base {
                field Number: int
                init(number: int) { self.Number = number }
            }
            """;
        var sources = derivedFirst ? new[] { derived, parent } : new[] { parent, derived };
        var trees = separateTrees ? sources.Select(source => SyntaxTree.ParseText(source)).ToArray() : new[] { SyntaxTree.ParseText(string.Join("\n", sources)) };
        var compilation = Compilation.Create("ConstructorOrder" + Guid.NewGuid().ToString("N"), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(trees)
            .AddReferences(TestMetadataReferences.Default);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        foreach (var tree in trees)
        {
            var model = compilation.GetSemanticModel(tree);
            foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>())
            {
                var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(syntax));
                Assert.Same(method, method.ContainingType!.Constructors.Single(candidate => candidate.Parameters.Length == method.Parameters.Length));
            }
        }
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var type = Assembly.Load(output.ToArray()).GetType("Derived")!;
        var value = Activator.CreateInstance(type, 40)!;
        Assert.Equal(42, type.GetMethod("Read")!.Invoke(value, null));
    }

    [Fact]
    public void InvalidForwardBaseInitializerPreventsPublication()
    {
        var compilation = Compilation.Create("InvalidConstructorOrder", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText("class Derived : Base { init(): base(1) {} }\nopen class Base { init() {} }"))
            .AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error && d.Id == "RAV1501");
        using var output = new MemoryStream();
        Assert.False(compilation.Emit(output).Success);
        Assert.Equal(0, output.Length);
    }
}
