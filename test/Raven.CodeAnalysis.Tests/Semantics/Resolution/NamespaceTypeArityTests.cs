using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class NamespaceTypeArityTests
{
    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("Value<T>", "Value", "Value")]
    [InlineData("Value<T, U>", "Value<T>", "Value<int>")]
    public void DifferentArityInCurrentNamespaceDoesNotHideImportedType(string local, string imported, string use)
    {
        var compilation = Compilation.Create("ImportedArity", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(
                SyntaxTree.ParseText($"namespace Definitions\npublic class {imported} {{}}"),
                SyntaxTree.ParseText($"namespace Consumer\nimport Definitions.*\npublic class {local} {{}}\npublic class Use {{ func Echo(value: {use}) -> {use} => value }}"));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var owner = compilation.GetTypeByMetadataName("Consumer.Use")!;
        var method = Assert.IsAssignableFrom<MethodSymbol>(Assert.Single(owner.GetMembers("Echo")));
        Assert.Equal("Definitions", method.Parameters[0].Type.ContainingNamespace!.Name);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void SameArityLocalTypeStillShadowsImportedType()
    {
        var compilation = Create("public class Value {}", "import Definitions.*");
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var method = Assert.IsAssignableFrom<MethodSymbol>(Assert.Single(compilation.GetTypeByMetadataName("Consumer.Use")!.GetMembers("Echo")));
        Assert.Equal("Consumer", method.Parameters[0].Type.ContainingNamespace!.Name);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void MissingMatchingImportRetainsArityDiagnostic()
    {
        var compilation = Create("public class Value<T> {}", "");
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0305");
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void MultipleMatchingImportsRemainAmbiguous()
    {
        var compilation = Create("public class Value<T> {}", "import Definitions.*\nimport Other.*")
            .AddSyntaxTrees(SyntaxTree.ParseText("namespace Other\npublic class Value {}"));
        var errors = compilation.GetDiagnostics().Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.NotEmpty(errors);
        Assert.Contains(errors, diagnostic => diagnostic.Id == "RAV0121");
        Assert.DoesNotContain(errors, diagnostic => diagnostic.Id == "RAV0305");
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void ImportedZeroArityTypeWorksInGenericCallsConstructorsAndStaticReceivers()
    {
        var compilation = Compilation.Create("ImportedArityExpressions", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(
                SyntaxTree.ParseText("namespace Definitions\npublic class Value { public class Missing {}\n static func Answer() -> int => 42 }"),
                SyntaxTree.ParseText("""
                    namespace Consumer
                    import Definitions.*
                    public class Value<T> {}
                    public class Use {
                        static func Identity<T>(value: T) -> T => value
                        static func Build() -> Value => Identity<Value>(Value())
                        static func Nested() -> Value.Missing => Value.Missing()
                        static func Number() -> int => Value.Answer()
                    }
                    """));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        var type = loaded.Assembly.GetType("Consumer.Use")!;
        Assert.Equal("Definitions.Value", type.GetMethod("Build")!.Invoke(null, null)!.GetType().FullName);
        Assert.Equal("Definitions.Value+Missing", type.GetMethod("Nested")!.Invoke(null, null)!.GetType().FullName);
        Assert.Equal(42, type.GetMethod("Number")!.Invoke(null, null));
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData(false)]
    [InlineData(true)]
    public void SourceImportWithBootstrapMetadataRetainsMatchingArity(bool includeBootstrap)
    {
        var compilation = Compilation.Create("SourceImportArity", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithFrameworkProjectionMode(FrameworkProjectionMode.None))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(
                SyntaxTree.ParseText("namespace Raven.CodeAnalysis.Syntax\npublic abstract class ExpressionSyntax {}"),
                SyntaxTree.ParseText("namespace Raven.CodeAnalysis.Macros\npublic record class ExpressionSyntax<T> {}"),
                SyntaxTree.ParseText("""
                    namespace Raven.CodeAnalysis.Macros
                    import Raven.CodeAnalysis.Syntax.*
                    public class Use {
                        func Echo(value: ExpressionSyntax?) -> ExpressionSyntax? => value
                    }
                    """));
        if (includeBootstrap)
            compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(Compilation).Assembly.Location));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.True(errors.Length == 0, string.Join(Environment.NewLine, errors.Select(d => d.ToString())));
        var method = Assert.IsAssignableFrom<MethodSymbol>(Assert.Single(compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Macros.Use")!.GetMembers("Echo")));
        Assert.True(method.Parameters[0].Type.TryGetNullableUnderlyingType(out var underlying));
        Assert.Same(compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.ExpressionSyntax"), underlying);
        Assert.Same(compilation.Assembly, underlying!.ContainingAssembly);
    }

    private static Compilation Create(string localDeclaration, string imports)
        => Compilation.Create("ImportedArity", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(
                SyntaxTree.ParseText("namespace Definitions\npublic class Value {}"),
                SyntaxTree.ParseText($"namespace Consumer\n{imports}\n{localDeclaration}\npublic class Use {{ func Echo(value: Value) -> Value => value }}"));
}
