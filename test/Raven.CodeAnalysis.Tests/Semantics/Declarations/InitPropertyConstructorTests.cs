using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class InitPropertyConstructorTests
{
    [Theory]
    [InlineData("open class Parent<T> { val Value: T { get; init; } }\nclass Child : Parent<int> { init() { Value = 42 } }")]
    [InlineData("class Child { val Value: int { get; init; }\nval Other: int { get => Value; init { Value = value } } }")]
    [InlineData("class Child { val Value: int { get; init; }\ninit { Value = 42 } }")]
    public void InitializationContexts_AssignInitProperties(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("InitPropertyContexts", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("Name = value", false)]
    [InlineData("self.Name = value", false)]
    [InlineData("Name = value", true)]
    [InlineData("self.Name = value", true)]
    [InlineData("base.Name = value", true)]
    public void Constructor_AssignsInitPropertyOnSelf(string assignment, bool inherited)
    {
        var source = inherited
            ? $$"""
              open class Parent { val Name: string { get; init; } }
              class Child : Parent { init(value: string) { {{assignment}} } }
              """
            : $$"""
              class Child {
                  val Name: string { get; init; }
                  init(value: string) { {{assignment}} }
              }
              """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("InitPropertyConstructor", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("init(other: Child) { other.Name = 1 }")]
    [InlineData("func Change() { Name = 1 }")]
    [InlineData("static init() { Child().Name = 1 }")]
    [InlineData("init() { let action: System.Action = () => { Name = 1 } }")]
    [InlineData("init() { func Change() { Name = 1 } }")]
    public void AssignmentOutsideSelfConstruction_IsRejected(string member)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Child {
                val Name: int { get; init; }
                {{member}}
            }
            """);
        var compilation = Compilation.Create("InvalidInitPropertyAssignment", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var error = Assert.Single(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal("RAV0200", error.Id);
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrInitPropertyConstructorTests
{
    [Theory]
    [InlineData("Value = value", false)]
    [InlineData("self.Value = value", false)]
    [InlineData("other.Value = value", true)]
    public void NeoClrSemanticConstructorBinding_UsesSharedInitAssignmentRules(string assignment, bool rejected)
    {
        var tree = SyntaxTree.ParseText($$"""
            open class Parent { val Value: int { get; init; } }
            class Child : Parent { init(other: Child, value: int) { {{assignment}} } }
            """);
        var compilation = Compilation.Create("NeoClrInitConstructor", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        // Host references exercise shared semantic binding, not a native core or emission configuration.
        var errors = compilation.GetSemanticModel(tree).GetDiagnostics()
            .Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (rejected)
            Assert.Equal("RAV0200", Assert.Single(errors).Id);
        else
            Assert.Empty(errors);
    }
}
