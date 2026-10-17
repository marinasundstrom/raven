namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SourceMetadataNameTests : CompilationTestBase
{
    [Theory]
    [InlineData("class Box<T> {}\nclass Box {}", "Box")]
    [InlineData("class Owner { class Box<T> {}\nclass Box {} }", "Owner+Box")]
    public void NongenericLookupDoesNotSelectGenericDeclaration(string source, string name)
    {
        var (compilation, _) = CreateCompilation(source);
        var plain = compilation.GetTypeByMetadataName(name);
        var generic = compilation.GetTypeByMetadataName(name + "`1");
        Assert.NotNull(plain);
        Assert.NotNull(generic);
        Assert.Equal(0, plain!.Arity);
        Assert.Equal(1, generic!.Arity);
        Assert.NotSame(plain, generic);
    }

    [Theory]
    [InlineData("class Box<T> {}", "Box")]
    [InlineData("class Owner { class Box<T> {} }", "Owner+Box")]
    public void NongenericLookupDoesNotFallBackToGenericDeclaration(string source, string name)
    {
        var (compilation, _) = CreateCompilation(source);
        Assert.Null(compilation.GetTypeByMetadataName(name));
        Assert.NotNull(compilation.GetTypeByMetadataName(name + "`1"));
    }
    [Fact]
    public void QualifiedGenericUnionCaseStillInfersArguments()
    {
        var (compilation, _) = CreateCompilation("""
            namespace Example
            union Maybe<T> {
                case Some(value: T)
                case None
            }
            class Consumer {
                static func Empty() -> Maybe<int> => Example.Maybe.None()
                static func Present() -> Maybe<int> => Example.Maybe.Some(42)
                static func IsEmpty(value: Maybe<int>) -> bool {
                    if let Example.Maybe.None = value { return true }
                    return false
                }
            }
            """, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Null(compilation.GetTypeByMetadataName("Example.Maybe"));
        Assert.NotNull(compilation.GetTypeByMetadataName("Example.Maybe`1"));
        var tree = compilation.SyntaxTrees.Single();
        var statement = tree.GetRoot().DescendantNodes().OfType<Raven.CodeAnalysis.Syntax.IfPatternStatementSyntax>().Single();
        var bound = Assert.IsType<BoundIfStatement>(compilation.GetSemanticModel(tree).GetBoundNode(statement));
        var condition = Assert.IsType<BoundIsPatternExpression>(bound.Condition);
        var pattern = Assert.IsType<BoundDeclarationPattern>(condition.Pattern);
        Assert.NotEqual(TypeKind.Error, pattern.DeclaredType.TypeKind);
        using var output = new System.IO.MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
    }
}
