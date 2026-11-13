using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class ConstructedSourceDeclarationContractTests
{
    [Fact]
    public void UnionAndCaseDeclarationsKeepMutableMemberShape()
    {
        var tree = SyntaxTree.ParseText("class Plain {}\nunion Result<T> { case Value(value: T) }");
        var compilation = Compilation.Create("SourceShape", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var ordinary = Assert.IsAssignableFrom<SourceNamedTypeIdentity>(compilation.GetTypeByMetadataName("Plain"));
        var union = Assert.IsAssignableFrom<SourceNamedTypeIdentity>(compilation.GetTypeByMetadataName("Result`1"));
        Assert.False(ordinary.HasMutableMemberShape);
        Assert.True(union.HasMutableMemberShape);
        var cases = Assert.IsAssignableFrom<UnionSymbol>(union).DeclaredCaseTypes;
        Assert.NotEmpty(cases);
        Assert.All(cases, item => Assert.True(Assert.IsAssignableFrom<SourceNamedTypeIdentity>(item).HasMutableMemberShape));
    }
}
