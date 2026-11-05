using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExplicitConstructorTypeArgumentTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("Box<byte>", true)]
    [InlineData("Box<long>", false)]
    [InlineData("Box", false)]
    public void DotNet_ConstructorTypeArgumentsRemainAuthoritative(string constructor, bool rejected)
        => Verify(TargetPlatform.DotNet, constructor, rejected);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("Box<byte>", true)]
    [InlineData("Box<long>", false)]
    [InlineData("Box", false)]
    public void NeoClr_ConstructorTypeArgumentsRemainAuthoritative(string constructor, bool rejected)
        => Verify(TargetPlatform.NeoCLR, constructor, rejected);

    private static void Verify(TargetPlatform target, string constructor, bool rejected)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Box<T> {
                init(value: T) {}
            }
            class Probe {
                static func Make(value: long) {
                    let box = {{constructor}}(value)
                }
            }
            """);
        var compilation = Compilation.Create("ExplicitConstructorTypeArguments", [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var errors = compilation.GetSemanticModel(tree).GetDiagnostics()
            .Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ToArray();
        if (rejected)
            Assert.Equal("RAV1501", Assert.Single(errors).Id);
        else
            Assert.Empty(errors);
    }
}
