using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public class NullableReferenceConstraintTests
{
    [Theory]
    [InlineData("Node?", false, "Node")]
    [InlineData("Derived?", false, "Node")]
    [InlineData("Other?", true, "Node")]
    [InlineData("int?", true, "Node")]
    [InlineData("Node?", true, "notnull, Node")]
    [InlineData("Other?", true, "Node?")]
    [InlineData("Derived?", false, "Node?")]
    public void ReferenceAnnotationsPreserveNominalConstraintMembership(string argument, bool error, string constraint)
    {
        var source = $$"""
            open class Node { }
            class Derived : Node { }
            class Other { }
            class Holder {
                func Slot<T>() -> T where T: {{constraint}} { throw System.Exception() }
                func Optional() -> {{argument}} => Slot<{{argument}}>()
            }
            """;
        var compilation = Compilation.Create("NullableReferenceConstraint", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (error)
            Assert.Contains(errors, d => d.Id == "RAV0320");
        else
            Assert.Empty(errors);
    }
}
