using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionTypeDiagnosticsTests : CompilationTestBase
{
    [Theory]
    [InlineData("func Accept(value: A & B) {}")]
    [InlineData("class Holder { field value: A & B }")]
    [InlineData("func Accept(value: System.Collections.Generic.List<A & B>) {}")]
    [InlineData("class Box<T: System.Collections.Generic.List<A & B>> {}")]
    [InlineData("class Box<T: (A & B)[]> {}")]
    [InlineData("class Box<T: (A & B)?> {}")]
    public void UnsupportedStoragePosition_ReportsIntersectionDiagnostic(string declaration)
    {
        var (compilation, tree) = CreateCompilation("interface A {}\ninterface B {}\n" + declaration);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<IntersectionTypeSyntax>())
        {
            var type = model.GetTypeInfo(syntax).Type;
            Assert.True(type is null || type.TypeKind == TypeKind.Error);
        }
        Assert.Contains(compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.IntersectionTypeNotSupported);
    }
}
