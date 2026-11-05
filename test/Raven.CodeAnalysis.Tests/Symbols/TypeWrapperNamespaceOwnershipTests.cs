using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class TypeWrapperNamespaceOwnershipTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void WrappingTypeDoesNotAddNamespaceDeclarations(bool useSourceType, bool nullable)
    {
        var compilation = Compilation.Create("WrapperOwnership",
            [SyntaxTree.ParseText("namespace Shared { public class Value { } }")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var element = compilation.GetTypeByMetadataName(useSourceType ? "Shared.Value" : "System.String")!;
        var owner = element.ContainingNamespace!;
        var before = owner.GetMembers();

        TypeSymbol wrapper = nullable
            ? new NullableTypeSymbol(element, element.ContainingSymbol, element.ContainingType, owner, [])
            : new RefTypeSymbol(element);

        Assert.Same(owner, wrapper.ContainingNamespace);
        Assert.DoesNotContain(owner.GetMembers(), member => ReferenceEquals(member, wrapper));
        Assert.Equal(before.Length, owner.GetMembers().Length);
        Assert.DoesNotContain(owner.GetMembers(wrapper.Name), member => ReferenceEquals(member, wrapper));
    }
}
