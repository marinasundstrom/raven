using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ExplicitInterfacePropertyOverrideTests
{
    [Theory]
    [Trait("Target", "Shared")]
    [InlineData(false)]
    [InlineData(true)]
    public void OverrideSelectsVirtualPropertyAlongsideExplicitImplementation(bool explicitFirst)
    {
        const string explicitMember = "val Shape.Flag: bool => Flag";
        const string virtualMember = "internal virtual val Flag: bool => false";
        var members = explicitFirst ? $"{explicitMember}\n{virtualMember}" : $"{virtualMember}\n{explicitMember}";
        var tree = SyntaxTree.ParseText($$"""
            interface Shape { val Flag: bool { get; } }
            open class Base : Shape {
                {{members}}
            }
            class Derived : Base {
                internal override val Flag: bool => true
            }
            """);
        var compilation = Compilation.Create("ExplicitPropertyOverride", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var derived = compilation.GetTypeByMetadataName("Derived")!;
        var property = Assert.IsAssignableFrom<PropertySymbol>(Assert.Single(derived.GetMembers("Flag")));
        var getter = Assert.IsType<SourceMethodSymbol>(property.GetMethod);
        Assert.True(getter.IsOverride);
        Assert.NotNull(getter.OverriddenMethod);
        Assert.Empty(getter.OverriddenMethod.ExplicitInterfaceImplementations);
        Assert.Equal(Accessibility.Internal, getter.OverriddenMethod.DeclaredAccessibility);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void ExplicitImplementationAloneDoesNotProvideOverrideTarget()
    {
        var tree = SyntaxTree.ParseText("""
            interface Shape { val Flag: bool { get; } }
            open class Base : Shape { val Shape.Flag: bool => false }
            class Derived : Base { override val Flag: bool => true }
            """);
        var compilation = Compilation.Create("ExplicitPropertyOnly", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0307");
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void InterfaceDispatchUsesClassVirtualOverride()
    {
        var tree = SyntaxTree.ParseText("""
            interface Shape { val Flag: bool { get; } }
            open class Base : Shape {
                val Shape.Flag: bool => Flag
                internal virtual val Flag: bool => false
            }
            class Derived : Base {
                internal override val Flag: bool => true
                static func Run() -> bool {
                    let shape: Shape = Derived()
                    return shape.Flag
                }
            }
            """);
        var compilation = Compilation.Create("ExplicitPropertyDispatch", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        Assert.Equal(true, loaded.Assembly.GetType("Derived")!.GetMethod("Run")!.Invoke(null, null));
    }
}
