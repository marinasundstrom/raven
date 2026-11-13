using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

[Trait("Target", "Shared")]
public sealed class ExplicitPropertyIdentityTests : CompilationTestBase
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void OrdinaryAndExplicitPropertiesWithSameNameRemainDistinct(bool explicitFirst)
    {
        const string ordinary = "internal val Value: int => 1";
        const string explicitProperty = "val IValue.Value: int => Value";
        var tree = SyntaxTree.ParseText($$"""
            interface IValue { val Value: int }
            class C : IValue {
                {{(explicitFirst ? explicitProperty : ordinary)}}
                {{(explicitFirst ? ordinary : explicitProperty)}}
            }
            """);
        var compilation = CreateCompilation([tree], options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var type = compilation.GetTypeByMetadataName("C")!;
        var properties = type.GetMembers("Value").OfType<PropertySymbol>().ToArray();
        Assert.Equal(2, properties.Length);
        Assert.Equal(2, properties.Select(p => p.MetadataName).Distinct().Count());
        var ordinarySymbol = Assert.Single(properties, p => p.ExplicitInterfaceImplementations.IsEmpty);
        Assert.Equal(Accessibility.Internal, ordinarySymbol.DeclaredAccessibility);
        var explicitSymbol = Assert.Single(properties, p => !p.ExplicitInterfaceImplementations.IsEmpty);
        Assert.Equal("IValue", Assert.Single(explicitSymbol.ExplicitInterfaceImplementations).ContainingType!.Name);
    }

    [Fact]
    public void DistinctExplicitInterfacePropertiesDoNotCollide()
    {
        var tree = SyntaxTree.ParseText("""
            interface ILeft { val Value: int }
            interface IRight { val Value: int }
            class C : ILeft, IRight {
                val ILeft.Value: int => 1
                val IRight.Value: int => 2
            }
            """);
        var compilation = CreateCompilation([tree], options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Equal(2, compilation.GetTypeByMetadataName("C")!.GetMembers("Value").OfType<PropertySymbol>().Count());
    }

    [Theory]
    [InlineData("Value")]
    [InlineData("IValue.Value")]
    public void RepeatedPropertyIdentityStillReportsDuplicate(string name)
    {
        var tree = SyntaxTree.ParseText($$"""
            interface IValue { val Value: int }
            class C : IValue {
                val {{name}}: int => 1
                val {{name}}: int => 2
            }
            """);
        var compilation = CreateCompilation([tree], options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0111");
    }
}
