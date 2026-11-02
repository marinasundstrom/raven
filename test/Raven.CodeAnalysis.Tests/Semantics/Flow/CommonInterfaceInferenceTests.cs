using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class CommonInterfaceInferenceTests
{
    [Theory]
    [InlineData("IReadOnlyList<int>", "IEnumerable<int>", false)]
    [InlineData("IReadOnlyList<int>", "IEnumerable<int>", true)]
    [InlineData("IEnumerable<int>", "IReadOnlyList<int>", false)]
    [InlineData("IEnumerable<int>", "IReadOnlyList<int>", true)]
    [InlineData("List<int>", "IEnumerable<int>", false)]
    [InlineData("List<int>", "IEnumerable<int>", true)]
    [InlineData("IReadOnlyCollection<int>", "IEnumerable<int>", false)]
    [InlineData("IReadOnlyCollection<int>", "IEnumerable<int>", true)]
    public void Coalescing_PreservesMostSpecificCommonInterface(string left, string right, bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            func Pick(left: {{left}}?, right: {{right}}) -> IEnumerable<int> {
                let result = left ?? right
                return result
            }
            """);
        var compilation = Compilation.Create("CommonInterfaceInference", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var declaration = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(local.Type);
        Assert.Equal("IEnumerable", type.Name);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(type.TypeArguments).SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void IncomparableCommonInterfaces_UseObjectFallback()
    {
        var tree = SyntaxTree.ParseText("""
            interface First { }
            interface Second { }
            class Left : First, Second { }
            class Right : First, Second { }
            """);
        var compilation = Compilation.Create("AmbiguousInterfaces", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var inferred = TypeSymbolNormalization.GetBestCommonType([
            compilation.GetTypeByMetadataName("Left")!, compilation.GetTypeByMetadataName("Right")!]);
        Assert.Equal(SpecialType.System_Object, inferred.SpecialType);
    }

    [Fact]
    public void CommonInterface_PrefersDerivedContractOverEarlierAncestor()
    {
        var tree = SyntaxTree.ParseText("""
            interface Root { }
            interface Child : Root { }
            class Left : Root, Child { }
            class Right : Root, Child { }
            """);
        var compilation = Compilation.Create("MostSpecificInterface", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var left = compilation.GetTypeByMetadataName("Left")!;
        var right = compilation.GetTypeByMetadataName("Right")!;
        var child = compilation.GetTypeByMetadataName("Child")!;
        Assert.True(SymbolEqualityComparer.Default.Equals(child, TypeSymbolNormalization.GetBestCommonType([left, right])));
    }

    [Theory]
    [InlineData("IList<int>", "IReadOnlyList<int>", false)]
    [InlineData("IList<int>", "IReadOnlyList<int>", true)]
    [InlineData("IEnumerable<int>", "IReadOnlyList<int>", false)]
    [InlineData("IEnumerable<int>", "IReadOnlyList<int>", true)]
    public void BranchInference_PreservesMostSpecificSharedGenericInterface(string left, string right, bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            func Pick(flag: bool, left: {{left}}, right: {{right}}) -> IEnumerable<int> {
                let result = if flag { left } else { right }
                return result
            }
            """);
        var compilation = Compilation.Create("CommonBranchInterface", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<IfExpressionSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetTypeInfo(expression).Type);
        Assert.Equal("IEnumerable", type.Name);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(type.TypeArguments).SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void InterfaceOperand_IsIncludedInItsOwnCommonContract()
    {
        var tree = SyntaxTree.ParseText("""
            interface Root { }
            interface Child : Root { }
            """);
        var compilation = Compilation.Create("InterfaceSelfContract", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var root = compilation.GetTypeByMetadataName("Root")!;
        var child = compilation.GetTypeByMetadataName("Child")!;
        var inferred = TypeSymbolNormalization.GetBestCommonType([child, root]);
        Assert.True(SymbolEqualityComparer.Default.Equals(root, inferred));
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrCommonInterfaceInferenceTests
{
    [Fact]
    public void NeoClrSemanticInferenceWithHostReferences_PreservesGenericInterface()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            func Pick(left: IReadOnlyList<int>?, right: IEnumerable<int>) -> IEnumerable<int> {
                let result = left ?? right
                return result
            }
            """);
        var compilation = Compilation.Create("NeoClrCommonInterfaceInference", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        Assert.DoesNotContain(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
