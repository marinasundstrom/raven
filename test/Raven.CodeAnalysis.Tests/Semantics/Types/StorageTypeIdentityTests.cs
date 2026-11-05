using Raven.CodeAnalysis.Syntax;

using CS = Microsoft.CodeAnalysis.CSharp;
using R = Microsoft.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests;

public sealed class StorageTypeIdentityTests
{
    public static IEnumerable<object[]> SourceOrders()
    {
        int[][] orders = [[0, 1, 2], [0, 2, 1], [1, 0, 2], [1, 2, 0], [2, 0, 1], [2, 1, 0]];
        foreach (var order in orders)
        {
            yield return [order, true];
            yield return [order, false];
        }
    }

    [Theory]
    [MemberData(nameof(SourceOrders))]
    [Trait("Target", "DotNet")]
    public void DotNet_PropertyRetainsConstructedType(int[] order, bool satisfiesConstraint)
        => CheckPropertyType(order, satisfiesConstraint, TargetPlatform.DotNet);

    [Theory]
    [MemberData(nameof(SourceOrders))]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_PropertyRetainsConstructedType(int[] order, bool satisfiesConstraint)
        => CheckPropertyType(order, satisfiesConstraint, TargetPlatform.NeoCLR);

    private static void CheckPropertyType(int[] order, bool satisfiesConstraint, TargetPlatform target)
    {
        string[] sources =
        [
            """
            namespace Demo
            public open class Node {}
            public struct List<T> where T: Node {}
            """,
            """
            namespace Demo
            public class Container {
                val Items: List<Element> { get => default(List<Element>) }
            }
            """,
            "namespace Demo\npublic class Element" + (satisfiesConstraint ? " : Node" : "") + " {}"
        ];
        var trees = order.Select(index => SyntaxTree.ParseText(sources[index], path: $"{index}.rvn")).ToArray();
        var compilation = Compilation.Create(
            "ConstraintIdentity",
            trees,
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithFrameworkProjectionMode(FrameworkProjectionMode.None)
                .WithTargetPlatform(target));
        var tree = trees.Single(tree => tree.FilePath == "1.rvn");
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();

        var property = Assert.IsAssignableFrom<PropertySymbol>(model.GetDeclaredSymbol(declaration));
        var type = Assert.IsAssignableFrom<NamedTypeSymbol>(property.Type);
        Assert.Equal("List", type.Name);
        Assert.Equal("Element", Assert.Single(type.TypeArguments).Name);

        // neoCLR checks shared binding against host references, not native emission.
        var errors = model.GetDiagnostics().Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ToArray();
        if (satisfiesConstraint)
            Assert.Empty(errors);
        else
            Assert.Contains(errors, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);

        Assert.Same(property, model.GetDeclaredSymbol(declaration));
        Assert.Same(type, property.Type);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    [Trait("Target", "DotNet")]
    public void CSharp_PreservesConstructedPropertyType(bool satisfiesConstraint)
    {
        var source = "public class Node {} public struct List<T> where T:Node {} public class Element"
            + (satisfiesConstraint ? ":Node" : "")
            + " {} public class Container { public List<Element> Items => default; }";
        var tree = CS.CSharpSyntaxTree.ParseText(source);
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion(TestMetadataReferences.TargetFramework))
            .Select(path => R.MetadataReference.CreateFromFile(path));
        var compilation = CS.CSharpCompilation.Create(
            "ConstraintIdentityOracle",
            [tree],
            references,
            new CS.CSharpCompilationOptions(R.OutputKind.DynamicallyLinkedLibrary));
        var declaration = tree.GetRoot().DescendantNodes().OfType<CS.Syntax.PropertyDeclarationSyntax>().Single();
        var property = Assert.IsAssignableFrom<R.IPropertySymbol>(CS.CSharpExtensions.GetDeclaredSymbol(compilation.GetSemanticModel(tree), declaration));
        var type = Assert.IsAssignableFrom<R.INamedTypeSymbol>(property.Type);
        Assert.Equal("List", type.Name);
        Assert.Equal("Element", Assert.Single(type.TypeArguments).Name);
        Assert.Equal(!satisfiesConstraint, compilation.GetDiagnostics().Any(diagnostic => diagnostic.Severity == R.DiagnosticSeverity.Error));
    }
}
