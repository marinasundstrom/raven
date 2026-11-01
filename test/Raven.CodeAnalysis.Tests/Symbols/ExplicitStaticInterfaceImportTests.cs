using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ExplicitStaticInterfaceImportTests
{
    [Theory]
    [InlineData("int", "MaxValue", SpecialType.System_Int32)]
    [InlineData("int", "MinValue", SpecialType.System_Int32)]
    [InlineData("long", "MaxValue", SpecialType.System_Int64)]
    [InlineData("double", "MaxValue", SpecialType.System_Double)]
    public void DotNet_NumericConstantLookupDoesNotSelectExplicitInterfaceProperty(string typeName, string member, SpecialType specialType)
    {
        var tree = SyntaxTree.ParseText($"class Bounds {{ static func Read() -> {typeName} => {typeName}.{member} }}");
        var compilation = Compilation.Create("NumericBounds", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var access = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>().Single();
        var field = Assert.IsAssignableFrom<IFieldSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(access).Symbol);
        Assert.True(field.IsConst);
        Assert.Equal(Accessibility.Public, field.DeclaredAccessibility);
        Assert.Equal(specialType, field.Type.SpecialType);
    }

    [Fact]
    public void DotNet_ClosedGenericExplicitMembersRetainInterfaceIdentity()
    {
        var compilation = Compilation.Create("ExplicitClosedGeneric").AddReferences([
            .. TestMetadataReferences.Default,
            MetadataReference.CreateFromFile(typeof(PortImportFixture).Assembly.Location)
        ]);
        var type = compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Tests.PortImportFixture")!;
        var property = Assert.Single(type.GetMembers("Value").OfType<IPropertySymbol>());
        var method = Assert.Single(type.GetMembers("Read").OfType<IMethodSymbol>());
        var @event = Assert.Single(type.GetMembers("Changed").OfType<IEventSymbol>());
        Assert.Single(property.ExplicitInterfaceImplementations);
        Assert.Single(method.ExplicitInterfaceImplementations);
        Assert.Single(@event.ExplicitInterfaceImplementations);
        Assert.Single(property.GetMethod!.ExplicitInterfaceImplementations);
        Assert.Single(@event.AddMethod!.ExplicitInterfaceImplementations);
    }

    [Fact]
    public void DotNet_StaticExplicitPropertyAndAccessorRetainInterfaceIdentity()
    {
        var compilation = Compilation.Create("ExplicitMinMax").AddReferences(TestMetadataReferences.Default);
        var type = compilation.GetSpecialType(SpecialType.System_Int32);
        var property = Assert.Single(type.GetMembers("MaxValue").OfType<IPropertySymbol>());
        var contract = Assert.Single(property.ExplicitInterfaceImplementations);
        Assert.Equal("MaxValue", contract.Name);
        Assert.True(contract.IsStatic);
        Assert.Equal("IMinMaxValue", contract.ContainingType!.Name);
        Assert.Equal(SpecialType.System_Int32, contract.Type.SpecialType);
        var accessorContract = Assert.Single(property.GetMethod!.ExplicitInterfaceImplementations);
        Assert.Equal("get_MaxValue", accessorContract.Name);
    }
}

public interface IPortImportContract<T>
{
    T Value { get; }
    T Read();
    event Action<T> Changed;
}

public sealed class PortImportFixture : IPortImportContract<int>
{
    int IPortImportContract<int>.Value => 42;
    int IPortImportContract<int>.Read() => 42;
    event Action<int> IPortImportContract<int>.Changed { add { } remove { } }
}
