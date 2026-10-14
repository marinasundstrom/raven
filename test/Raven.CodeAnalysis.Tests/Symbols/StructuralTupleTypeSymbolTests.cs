using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StructuralTupleTypeSymbolTests
{
    [Fact]
    public void NativeTupleFactoryNeedsNoNominalTupleDeclaration()
    {
        var types = Compilation.Create("Types", [], TestMetadataReferences.Default, CompilationOptions.DotNet);
        var integer = types.GetSpecialType(SpecialType.System_Int32);
        var text = types.GetSpecialType(SpecialType.System_String);
        var native = Compilation.Create("Native", [], [], CompilationOptions.NeoCLR.WithMetadataImportOptions(
            new MetadataImportOptions("NeoCLR.CoreProbe").WithStructuralTuples()));
        var tuple = Assert.IsType<StructuralTupleTypeSymbol>(native.CreateTupleTypeSymbol([("count", integer), ("text", text)]));
        Assert.Null(tuple.UnderlyingTupleType);
        Assert.Null(tuple.ContainingAssembly);
        Assert.Empty(tuple.MetadataName);
        Assert.Empty(tuple.TypeArguments);
        Assert.Empty(tuple.Interfaces);
        Assert.Empty(tuple.Constructors);
        Assert.True(tuple.IsValueType);
        Assert.Same(tuple, tuple.TupleElements[0].ContainingType);
        Assert.Same(integer, Assert.IsAssignableFrom<IFieldSymbol>(Assert.Single(tuple.GetMembers("Item1"))).Type);
        Assert.Same(text, Assert.IsAssignableFrom<IFieldSymbol>(Assert.Single(tuple.GetMembers("text"))).Type);
        Assert.Empty(tuple.GetMembers("ToString"));
        Assert.Equal("(count: int, text: string)", tuple.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat.WithMiscellaneousOptions(
            SymbolDisplayFormat.MinimallyQualifiedFormat.MiscellaneousOptions | SymbolDisplayMiscellaneousOptions.IncludeTupleElementNames)));
    }

    [Fact]
    public void IdentityUsesOrderedNestedElementTypesAndIgnoresNames()
    {
        var compilation = Compilation.Create("Types", [], TestMetadataReferences.Default, CompilationOptions.DotNet);
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var text = compilation.GetSpecialType(SpecialType.System_String);
        var left = new StructuralTupleTypeSymbol([("left", integer), ("right", text)]);
        var renamed = new StructuralTupleTypeSymbol([("x", integer), ("y", text)]);
        var reversed = new StructuralTupleTypeSymbol([("left", text), ("right", integer)]);
        var repeated = new StructuralTupleTypeSymbol([("left", integer), ("right", integer)]);
        Assert.True(SymbolEqualityComparer.Default.Equals(left, renamed));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(left), SymbolEqualityComparer.Default.GetHashCode(renamed));
        Assert.False(SymbolEqualityComparer.Default.Equals(left, reversed));
        Assert.False(left.MetadataIdentityEquals(repeated));
        var list = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        Assert.False(list.Construct(left).MetadataIdentityEquals(list.Construct(reversed)));
        var nested = new StructuralTupleTypeSymbol([("pair", left), ("again", left)]);
        var nestedRenamed = new StructuralTupleTypeSymbol([("first", renamed), ("second", new StructuralTupleTypeSymbol([("a", integer), ("b", text)]))]);
        Assert.True(SymbolEqualityComparer.Default.Equals(nested, nestedRenamed));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(nested), SymbolEqualityComparer.Default.GetHashCode(nestedRenamed));
        Assert.False(SymbolEqualityComparer.Default.Equals(left, compilation.CreateTupleTypeSymbol([("left", integer), ("right", text)])));
    }

    [Fact]
    public void StructuralTupleOptionRejectsOrdinaryDotNetTarget()
    {
        var compilation = Compilation.Create("Types", [], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithMetadataImportOptions(new MetadataImportOptions().WithStructuralTuples()));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAVT003");
    }

    [Fact]
    public void SubstitutionPreservesNativeShapeAndNames()
    {
        var compilation = Compilation.Create("Types", [], TestMetadataReferences.Default, CompilationOptions.DotNet);
        var text = compilation.GetSpecialType(SpecialType.System_String);
        var parameter = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!.TypeParameters[0];
        var tuple = new StructuralTupleTypeSymbol([("value", parameter), (null, text)]);
        Assert.Same(tuple, TypeSubstitution.SubstituteTupleElements(tuple, type => type));
        var replaced = Assert.IsType<StructuralTupleTypeSymbol>(TypeSubstitution.SubstituteTupleElements(tuple, type => ReferenceEquals(type, parameter) ? text : type));
        Assert.Null(replaced.UnderlyingTupleType);
        Assert.Equal(new[] { "value", "Item2" }, replaced.TupleElements.Select(field => field.Name));
        Assert.All(replaced.TupleElements, field => Assert.Same(text, field.Type));
        Assert.Same(parameter, tuple.TupleElements[0].Type);
    }
}
