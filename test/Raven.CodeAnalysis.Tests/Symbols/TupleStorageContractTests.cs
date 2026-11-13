using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public sealed class TupleStorageContractTests
{
    [Theory]
    [InlineData(1)]
    [InlineData(7)]
    [InlineData(8)]
    [InlineData(15)]
    public void ConstructionAndProjectionPreserveFlattenedElementsAndStoragePaths(int length)
    {
        var element = Named("Types", "Example.Element");
        var elements = Enumerable.Repeat<TypeSymbol>(element, length).ToArray();
        var calls = new List<int>();
        NamedTypeSymbol Resolve(int arity)
        {
            calls.Add(arity);
            var definition = Storage(new TypeSymbol[arity]);
            Set(definition, "Construct", (Func<object?[]?, object?>)(args => Storage((TypeSymbol[])args![0]!)));
            return definition;
        }
        var storage = TupleStorageLayout.Construct(elements, Resolve)!;
        var slots = TupleStorageLayout.Describe(storage);
        Assert.Equal(length, slots.Length);
        for (var i = 0; i < length; i++)
        {
            Assert.Same(element, slots[i].Type);
            Assert.Equal($"Item{i % 7 + 1}", slots[i].Field.Name);
            Assert.Equal(i / 7, slots[i].Path.Length);
            Assert.All(slots[i].Path, field => Assert.Equal("Rest", field.Name));
        }
        var tuple = TupleStorageLayout.Project(storage, slots, elements.Select((type, i) => ((string?)$"value{i}", type)).ToArray())!;
        Assert.Same(storage, tuple.UnderlyingTupleType);
        Assert.Equal(elements, tuple.TypeArguments);
        Assert.Equal(length, tuple.TupleElements.Length);
        Assert.Same(tuple.TupleElements[0], Assert.Single(tuple.GetMembers("value0")));
        Assert.True(tuple.IsMemberDefined("value0", out var found));
        Assert.Same(tuple.TupleElements[0], found);
        Assert.Null(tuple.GetDocumentationComment());
        Assert.Null(tuple.ContainingAssembly);
        Assert.Null(tuple.ContainingModule);
        Assert.Equal("tuple", tuple.Accept(new Visitor()));
        Assert.Equal(length <= 7 ? new[] { length } : new[] { 8, (length - 1) % 7 + 1 }, calls);
    }

    [Fact]
    public void ProjectionRetainsOriginalOwnershipAndUnderlyingMemberLookup()
    {
        var element = Named("Types", "Example.Element");
        var storage = Storage([element]);
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, "Owner");
        var module = Make<ModuleSymbol>(SymbolKind.Module, "Module");
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, "Example");
        Set(ns, "get_ContainingAssembly", assembly);
        Set(ns, "get_ContainingModule", module);
        var original = new TupleTypeSymbolImplementation(storage, assembly, null, ns, []);
        var tuple = TupleStorageLayout.Project(storage, TupleStorageLayout.Describe(storage), [("value", element)], original)!;
        Assert.Same(assembly, tuple.ContainingSymbol);
        Assert.Same(assembly, tuple.ContainingAssembly);
        Assert.Same(module, tuple.ContainingModule);
        Assert.Same(ns, tuple.ContainingNamespace);
        Assert.Equal(2, tuple.GetMembers().Length);
        Assert.Single(tuple.GetMembers("Item1"));
        Assert.False(tuple.IsMemberDefined("missing", out var missing));
        Assert.Null(missing);
        Assert.Null(tuple.LookupType("missing"));
        Assert.Same(tuple, tuple.Construct(element));
    }

    [Fact]
    public void InvalidStorageOrProjectionReturnsFailure()
    {
        var element = Named("Types", "Example.Element");
        Assert.True(TupleStorageLayout.Describe(Storage([])).IsDefault);
        var missing = Storage([element]);
        Set(missing, "GetMembers", ImmutableArray<Symbol>.Empty);
        Assert.True(TupleStorageLayout.Describe(missing).IsDefault);
        Set(element, "get_ConstructedFrom", null);
        var badRest = Storage(Enumerable.Repeat<TypeSymbol>(element, 8).ToArray());
        Assert.True(TupleStorageLayout.Describe(badRest).IsDefault);
        Assert.Null(TupleStorageLayout.Project(missing, default, []));
        Assert.Null(TupleStorageLayout.Project(missing, [], [(null, element)]));
        Assert.Null(TupleStorageLayout.Construct([element], _ => null));
        Assert.Null(TupleStorageLayout.Construct(Enumerable.Repeat<TypeSymbol>(element, 8).ToArray(), _ => null));
    }

    private static NamedTypeSymbol Storage(TypeSymbol[] elements)
    {
        var storage = Named("Types", "System.ValueTuple");
        Set(storage, "get_ConstructedFrom", null);
        Set(storage, "get_SpecialType", elements.Length == 8 ? SpecialType.System_ValueTuple_TRest : SpecialType.System_ValueTuple_T1);
        Set(storage, "get_TypeArguments", elements.ToImmutableArray());
        Set(storage, "get_BaseType", null);
        var fields = elements.Select((type, index) =>
        {
            var field = Make<FieldSymbol>(SymbolKind.Field, index == 7 ? "Rest" : $"Item{index + 1}");
            Set(field, "get_Type", type);
            Set(field, "get_IsStatic", false);
            return (Symbol)field;
        }).ToImmutableArray();
        Set(storage, "GetMembers", (Func<object?[]?, object?>)(args => args!.Length == 0 ? fields : fields.Where(field => field.Name == (string)args[0]!).ToImmutableArray()));
        Set(storage, "IsMemberDefined", (Func<object?[]?, object?>)(args => { args![1] = null; return false; }));
        Set(storage, "LookupType", null);
        return storage;
    }

    private sealed class Visitor : SymbolVisitor<string>
    {
        public override string VisitTupleType(TupleTypeSymbol symbol) => "tuple";
    }
}
