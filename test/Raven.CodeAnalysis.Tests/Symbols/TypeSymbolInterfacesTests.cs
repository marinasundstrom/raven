using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TypeSymbolInterfacesTests
{
    [Fact]
    public void List_AllInterfaces_IncludesIEnumerableT()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDef = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var list = (INamedTypeSymbol)listDef.Construct(intType);

        Assert.Contains(list.AllInterfaces, i => i.Name == "IEnumerable" && i.TypeArguments.Length == 1);
        Assert.NotEmpty(list.Interfaces);
    }

    [Fact]
    public void MetadataTypeAndMethodTypeParameters_ReportOwnerKind()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDef = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        var listTypeParameter = Assert.Single(listDef.TypeParameters);

        Assert.Equal(TypeParameterOwnerKind.Type, listTypeParameter.OwnerKind);
        Assert.Same(listDef, listTypeParameter.DeclaringTypeParameterOwner);
        Assert.Null(listTypeParameter.DeclaringMethodParameterOwner);

        var convertAll = listDef.GetMembers("ConvertAll")
            .OfType<IMethodSymbol>()
            .Single(method => method.TypeParameters.Length == 1);
        var methodTypeParameter = Assert.Single(convertAll.TypeParameters);

        Assert.Equal(TypeParameterOwnerKind.Method, methodTypeParameter.OwnerKind);
        Assert.Same(convertAll, methodTypeParameter.DeclaringMethodParameterOwner);
        Assert.Null(methodTypeParameter.DeclaringTypeParameterOwner);
    }

    [Fact]
    public void Array_AllInterfaces_IncludeIEnumerables()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var array = compilation.CreateArrayTypeSymbol(intType);

        Assert.Contains(
            array.AllInterfaces,
            i =>
            {
                var typeArguments = i.TypeArguments;
                return i.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T
                    && !typeArguments.IsDefault
                    && typeArguments.Length == 1
                    && SymbolEqualityComparer.Default.Equals(typeArguments[0], intType);
            });
        Assert.Contains(array.AllInterfaces, i => i.SpecialType == SpecialType.System_Collections_IEnumerable);
        Assert.Contains(array.Interfaces, i => i.SpecialType == SpecialType.System_Collections_IEnumerable);
    }

    [Fact]
    public void Interfaces_ExcludeInheritedInterfaces()
    {
        var source = @"interface IA {} interface IB : IA {} class C : IB {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));

        var c = GetSourceType(compilation, "C");
        var ib = GetSourceType(compilation, "IB");
        var ia = GetSourceType(compilation, "IA");

        Assert.Contains(c.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ib));
        Assert.DoesNotContain(c.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.Contains(c.AllInterfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
    }

    [Fact]
    public void Class_WithBaseTypeAndMultipleInterfaces()
    {
        var source = @"interface IA {} interface IB {} class Base {} class C : Base, IA, IB {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));

        var c = GetSourceType(compilation, "C");
        var ia = GetSourceType(compilation, "IA");
        var ib = GetSourceType(compilation, "IB");
        var baseType = GetSourceType(compilation, "Base");

        Assert.Equal(baseType, c.BaseType);
        Assert.Contains(c.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.Contains(c.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ib));
    }

    [Fact]
    public void Class_WithOnlyInterfaces_HasObjectBaseType()
    {
        var source = @"interface IA {} interface IB {} class C : IA, IB {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));

        var c = GetSourceType(compilation, "C");
        var ia = GetSourceType(compilation, "IA");
        var ib = GetSourceType(compilation, "IB");

        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        Assert.Equal(objectType, c.BaseType);
        Assert.Contains(c.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.Contains(c.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ib));
    }

    [Fact]
    public void Interface_WithBaseInterface_AllInterfacesIncludeBase()
    {
        var source = @"interface IA {} interface IB : IA {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));

        var ia = GetSourceType(compilation, "IA");
        var ib = GetSourceType(compilation, "IB");

        Assert.Contains(ib.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.Contains(ib.AllInterfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.Empty(ia.AllInterfaces);
    }

    [Fact]
    public void Interface_CyclicInheritance_AllInterfacesDoNotIncludeSelf()
    {
        var source = @"interface IA : IB {} interface IB : IA {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));

        var ia = GetSourceType(compilation, "IA");
        var ib = GetSourceType(compilation, "IB");

        Assert.DoesNotContain(ia.AllInterfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.Contains(ib.AllInterfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.DoesNotContain(ib.AllInterfaces, i => SymbolEqualityComparer.Default.Equals(i, ib));
        Assert.Contains(ib.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
    }

    [Fact]
    public void Dictionary_AllInterfaces_SubstitutesTypeArgsIntoKeyValuePair()
    {
        // Regression test: ConstructedNamedTypeSymbol.AllInterfaces was not substituting type
        // parameters inside generic type arguments like KeyValuePair<TKey,TValue>.
        // Dictionary<string, int>.AllInterfaces should include IEnumerable<KeyValuePair<string, int>>
        // with the concrete types, not the open IEnumerable<KeyValuePair<TKey, TValue>>.
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        var dictDef = compilation.GetTypeByMetadataName("System.Collections.Generic.Dictionary`2")!;
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var dict = (INamedTypeSymbol)dictDef.Construct(stringType, intType);

        // Find IEnumerable<T> in all interfaces
        var ienumerable = dict.AllInterfaces
            .FirstOrDefault(i => i.Name == "IEnumerable" && i.TypeArguments.Length == 1);

        Assert.NotNull(ienumerable);

        // The single type argument should be KeyValuePair<string, int>, not KeyValuePair<TKey, TValue>
        var typeArg = Assert.IsAssignableFrom<INamedTypeSymbol>(ienumerable.TypeArguments[0]);
        Assert.Equal("KeyValuePair", typeArg.Name);
        Assert.Equal(2, typeArg.TypeArguments.Length);

        Assert.True(
            SymbolEqualityComparer.Default.Equals(typeArg.TypeArguments[0], stringType),
            $"Expected KeyValuePair<string, ?> but first arg was {typeArg.TypeArguments[0].ToDisplayString()}");

        Assert.True(
            SymbolEqualityComparer.Default.Equals(typeArg.TypeArguments[1], intType),
            $"Expected KeyValuePair<?, int> but second arg was {typeArg.TypeArguments[1].ToDisplayString()}");
    }

    [Fact]
    public void Dictionary_AllInterfaces_SubstitutesTypeArgsIntoKeyValuePair_ForAllCollectionInterfaces()
    {
        // Verify the substitution also covers ICollection<KeyValuePair<TKey,TValue>> and
        // IReadOnlyCollection<KeyValuePair<TKey,TValue>>, not just IEnumerable.
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        var dictDef = compilation.GetTypeByMetadataName("System.Collections.Generic.Dictionary`2")!;
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var dict = (INamedTypeSymbol)dictDef.Construct(stringType, intType);

        // Every generic interface whose single type argument is KeyValuePair<?,?> should
        // have it fully substituted.
        var kvpInterfaces = dict.AllInterfaces
            .Where(i => i.TypeArguments.Length == 1
                        && i.TypeArguments[0] is INamedTypeSymbol n
                        && n.Name == "KeyValuePair")
            .ToList();

        Assert.NotEmpty(kvpInterfaces);

        foreach (var iface in kvpInterfaces)
        {
            var kvp = Assert.IsAssignableFrom<INamedTypeSymbol>(iface.TypeArguments[0]);

            Assert.True(
                SymbolEqualityComparer.Default.Equals(kvp.TypeArguments[0], stringType),
                $"{iface.Name}: expected KVP<string, ?> but first type arg was {kvp.TypeArguments[0].ToDisplayString()}");

            Assert.True(
                SymbolEqualityComparer.Default.Equals(kvp.TypeArguments[1], intType),
                $"{iface.Name}: expected KVP<?, int> but second type arg was {kvp.TypeArguments[1].ToDisplayString()}");
        }
    }

    private static INamedTypeSymbol GetSourceType(Compilation compilation, string name)
    {
        foreach (var syntaxTree in compilation.SyntaxTrees)
            _ = compilation.GetSemanticModel(syntaxTree);

        return Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.LookupType(name));
    }
}
