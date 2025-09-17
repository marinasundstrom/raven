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
    public void Interfaces_ExcludeInheritedInterfaces()
    {
        var source = @"interface IA {} interface IB : IA {} class C : IB {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));

        var c = (INamedTypeSymbol)compilation.GetTypeByMetadataName("C")!;
        var ib = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IB")!;
        var ia = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IA")!;

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

        var c = (INamedTypeSymbol)compilation.GetTypeByMetadataName("C")!;
        var ia = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IA")!;
        var ib = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IB")!;
        var baseType = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Base")!;

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

        var c = (INamedTypeSymbol)compilation.GetTypeByMetadataName("C")!;
        var ia = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IA")!;
        var ib = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IB")!;

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

        var ia = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IA")!;
        var ib = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IB")!;

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

        var ia = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IA")!;
        var ib = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IB")!;

        Assert.DoesNotContain(ia.AllInterfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.Contains(ib.AllInterfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
        Assert.DoesNotContain(ib.AllInterfaces, i => SymbolEqualityComparer.Default.Equals(i, ib));
        Assert.Contains(ib.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, ia));
    }
}
