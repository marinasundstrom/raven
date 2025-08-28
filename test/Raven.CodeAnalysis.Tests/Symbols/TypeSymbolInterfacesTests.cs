using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

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
}
