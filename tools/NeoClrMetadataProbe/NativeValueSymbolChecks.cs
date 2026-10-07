using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;

namespace NeoClrMetadataProbe;

internal static class NativeValueSymbolChecks
{
    internal static void Run(string corePath)
    {
        var core = AssemblyDefinition.ReadAssembly(File.ReadAllBytes(corePath), expectedExtended: false);
        var graph = new AssemblyBuilder(new("NativeValueSymbols", new(1, 0, 0, 0)), core.Identity);
        var value = graph.AddValueType("System", "Value");
        value.SetNativePrimitive(PrimitiveType.Value);
        var integer = graph.AddValueType("System", "Int32");
        integer.SetNativePrimitive(PrimitiveType.Int32);
        var api = graph.AddType("Example", "Api");
        var echo = api.AddMethod("Echo", new(value, [value]));
        echo.GetILGenerator().LoadArgument(0);
        echo.GetILGenerator().Return();
        var native = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(graph));
        var compilation = Compilation.Create("ValueSymbols", [],
            [Raven.CodeAnalysis.MetadataReference.CreateFromFile(corePath), native], CompilationOptions.NeoCLR);
        _ = compilation.GetTypeByMetadataName("Example.Api");
        var assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(native)!;
        var symbol = assembly.GetTypeByMetadataName("System.Value")!;
        if (symbol.SpecialType != SpecialType.None || !symbol.IsValueType || symbol.TypeKind != TypeKind.Struct ||
            symbol.ContainingAssembly.Name != "NativeValueSymbols")
            throw new Exception("native erased Value lost its nominal semantic identity");
        if (assembly.GetTypeByMetadataName("System.Int32")!.SpecialType != SpecialType.System_Int32)
            throw new Exception("native numeric special type classification changed");
        var method = assembly.GetTypeByMetadataName("Example.Api")!.GetMembers("Echo").OfType<IMethodSymbol>().Single();
        if (!SymbolEqualityComparer.Default.Equals(method.ReturnType, symbol) ||
            !SymbolEqualityComparer.Default.Equals(method.Parameters.Single().Type, symbol))
            throw new Exception("native Value signature lost its owner identity");
        Console.WriteLine("PASS native erased Value is not a CLI special type; numeric classification retained");
    }
}
