using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeArrayShapeChecks
{
    internal static void Run(string corePath)
    {
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, []);
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
            .WithRuntimeTypeOfContract(null).WithRuntimeUnitContract(new(catalog.CoreIdentity.Name, "System.Void"))
            .WithRuntimeIterationContract(new("Arrays", "System.Collections.Iterable`1", "System.Collections.Iterator`1", ArraysImplementIterable: true, ArrayShapeTypeName: "System.Array`1"))
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name).WithObjectAssemblyName(catalog.CoreIdentity.Name).WithNativeMetadata());
        var source = """
            namespace System {
                public interface Sequence<T> { val Count: int { get } }
                public class Array<T> : Sequence<T> {
                    val Count: int => 1
                    val Length: int => 1
                }
            }
            """;
        var compilation = Compilation.Create("Arrays", [SyntaxTree.ParseText(source)], catalog.References.ToArray(), options);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        if (compilation.GetSpecialType(SpecialType.System_Array).SpecialType != SpecialType.System_Array ||
            compilation.GetSpecialType(SpecialType.System_Enum).SpecialType != SpecialType.System_Enum)
            throw new Exception("Native core array/enum identity missing");
        var vector = (IArrayTypeSymbol)compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_Int32));
        if (!vector.GetMembers("Length").Any() || !vector.AllInterfaces.Any(i => i.Name == "Sequence"))
            throw new Exception("Native vector lost selected shape members/interfaces");
        var multidimensional = (IArrayTypeSymbol)compilation.CreateArrayTypeSymbol(vector.ElementType, 2);
        if (multidimensional.GetMembers("Length").Any() || multidimensional.AllInterfaces.Any(i => i.Name == "Sequence"))
            throw new Exception("Rank-two array acquired vector shape");
        Console.WriteLine("PASS native Array/Enum classification and explicit vector shape projection");
    }
}
