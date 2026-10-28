using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeInliningChecks
{
    internal static void Run(string corePath)
    {
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, []);
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeTypeOfContract(null).WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
            .WithRuntimeUnitContract(new(catalog.CoreIdentity.Name, "System.Void"))
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name)
                .WithObjectAssemblyName(catalog.CoreIdentity.Name).WithNativeMetadata());
        foreach (var flags in new[] { 0, 8, 256, 264, 64, 512, 4096 })
        {
            var source = "import System.Runtime.CompilerServices.*\n[MethodImpl((MethodImplOptions)" + flags + ")]\nfunc Compute(value: int) -> int => value * 3";
            var compilation = Compilation.Create("InlineSource", [SyntaxTree.ParseText(source)], catalog.References.ToArray(), options);
            using var output = new MemoryStream();
            var result = compilation.Emit(output, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
                new(new("InlineSource", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies))));
            if (result.Success != ((flags & ~0x108) == 0)) throw new Exception(string.Join("\n", result.Diagnostics));
            if (result.Success)
            {
                var method = AssemblyDefinition.ReadNativeAssembly(output.ToArray()).MainModule.Methods.Single(m => m.Name == "Compute");
                if (method.ImplementationAttributes != flags || method.CustomAttributes.Count != 0)
                    throw new Exception("MethodImpl must be implementation bits, not an executable custom attribute");
            }
            else if (output.Length != 0) throw new Exception("Invalid flags published output");
        }
        Console.WriteLine("PASS native inlining flags and unsupported flag rejection");
    }
}
