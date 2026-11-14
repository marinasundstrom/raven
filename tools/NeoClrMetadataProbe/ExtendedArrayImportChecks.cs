using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ExtendedArrayImportChecks
{
    internal static void Run(string corePath, string libraryPath, string sourcePath, string output)
    {
        Directory.CreateDirectory(output);
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, [libraryPath]);
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeTypeOfContract(null).WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
            .WithRuntimeUnitContract(new(catalog.CoreIdentity.Name, "System.Void"))
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name)
                .WithObjectAssemblyName(catalog.CoreIdentity.Name).WithNativeMetadata());
        var compilation = Compilation.Create("ArrayConsumer", [SyntaxTree.ParseText(File.ReadAllText(sourcePath))], catalog.References.ToArray(), options);
        var emitOptions = new NeoClrEmitOptions(new("ArrayConsumer", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
            outputFormat: NeoClrOutputFormat.ExtendedCli);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(emitOptions)));
        if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
        var snapshot = AssemblyDefinition.ReadExtendedCliAssembly(stream.ToArray());
        foreach (var (name, depth) in new[] { ("ForwardArray", 1), ("ForwardJagged", 2) })
        {
            var method = snapshot.MainModule.Functions.Single(m => m.Name == name);
            if (!method.TryGetSignature(out var signature) || signature!.ParameterTypes.Count != 1 ||
                signature.ReturnType != signature.ParameterTypes[0] || method.ParameterNames[0] != "value")
                throw new Exception("array import signature or name lost");
            var element = signature.ReturnType;
            for (int i = 0; i < depth; i++) element = element.ArrayElement ?? throw new Exception("array nesting lost");
            if (element.Primitive != PrimitiveType.Int32) throw new Exception("array element changed");
        }
        if (snapshot.MainModule.MemberReferences.Count != 2 ||
            snapshot.MainModule.MemberReferences.Any(m => m.Namespace != "Example.Arrays"))
            throw new Exception("array import namespace lost");
        File.WriteAllBytes(Path.Combine(output, "ArrayConsumer.dll"), stream.ToArray());
        // Default selection still emits the native transport for a local jagged identity.
        using var nativeStream = new MemoryStream();
        var native = new NeoClrEmitOptions(new("ArrayConsumer", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies);
        if (native.OutputFormat != NeoClrOutputFormat.Native) throw new Exception("native default changed");
        var local = Compilation.Create("ArrayConsumer", [SyntaxTree.ParseText("public func Identity(value: int[][]) -> int[][] { return value }")], catalog.References.ToArray(), options);
        var emitted = local.Emit(nativeStream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(native)));
        if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
        var nativeSnapshot = AssemblyDefinition.ReadNativeAssembly(nativeStream.ToArray());
        if (!nativeSnapshot.MainModule.Functions.Single().TryGetSignature(out var nativeSignature) ||
            nativeSignature!.ReturnType.ArrayElement?.ArrayElement?.Primitive != PrimitiveType.Int32)
            throw new Exception("native jagged signature lost");
        Console.WriteLine("PASS neoCLR vector/jagged callable imports, recursive signatures, names/scopes and native defaults");
    }
}
