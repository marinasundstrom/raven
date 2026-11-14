using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ExtendedCliChecks
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
        var compilation = Compilation.Create("TupleConsumer", [SyntaxTree.ParseText(File.ReadAllText(sourcePath))], catalog.References.ToArray(), options);
        var native = new NeoClrEmitOptions(new("TupleConsumer", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies);
        if (native.OutputFormat != NeoClrOutputFormat.Native) throw new Exception("native default changed");
        using (var stream = new MemoryStream())
        {
            var rejected = compilation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(native)));
            if (rejected.Success || stream.Length != 0 || !rejected.Diagnostics.Any(d => d.Id == "NEOMETA003"))
                throw new Exception("legacy mixed-format guard changed");
        }
        var extended = new NeoClrEmitOptions(new("TupleConsumer", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
            outputFormat: NeoClrOutputFormat.ExtendedCli);
        foreach (var binary in new[] { true, false })
        {
            using var stream = new MemoryStream();
            var result = compilation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(extended, binary)));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            var image = stream.ToArray();
            var snapshot = AssemblyDefinition.ReadExtendedCliAssembly(image);
            var forward = snapshot.MainModule.Functions.Single();
            if (forward.Namespace != "" || forward.Name != "Forward" || forward.ParameterNames[0] != "value" ||
                !forward.TryGetSignature(out var signature) || signature!.ParameterTypes[0].Tuple?.ElementTypes[0].Tuple?.ElementTypes.Count != 2 ||
                signature.ReturnType != signature.ParameterTypes[0] || snapshot.MainModule.MemberReferences.Single().Namespace != "Example.Values")
                throw new Exception("Raven structural signature, parameter name or import scope changed");
            File.WriteAllBytes(Path.Combine(output, binary ? "TupleConsumer.dll" : "TupleConsumer-direct.dll"), image);
        }
        var declaredModule = Compilation.Create("TupleConsumer", [SyntaxTree.ParseText("namespace Declared { public func Echo(value: int) -> int { return value } }")], catalog.References.ToArray(), options);
        using (var stream = new MemoryStream())
        {
            var emitted = declaredModule.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(extended)));
            if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
            var declared = AssemblyDefinition.ReadExtendedCliAssembly(stream.ToArray()).GetModules().Single();
            if (declared.Name != "Declared" || declared.IsProjection || declared.GetMembers().Single().Name != "Echo")
                throw new Exception("explicit logical module manifest was lost");
        }
        var branching = Compilation.Create("TupleConsumer", [SyntaxTree.ParseText("func Choose(value: bool) -> bool { if value { return true } else { return false } }")], catalog.References.ToArray(), options);
        using (var stream = new MemoryStream())
        {
            var result = branching.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(extended)));
            if (!result.Success) throw new Exception("extended branch emission failed: " + string.Join("\n", result.Diagnostics));
        }
        var unsupported = Compilation.Create("TupleConsumer", [SyntaxTree.ParseText("func Wide(value: long) -> long { return value }")], catalog.References.ToArray(), options);
        using (var stream = new MemoryStream())
        {
            var rejected = unsupported.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(extended)));
            if (rejected.Success || stream.Length != 0 || !rejected.Diagnostics.Any(d => d.Id == "NEOMETA003"))
                throw new Exception("unsupported extended body did not fail atomically");
        }
        using (var stream = new MemoryStream())
        {
            var result = unsupported.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(native)));
            if (!result.Success) throw new Exception("native Int64 forwarding regressed: " + string.Join("\n", result.Diagnostics));
        }
        Console.WriteLine("PASS neoCLR explicit extended output, tuple imports/parameter names, native defaults and atomic profile rejection");
    }
}
