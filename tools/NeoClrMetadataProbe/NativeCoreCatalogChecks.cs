using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeCoreCatalogChecks
{
    internal static void Run(string nativeCorePath, string cliCorePath, string directory)
    {
        Directory.CreateDirectory(directory);
        var corePath = Path.Combine(directory, "Core.dll");
        File.Copy(nativeCorePath, corePath, true);
        var empty = NeoClrReferenceCatalog.ReadNative(corePath, []);
        if (!empty.UsesNativeMetadata || empty.NativeCore is null ||
            !ReferenceEquals(empty.CoreReference, empty.NativeCore) ||
            !ReferenceEquals(empty.References[0], empty.Dependencies[0].Reference))
            throw new Exception("Native core semantic/emission snapshots differ.");
        Reject<InvalidOperationException>(() => _ = empty.Bootstrap);
        Reject<InvalidDataException>(() => NeoClrReferenceCatalog.ReadNative(cliCorePath, []));
        Reject<InvalidDataException>(() => NeoClrReferenceCatalog.ReadNative(corePath, [cliCorePath]));
        Reject<ArgumentException>(() => NeoClrReferenceCatalog.ReadNative(corePath, [corePath]));
        var copy = Path.Combine(directory, "Copy.dll");
        File.Copy(corePath, copy, true);
        Reject<ArgumentException>(() => NeoClrReferenceCatalog.ReadNative(corePath, [copy]));
        Reject<IOException>(() => NeoClrReferenceCatalog.ReadNative(Path.Combine(directory, "Missing.dll"), []));
        File.WriteAllBytes(copy, [1, 2, 3]);
        Reject<InvalidDataException>(() => NeoClrReferenceCatalog.ReadNative(copy, []));

        var libraryPath = Path.Combine(directory, "Input.dll");
        void WriteLibrary(string name)
        {
            var graph = new AssemblyBuilder(new("Input", new(1, 0, 0, 0)), empty.CoreIdentity);
            var method = graph.AddType("Example", "Input").AddMethod(name, new(PrimitiveType.Int32, []));
            method.GetILGenerator().LoadConstant(40);
            method.GetILGenerator().Return();
            File.WriteAllBytes(libraryPath, RuntimeAssemblyContainer.WriteLibraryBinary(graph));
        }
        WriteLibrary("Value");
        File.WriteAllText(Path.ChangeExtension(corePath, ".xml"), "<doc><members><member name=\"T:System.Object\"><summary>Native core root.</summary></member></members></doc>");
        var first = NeoClrReferenceCatalog.ReadNative(corePath, [libraryPath]);
        var options = CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null)
            .WithTargetCoreAssemblyName(first.CoreIdentity.Name)
            .WithRuntimeUnitContract(new(first.CoreIdentity.Name, "System.Void"))
            .WithMetadataImportOptions(new MetadataImportOptions(first.CoreIdentity.Name)
                .WithObjectAssemblyName(first.CoreIdentity.Name).WithNativeMetadata());
        Compilation Create(NeoClrReferenceCatalog catalog, string member) => Compilation.Create("Consumer",
            [SyntaxTree.ParseText($"func Main() -> int => Example.Input.{member}() + 2")], catalog.References.ToArray(), options);
        void Valid(Compilation compilation)
        {
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        }
        var earlier = Create(first, "Value");
        Valid(earlier);
        if (earlier.GetSpecialType(SpecialType.System_Object).GetDocumentationComment()?.Content.Contains("Native core root.") != true)
            throw new Exception("Native core documentation was not captured.");
        WriteLibrary("Updated");
        var second = NeoClrReferenceCatalog.ReadNative(corePath, [libraryPath]);
        Valid(Create(second, "Updated"));
        Valid(Create(first, "Value"));
        if (!Create(second, "Value").GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error))
            throw new Exception("New native catalog retained stale library symbols.");
        File.WriteAllBytes(corePath, [1, 2, 3]);
        Valid(earlier);
        Valid(Create(first, "Value"));
        Reject<InvalidDataException>(() => NeoClrReferenceCatalog.ReadNative(corePath, [libraryPath]));
        using var image = new MemoryStream();
        var emitted = earlier.Emit(image, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("Consumer", new(1, 0, 0, 0)), first.CoreIdentity, first.Dependencies))));
        if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
        var consumerPath = Path.Combine(directory, "Consumer.dll");
        File.WriteAllBytes(consumerPath, image.ToArray());
        File.Copy(nativeCorePath, corePath, true);
        var incomplete = NeoClrReferenceCatalog.ReadNative(corePath, [consumerPath]);
        var missing = Create(incomplete, "Value");
        if (!missing.GetDiagnostics().Any(d => d.ToString().Contains("missing or mismatched native dependency")))
            throw new Exception("Native catalog missing dependency did not remain diagnostic.");
        Console.WriteLine("PASS native core catalog snapshots, dependency emission, XML, missing dependency, duplicates and CLI/malformed rejection");
    }

    private static void Reject<T>(Action action) where T : Exception
    {
        try { action(); }
        catch (T) { return; }
        throw new Exception("Expected " + typeof(T).Name);
    }
}
