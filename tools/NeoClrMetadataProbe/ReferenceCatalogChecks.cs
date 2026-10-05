using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ReferenceCatalogChecks
{
    internal static void Run(string corePath, string seedPath, string directory)
    {
        Directory.CreateDirectory(directory);
        var empty = NeoClrReferenceCatalog.Read(corePath, []);
        var libraryPath = Path.Combine(directory, "Library.dll");
        void WriteLibrary(string member)
        {
            var graph = new AssemblyBuilder(new("CatalogLibrary", new(1, 0, 0, 0)), empty.CoreIdentity);
            var method = graph.AddType("Library", "Api").AddMethod(member, new MethodSignature(PrimitiveType.Int32, []));
            var il = method.GetILGenerator(); il.LoadConstant(42); il.Return();
            File.WriteAllBytes(libraryPath, RuntimeAssemblyContainer.WriteLibraryBinary(graph));
        }
        WriteLibrary("Value");
        var first = NeoClrReferenceCatalog.Read(corePath, [libraryPath], seedPath);
        if (first.References.Length != 2 || first.Dependencies.Length != 2 ||
            !ReferenceEquals(first.References[0], first.Bootstrap.Reference) ||
            !ReferenceEquals(first.Dependencies[0].Reference, first.References[0]) ||
            !ReferenceEquals(first.Dependencies[1].Reference, first.References[1]) ||
            first.References[1] is not NeoClrMetadataReference)
            throw new Exception("semantic/emission snapshot identities differ");
        Compilation Compile(NeoClrReferenceCatalog catalog, string member) => Compilation.Create("CatalogConsumer",
            [SyntaxTree.ParseText($"func Main() -> int => Library.Api.{member}()")], catalog.References.ToArray(),
            CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null).WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
                .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name)));
        static void Valid(Compilation compilation)
        {
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        }
        var oldCompilation = Compile(first, "Value"); Valid(oldCompilation);
        var earlier = Path.Combine(directory, "Earlier.dll"); File.Copy(libraryPath, earlier, true);
        WriteLibrary("Updated");
        Reject<ArgumentException>(() => NeoClrReferenceCatalog.Read(corePath, [libraryPath, earlier]));
        var second = NeoClrReferenceCatalog.Read(corePath, [libraryPath], seedPath);
        Valid(Compile(second, "Updated"));
        Valid(oldCompilation); Valid(Compile(first, "Value"));
        if (!Compile(second, "Value").GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error))
            throw new Exception("fresh catalog retained stale declarations");
        var current = Compile(second, "Updated");
        using var image = new MemoryStream();
        var emitted = current.Emit(image, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("CatalogConsumer", new(1, 0, 0, 0)), second.CoreIdentity, second.Dependencies, second.Bootstrap.Reference))));
        if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
        var consumerPath = Path.Combine(directory, "Consumer.dll");
        File.WriteAllBytes(consumerPath, image.ToArray());
        var incomplete = NeoClrReferenceCatalog.Read(corePath, [consumerPath]);
        if (!Compile(incomplete, "Updated").GetDiagnostics().Any(d => d.ToString().Contains("missing or mismatched native dependency", StringComparison.Ordinal)))
            throw new Exception("missing native dependency did not produce a semantic diagnostic");
        Reject<ArgumentException>(() => NeoClrReferenceCatalog.Read(corePath, [libraryPath, libraryPath]));
        var copy = Path.Combine(directory, "Copy.dll"); File.Copy(libraryPath, copy, true);
        Reject<ArgumentException>(() => NeoClrReferenceCatalog.Read(corePath, [libraryPath, copy]));
        Reject<ArgumentException>(() => NeoClrReferenceCatalog.Read(corePath, [corePath]));
        Reject<IOException>(() => NeoClrReferenceCatalog.Read(corePath, [Path.Combine(directory, "Missing.dll")]));
        File.WriteAllBytes(copy, [1, 2, 3]);
        Reject<InvalidDataException>(() => NeoClrReferenceCatalog.Read(corePath, [copy]));
        Reject<InvalidDataException>(() => NeoClrReferenceCatalog.Read(corePath, [], libraryPath));
        var seed = NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(seedPath));
        Reject<InvalidDataException>(() => first.ValidateSourceOwnership([seed.TypeNames.First()]));
        first.ValidateSourceOwnership(["NoCompeting.Declaration`1"]);
        Console.WriteLine("PASS reference catalog snapshot reuse, replacement, native semantic import/emission, invalid inputs and seed ownership");
    }

    private static void Reject<T>(Action action) where T : Exception
    {
        try { action(); } catch (T) { return; }
        throw new Exception("expected " + typeof(T).Name);
    }
}
