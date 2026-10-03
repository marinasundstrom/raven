using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

using MetadataReference = Raven.CodeAnalysis.MetadataReference;

namespace NeoClrMetadataProbe;

internal static class NativeCatalogChecks
{
    internal static void Run(string corePath, AssemblyIdentity core)
    {
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(File.ReadAllBytes(corePath));
        var builder = new AssemblyBuilder(core, new AssemblyIdentity("CatalogTest.Core", new Version(1, 0, 0, 0)));
        var method = builder.AddFunction("Collision");
        method.LoadConstant(42);
        method.Return();
        var native = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteBinary(builder), bootstrap);
        foreach (var references in new MetadataReference[][]
        {
            [bootstrap.Reference, native],
            [native, bootstrap.Reference]
        })
        {
            var compilation = Compilation.Create("ConflictingCatalog",
                [SyntaxTree.ParseText("func Main() -> int { return 42 }")], references,
                CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (!errors.Any(d => d.Id == "RAVT003" && d.GetMessage().Contains("conflicting metadata snapshots", StringComparison.Ordinal)))
                throw new Exception("bootstrap/native identity collision did not produce a catalog diagnostic");
            using var output = new MemoryStream();
            output.Write([17, 23, 42]);
            output.Position = 1;
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, output,
                new(new("ConflictingCatalog", new Version(1, 0, 0, 0)), core, [new(native, core)]));
            if (result.Success || !result.Diagnostics.Any(d => d.Id == "RAVT003") ||
                output.Position != 1 || !output.ToArray().SequenceEqual(new byte[] { 17, 23, 42 }))
                throw new Exception("conflicting metadata catalog published output");
        }
    }
}
