using System.Diagnostics;
using System.Reflection;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ReadonlyFieldChecks
{
    internal static async Task Run(string corePath, string runtime, string output)
    {
        Directory.CreateDirectory(output);
        var bytes = File.ReadAllBytes(corePath);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var core = AssemblyDefinition.ReadAssembly(bytes, false).Identity;
        const string declaration = """
            public class ReadonlyFields {
                public readonly field Number: int = 7
                internal readonly field Wide: long = 5000000000L
                private readonly field active: bool = true
                init(value: int) { Number = value }
                func Read() -> int {
                    if !active || Wide != 5000000000L { return 1 }
                    return Number
                }
            }
            """;
        Compilation Create(string name, string source, OutputKind kind, NeoClrMetadataReference? dependency = null)
            => Compilation.Create(name, [SyntaxTree.ParseText(source)],
                dependency is null ? [bootstrap.Reference] : [bootstrap.Reference, dependency],
                CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name)
                    .WithOutputKind(kind).WithRuntimeTypeOfContract(null));
        byte[] Emit(Compilation compilation, NeoClrMetadataReference? dependency = null)
        {
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new(compilation.AssemblyName!, new(1, 0, 0, 0)), core,
                    dependency is null ? [] : [new NeoClrMetadataDependency(dependency, core)], bootstrapReference: bootstrap.Reference));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            return image.ToArray();
        }
        var library = Emit(Create("ReadonlyLibrary", declaration, OutputKind.DynamicallyLinkedLibrary));
        var libraryPath = Path.Combine(output, "ReadonlyLibrary.dll");
        File.WriteAllBytes(libraryPath, library);
        var snapshot = RuntimeAssemblyContainer.ReadCliProjection(library);
        var fields = snapshot.MainModule.Types.Single(t => t.Name == "ReadonlyFields").Fields;
        if (fields.Count != 3 || fields.Any(f => (f.Attributes & (ushort)FieldAttributes.InitOnly) == 0))
            throw new Exception("Readonly field metadata lost");
        var reference = NeoClrMetadataReference.ReadAssembly(library, bootstrap);
        var consumer = Create("ReadonlyConsumer", "func Main() -> int => ReadonlyFields(42).Read()", OutputKind.ConsoleApplication, reference);
        var type = consumer.GetTypeByMetadataName("ReadonlyFields")!;
        if (type.GetMembers("Number").OfType<IFieldSymbol>().Single().IsReadOnly != true)
            throw new Exception("Imported readonly symbol lost");
        var appPath = Path.Combine(output, "ReadonlyConsumer.dll");
        File.WriteAllBytes(appPath, Emit(consumer, reference));
        foreach (var command in new[] { "verify", "run" })
        {
            var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var arg in new[] { command, appPath, "--module", libraryPath }) start.ArgumentList.Add(arg);
            if (command == "run") { start.ArgumentList.Add("--execution"); start.ArgumentList.Add("interpreter"); }
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            await process.WaitForExitAsync();
            if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(await stdout + await stderr);
        }
        foreach (var source in new[] {
            declaration.Replace("func Read() -> int {", "func Read() -> int { Number = 9"),
            "func Main() { let value = ReadonlyFields(42)\n value.Number = 9 }"
        })
        {
            var rejected = Create("RejectedReadonly", source, OutputKind.DynamicallyLinkedLibrary,
                source.StartsWith("func Main") ? reference : null);
            if (!rejected.GetDiagnostics().Any(d => d.Id == "RAV0191"))
                throw new Exception("Readonly mutation was not rejected");
        }
        File.WriteAllText(Path.Combine(output, "ReadonlyFields.rvn"), declaration);
        Console.WriteLine("PASS explicit readonly initialization, native metadata/import, separate-library execution and mutation rejection");
    }
}
