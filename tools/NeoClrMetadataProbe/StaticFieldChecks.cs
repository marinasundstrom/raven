using System.Diagnostics;
using System.Reflection;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class StaticFieldChecks
{
    internal static async Task Run(string corePath, string runtime, string output)
    {
        Directory.CreateDirectory(output);
        var bytes = File.ReadAllBytes(corePath);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var core = AssemblyDefinition.ReadAssembly(bytes, false).Identity;
        const string declaration = """
            public static class HeaderNames {
                private static var host: string?
                public static field Count: int
                public static val Host: string {
                    get {
                        if host is string value { return value }
                        host = "host"
                        Count = Count + 1
                        return host!
                    }
                }
            }
            public class Cache<T> {
                public static field Count: int
                public static func Read() -> int => Count
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
        var library = Emit(Create("StaticLibrary", declaration, OutputKind.DynamicallyLinkedLibrary));
        var libraryPath = Path.Combine(output, "StaticLibrary.dll");
        File.WriteAllBytes(libraryPath, library);
        var snapshot = RuntimeAssemblyContainer.ReadCliProjection(library);
        var fields = snapshot.MainModule.Types.Single(t => t.Name == "HeaderNames").Fields;
        if (fields.Count != 2 || fields.Any(f => (f.Attributes & (ushort)FieldAttributes.Static) == 0))
            throw new Exception("Static field metadata lost");
        var reference = NeoClrMetadataReference.ReadAssembly(library, bootstrap);
        var consumer = Create("StaticConsumer", """
            func Main() -> int {
                let first = HeaderNames.Host
                let second = HeaderNames.Host
                if HeaderNames.Count != 1 { return 1 }
                Cache<int>.Count = 40
                Cache<string>.Count = 2
                return Cache<int>.Read() + Cache<string>.Read()
            }
            """, OutputKind.ConsoleApplication, reference);
        var type = consumer.GetTypeByMetadataName("HeaderNames")!;
        if (!type.GetMembers("Count").OfType<IFieldSymbol>().Single().IsStatic)
            throw new Exception("Imported static symbol lost");
        var appPath = Path.Combine(output, "StaticConsumer.dll");
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
        var rejected = false;
        try { _ = Emit(Create("StaticInitializer", declaration.Replace("public static field Count: int", "public static field Count: int = 7"), OutputKind.DynamicallyLinkedLibrary)); }
        catch (Exception error) when (error.Message.Contains("static field initializers require runtime type initialization")) { rejected = true; }
        if (!rejected) throw new Exception("Static initializer was silently dropped");
        File.WriteAllText(Path.Combine(output, "StaticFields.rvn"), declaration);
        Console.WriteLine("PASS static metadata/import, generic separation and separate-library execution");
    }
}
