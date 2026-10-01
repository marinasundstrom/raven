using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class InterfaceLibraryChecks
{
    internal static async Task Run(string root, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        string[] paths = ["System/Collections/Comparer.rvn", "System/Collections/EqualityComparer.rvn", "System/Disposable.rvn", "System/Collections/Iterator.rvn", "System/Collections/Iterable.rvn"];
        var sources = paths.Select(p => File.ReadAllText(Path.Combine(root, p))).ToArray();
        const string entry = """
            static class ReferenceFlow {
                static func Empty() -> System.Collections.Iterator<int>? => default(System.Collections.Iterator<int>)
                static func Echo(value: System.Collections.Iterator<int>?) -> System.Collections.Iterator<int>? => value
            }
            func Main() -> int {
                let slots: System.Collections.Iterator<int>?[] = [ReferenceFlow.Empty()]
                let value = ReferenceFlow.Echo(slots[0])
                slots[0] = value
                return 42
            }
            """;
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var reference = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        foreach (var reverse in new[] { false, true })
        {
            var trees = paths.Select((p, i) => SyntaxTree.ParseText(sources[i], path: p))
                .Append(SyntaxTree.ParseText(entry, path: "Main.rvn")).ToArray();
            if (reverse) Array.Reverse(trees);
            var name = "InterfaceLibrary" + (reverse ? "Reverse" : "Forward");
            var compilation = Compilation.Create(name, trees, [reference], new CompilationOptions(OutputKind.ConsoleApplication));
            using var native = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native, new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!emitted.Success) throw new Exception(string.Join("; ", emitted.Diagnostics));
            var snapshot = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            if (snapshot.MainModule.Types.Count(t => t.Name is "Comparer`1" or "EqualityComparer`1") != 2) throw new Exception("missing interface declarations");
            foreach (var type in snapshot.MainModule.Types.Where(t => t.Name is "Comparer`1" or "EqualityComparer`1"))
                if ((type.Attributes & 0x20) == 0 || type.Methods.Any(m => m.IsStatic || (m.Attributes & 0x440) != 0x440))
                    throw new Exception("projected interface contract");
            var path = Path.Combine(output, name + ".dll"); File.WriteAllBytes(path, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                using var process = Process.Start(start)!; var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync(); var text = await stdout + await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(command + ": " + text);
            }
            using var cli = new MemoryStream(); var result = compilation.Emit(cli);
            if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
            var loaded = Assembly.Load(cli.ToArray());
            foreach (var namePart in new[] { "Comparer`1", "EqualityComparer`1" })
            {
                var type = loaded.GetType("System.Collections." + namePart)!;
                if (!type.IsInterface || type.GetMethods().Any(m => !m.IsAbstract || !m.IsVirtual || m.GetMethodBody() is not null))
                    throw new Exception("CLI interface contract");
            }
            if (loaded.GetType("ReferenceFlow")!.GetMethod("Empty")!.Invoke(null, null) is not null)
                throw new Exception("interface default must be null");
            var iterable = loaded.GetType("System.Collections.Iterable`1")!.MakeGenericType(typeof(int));
            if (iterable.GetMethod("GetIterator")!.ReturnType != loaded.GetType("System.Collections.Iterator`1")!.MakeGenericType(typeof(int)))
                throw new Exception("interface-valued signature");
            var iterator = loaded.GetType("System.Collections.Iterator`1")!.MakeGenericType(typeof(int));
            if (iterator.GetInterfaces().Single().FullName != "System.Disposable" ||
                iterator.GetProperty("Current")!.PropertyType != typeof(int) || !iterator.GetProperty("Current")!.GetMethod!.IsAbstract)
                throw new Exception("CLI iterator contract");
            var iteratorRow = snapshot.MainModule.Types.Single(t => t.Name == "Iterator`1");
            if (iteratorRow.Properties.Single().Name != "Current" || iteratorRow.Properties.Single().GetMethod!.IsStatic)
                throw new Exception("native iterator property association");
            if (!Equals(loaded.EntryPoint!.Invoke(null, null), 42)) throw new Exception("CLI entry");
        }
        File.WriteAllText(Path.Combine(output, "Main.rvn"), entry);
        for (int i = 0; i < paths.Length; i++) File.WriteAllText(Path.Combine(output, Path.GetFileName(paths[i])), sources[i]);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            sources = paths.Select((p, i) => new { path = p, sha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(sources[i]))) }),
            consumerSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(entry))),
            wholeFiles = true,
            sourceOrders = 2,
            nativeVerify = true,
            nativeEntryResult = 42,
            cliEntryResult = 42,
            inheritedInterface = true,
            abstractProperty = true,
            interfaceDispatch = false,
            entryUsesInterfaces = true,
            interfaceDefaultStorage = true,
            fullClassLibrary = false,
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            bootstrap = "host core; declaration loading, reference/default storage and projection, not interface dispatch"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS unchanged comparer/disposable/iterator interfaces: CLI/native load and verify in both file orders; interface reference/default storage entry 42, no dispatch claim");
    }
}
