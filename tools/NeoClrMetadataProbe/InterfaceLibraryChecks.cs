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
        string[] paths = ["System/Collections/Comparer.rvn", "System/Collections/EqualityComparer.rvn", "System/Disposable.rvn", "System/Collections/Iterator.rvn"];
        var sources = paths.Select(p => File.ReadAllText(Path.Combine(root, p))).ToArray();
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var reference = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        foreach (var reverse in new[] { false, true })
        {
            var trees = paths.Select((p, i) => SyntaxTree.ParseText(sources[i], path: p))
                .Append(SyntaxTree.ParseText("func Main() -> int => 42", path: "Main.rvn")).ToArray();
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
            var iterator = loaded.GetType("System.Collections.Iterator`1")!.MakeGenericType(typeof(int));
            if (iterator.GetInterfaces().Single().FullName != "System.Disposable" ||
                iterator.GetProperty("Current")!.PropertyType != typeof(int) || !iterator.GetProperty("Current")!.GetMethod!.IsAbstract)
                throw new Exception("CLI iterator contract");
            var iteratorRow = snapshot.MainModule.Types.Single(t => t.Name == "Iterator`1");
            if (iteratorRow.Properties.Single().Name != "Current" || iteratorRow.Properties.Single().GetMethod!.IsStatic)
                throw new Exception("native iterator property association");
            if (!Equals(loaded.EntryPoint!.Invoke(null, null), 42)) throw new Exception("CLI entry");
        }
        for (int i = 0; i < paths.Length; i++) File.WriteAllText(Path.Combine(output, Path.GetFileName(paths[i])), sources[i]);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            sources = paths.Select((p, i) => new { path = p, sha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(sources[i]))) }),
            wholeFiles = true,
            sourceOrders = 2,
            nativeVerify = true,
            nativeEntryResult = 42,
            cliEntryResult = 42,
            inheritedInterface = true,
            abstractProperty = true,
            interfaceDispatch = false,
            entryUsesInterfaces = false,
            fullClassLibrary = false,
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            bootstrap = "host core; declaration loading and projection, not interface dispatch"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS unchanged comparer/disposable/iterator interfaces: CLI/native load and verify in both file orders; independent entry 42, no dispatch claim");
    }
}
