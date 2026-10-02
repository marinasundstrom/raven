using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ReservedStorageChecks
{
    internal static async Task Run(string seed, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var identity = AssemblyName.GetAssemblyName(seed);
        var core = new AssemblyIdentity(identity.Name!, identity.Version!, identity.CultureName ?? "", Convert.ToHexString(identity.GetPublicKeyToken() ?? []));
        var reference = MetadataReference.CreateFromFile(seed);
        var reports = new List<object>();
        foreach (var unread in new[] { false, true })
        {
            var source = """
                func Reserve<T>(length: int) -> T[] {
                    return System.Runtime.CompilerServices.CheckedStorage.Reserve<T>(length)
                }
                func Main() -> int {
                    let values = Reserve<int>(2)
                    values[0] = 42
                    return values[INDEX]
                }
                """.Replace("INDEX", unread ? "1" : "0");
            var compilation = Compilation.Create("ReservedStorage", [SyntaxTree.ParseText(source)], [reference],
                CompilationOptions.NeoCLR.WithRuntimeSelfTypeContract(new("NeoCLR.CoreProbe", "System.Runtime.CompilerServices.Self")));
            foreach (var disabled in new[] { true, false })
            {
                using var image = new MemoryStream();
                var options = new NeoClrEmitOptions(new("ReservedStorage", new Version(1, 0, 0, 0)), core, [], bootstrapReference: disabled ? null : reference);
                var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, options);
                if (disabled)
                {
                    if (result.Success || image.Length != 0 || !result.Diagnostics.Any(d => d.Id == "NEOMETA001")) throw new Exception("bootstrap intrinsic enabled by default");
                    continue;
                }
                if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
                var path = Path.Combine(output, unread ? "Unread.dll" : "Written.dll");
                File.WriteAllBytes(path, image.ToArray());
                foreach (var command in new[] { "verify", "run" })
                {
                    var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                    start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                    using var process = Process.Start(start)!;
                    var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                    await process.WaitForExitAsync(); var text = await stdout + await stderr;
                    if (command == "run" && unread ? process.ExitCode == 0 || !text.Contains("uninitialized", StringComparison.OrdinalIgnoreCase) : process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(text);
                    reports.Add(new { unread, command, exitCode = process.ExitCode, text });
                }
            }
            using var rejected = new MemoryStream();
            var invalid = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, rejected, new(new("ReservedStorage", new Version(1, 0, 0, 0)), core, [], bootstrapReference: MetadataReference.CreateFromFile(seed)));
            if (invalid.Success || rejected.Length != 0 || !invalid.Diagnostics.Any(d => d.Id == "NEOMETA002")) throw new Exception("unregistered seed admitted");
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new {
            seedSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(seed))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            bootstrapOptInRequired = true, unregisteredSeedRejected = true, cases = reports
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS Raven reservation: generic helper returns 42; unread slot faults; explicit registered seed required");
    }
}
