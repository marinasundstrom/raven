using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NamespaceFunctionChecks
{
    internal static async Task Run(string seed, string output, string runtime, string nativeSystem)
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
                func Stop(message: string) {
                    System.Fail(message)
                }
                func Main() -> int {
                    if FAIL { Stop("namespace function failure") }
                    return 42
                }
                """.Replace("FAIL", unread ? "true" : "false");
            var compilation = Compilation.Create("NamespaceCaller", [SyntaxTree.ParseText(source)], [reference],
                CompilationOptions.NeoCLR.WithRuntimeSelfTypeContract(new("NeoCLR.CoreProbe", "System.Runtime.CompilerServices.Self")));
            {
                using var image = new MemoryStream();
                var options = new NeoClrEmitOptions(new("NamespaceCaller", new Version(1, 0, 0, 0)), core,
                    [new NeoClrMetadataDependency(reference, AssemblyDefinition.ReadAssembly(File.ReadAllBytes(seed), expectedExtended: false), core,
                        NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(nativeSystem)))]);
                var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, options);
                if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
                var path = Path.Combine(output, unread ? "Unread.dll" : "Written.dll");
                File.WriteAllBytes(path, image.ToArray());
                foreach (var command in new[] { "verify", "run" })
                {
                    var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                    start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                    start.ArgumentList.Add("--system"); start.ArgumentList.Add(nativeSystem);
                    using var process = Process.Start(start)!;
                    var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                    await process.WaitForExitAsync(); var text = await stdout + await stderr;
                    if (command == "run" && unread ? process.ExitCode == 0 || !text.Contains("namespace function failure", StringComparison.OrdinalIgnoreCase) : process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(text);
                    reports.Add(new { unread, command, exitCode = process.ExitCode, text });
                }
            }
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new {
            seedSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(seed))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            nativeSystemSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(nativeSystem))), cases = reports
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS namespace function import: success 42, dynamic System.Fail diagnostic preserved");
    }
}
