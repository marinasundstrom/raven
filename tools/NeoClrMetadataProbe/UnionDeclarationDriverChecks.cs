using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

namespace NeoClrMetadataProbe;

// Records the current native gate; rejected native compilation is not execution success.
internal static class UnionDeclarationDriverChecks
{
    internal static async Task Run(string driver, string core, string output)
    {
        driver = Path.GetFullPath(driver);
        core = Path.GetFullPath(core);
        output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var evidence = new List<object>();
        var commands = new List<object>();
        foreach (var generic in new[] { false, true })
        {
            var name = generic ? "Generic" : "Plain";
            var union = generic ? "Choice<T>" : "Choice";
            var payload = generic ? "T" : "int";
            var valueType = generic ? "Choice<int>" : "Choice";
            var source = Path.Combine(output, name + ".rvn");
            File.WriteAllText(source, $$"""
                union {{union}} {
                    case Some(value: {{payload}})
                    case None
                }
                func Main() -> int {
                    let value: {{valueType}} = .Some(42)
                    return match value {
                        .Some(let payload) => payload
                        .None => 1
                        _ => 2
                    }
                }
                """);
            var clr = Path.Combine(output, name + ".dll");
            await Command("dotnet", [driver, "--framework", "net10.0", "--emit-core-types-only", "-o", clr, source], 0);
            var executed = await Command("dotnet", ["exec", "--runtimeconfig", Path.ChangeExtension(driver, ".runtimeconfig.json"), clr], 42);
            if (executed.Stdout != "" || executed.Stderr != "") throw new Exception("unexpected CLR output");
            var native = Path.Combine(output, name + ".native.dll");
            var rejected = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "-o", native, source], 1);
            if (!rejected.Stderr.Contains("NEOMETA001") || !rejected.Stderr.Contains("union body get_Value:") ||
                !rejected.Stderr.Contains("lowered expression BoundLiteralExpression") || File.Exists(native))
                throw new Exception("native union gate changed; reassess contracts before updating this expectation");
            evidence.Add(new { name, sourceSha256 = Hash(source), dotnetAssemblySha256 = Hash(clr), dotnetExecuted = true, nativeExecuted = false, nativeOutputPublished = false });
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            scope = "Union declaration discovery baseline; native union execution remains blocked by synthesized union null-literal emission.",
            driverSha256 = Hash(driver), coreSha256 = Hash(core), evidence, commands
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS CLR union execution and explicit native rejection without publication; native execution remains pending");

        async Task<(string Stdout, string Stderr)> Command(string executable, string[] arguments, int expected)
        {
            var start = new ProcessStartInfo(executable) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var arg in arguments) start.ArgumentList.Add(arg);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(60));
            try { await process.WaitForExitAsync(timeout.Token); }
            catch { process.Kill(true); throw; }
            var text = await stdout; var error = await stderr;
            commands.Add(new { executable, arguments, exitCode = process.ExitCode, stdout = text, stderr = error });
            if (process.ExitCode != expected) throw new Exception($"Expected {expected}, got {process.ExitCode}: {text}{error}");
            return (text, error);
        }
        static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
    }
}
