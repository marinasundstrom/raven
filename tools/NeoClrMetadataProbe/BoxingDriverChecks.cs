using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

namespace NeoClrMetadataProbe;

internal static class BoxingDriverChecks
{
    internal static async Task Run(string driver, string runtime, string core, string seed, string output)
    {
        driver = Path.GetFullPath(driver); runtime = Path.GetFullPath(runtime);
        core = Path.GetFullPath(core); seed = Path.GetFullPath(seed); output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var source = Path.Combine(output, "Box.rvn");
        File.WriteAllText(source, """
            public static class Converter {
                static func Box<T>(value: T) -> object => value
            }
            func Main() -> int {
                let integer = Converter.Box(42)
                let text = Converter.Box("value")
                System.Console.WriteLine("boxed")
                return 42
            }
            """);
        var commands = new List<object>();
        foreach (var native in new[] { false, true })
        {
            var assembly = Path.Combine(output, native ? "Box.native.dll" : "Box.clr.dll");
            string[] args = native
                ? [driver, "neoclr", "--core-reference", core, "--runtime-seed", seed, "-o", assembly, source]
                : [driver, "--framework", "net10.0", "--emit-core-types-only", "-o", assembly, source];
            await Command("dotnet", args, 0);
            if (native) await Command(runtime, ["verify", assembly, "--system", seed], 0);
            await Command(native ? runtime : "dotnet", native
                ? ["run", assembly, "--system", seed]
                : ["exec", "--runtimeconfig", Path.ChangeExtension(driver, ".runtimeconfig.json"), assembly], 42, "boxed");
        }
        var rejected = Path.Combine(output, "MissingSeed.dll");
        var error = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "-o", rejected, source], 1);
        if (!error.Contains("unregistered dependency type: class object") || File.Exists(rejected)) throw new Exception("missing boxing seed was not rejected before publication");
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            scope = "Dual-target ordinary-command boxing smoke; detailed value/identity assertions live in C# metadata and CLR conversion tests, not this discarded-result smoke.",
            driverSha256 = Hash(driver), runtimeSha256 = Hash(runtime), coreSha256 = Hash(core), seedSha256 = Hash(seed), sourceSha256 = Hash(source), commands
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS dual-target boxing smoke and missing-seed publication guard");

        async Task<string> Command(string executable, string[] arguments, int expected, string? expectedOutput = null)
        {
            var start = new ProcessStartInfo(executable) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var arg in arguments) start.ArgumentList.Add(arg);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(60));
            try { await process.WaitForExitAsync(timeout.Token); } catch { process.Kill(true); throw; }
            var text = await stdout; var error = await stderr;
            commands.Add(new { executable, arguments, exitCode = process.ExitCode, stdout = text, stderr = error });
            if (process.ExitCode != expected || expectedOutput is not null && (text.Trim() != expectedOutput || error.Length != 0))
                throw new Exception($"Expected {expected}, got {process.ExitCode}: {text}{error}");
            return text + error;
        }
        static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
    }
}
