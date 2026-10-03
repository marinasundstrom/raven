using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;
using NeoCLR.Metadata.Experimental.Model;

namespace NeoClrMetadataProbe;

internal static class ValueOverrideDriverChecks
{
    internal static async Task Run(string driver, string runtime, string core, string seed, string output)
    {
        driver = Path.GetFullPath(driver); runtime = Path.GetFullPath(runtime);
        core = Path.GetFullPath(core); seed = Path.GetFullPath(seed); output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var evidence = new List<object>(); var commands = new List<object>();
        var configurationSource = Path.Combine(output, "Configuration.rvn");
        File.WriteAllText(configurationSource, "func Main() -> int => 42\n");
        var configurationOutput = Path.Combine(output, "Configuration.dll");
        var missingCore = await Command("dotnet", [driver, "neoclr", "--runtime-seed", seed, "-o", configurationOutput, configurationSource], 1);
        if (!missingCore.Contains("requires --core-reference") || File.Exists(configurationOutput)) throw new Exception("implicit core accepted for runtime seed");
        var ownership = Path.Combine(output, "ownership.json");
        File.WriteAllText(ownership, JsonSerializer.Serialize(new
        {
            version = 1,
            libraries = new[] { new { assemblyName = "SourceCollections", sources = new[] { "Iterable.rvn", "Iterator.rvn" }, types = new[] { "System.Collections.Iterable`1", "System.Collections.Iterator`1" } } },
            iteration = new { assemblyName = "SourceCollections", iterableTypeName = "System.Collections.Iterable`1", iteratorTypeName = "System.Collections.Iterator`1" }
        }));
        var duplicate = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "--runtime-seed", seed, "--bootstrap-ownership", ownership, "-o", configurationOutput, configurationSource], 1);
        if (!duplicate.Contains("Runtime seed duplicates a source-owned declaration") || File.Exists(configurationOutput)) throw new Exception("seed ownership conflict not rejected before publication");
        foreach (var generic in new[] { false, true })
        {
            var name = generic ? "Generic" : "Plain";
            var type = generic ? "Display<T>" : "Display";
            var constructed = generic ? "Display<int>" : "Display";
            var source = Path.Combine(output, name + ".rvn");
            File.WriteAllText(source, $$"""
                struct {{type}} {
                    override func ToString() -> string => "native override"
                }
                func Main() -> int {
                    let value = {{constructed}}()
                    System.Console.WriteLine(value.ToString())
                    return 42
                }
                """);
            var clr = Path.Combine(output, name + ".clr.dll");
            // Existing Raven language policy requires exact override return nullability.
            // Host Object returns string?; the retained native bootstrap returns string.
            // Record this incompatibility instead of rewriting the source for a passing CLR claim.
            var clrFailure = await Command("dotnet", [driver, "--framework", "net10.0", "--emit-core-types-only", "-o", clr, source], 1);
            if (!clrFailure.Contains("RAV0307") || File.Exists(clr)) throw new Exception("reassess override nullability baseline");
            var native = Path.Combine(output, name + ".native.dll");
            await Command("dotnet", [driver, "neoclr", "--core-reference", core, "--runtime-seed", seed, "-o", native, source], 0);
            var snapshot = AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(native));
            var method = snapshot.MainModule.Types.Single(t => t.Name.StartsWith("Display")).Methods.Single(m => m.Name == "ToString");
            if ((method.Attributes & 0x540) != 0x40) throw new Exception("native output lost override flags");
            await Command(runtime, ["verify", native, "--system", seed], 0);
            await Command(runtime, ["run", native, "--system", seed], 42, "native override");
            var rejected = Path.Combine(output, name + ".missing-seed.dll");
            var failure = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "-o", rejected, source], 1);
            if (!failure.Contains("runtime slot binding") || File.Exists(rejected)) throw new Exception("missing seed did not reject before publication");
            evidence.Add(new { name, sourceSha256 = Hash(source), nativeSha256 = Hash(native), dotnetExecuted = false, dotnetDiagnostic = "RAV0307", nativeExecuted = true });
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            scope = "Native driver value ToString overrides; CLR source rejected by existing return-nullability policy; source unions and boxed compiler calls remain pending",
            driverSha256 = Hash(driver), runtimeSha256 = Hash(runtime), coreSha256 = Hash(core), seedSha256 = Hash(seed), evidence, commands
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS native driver value overrides; CLR nullability mismatch recorded; explicit seed binding and failed publication checks");

        async Task<string> Command(string executable, string[] arguments, int expected, string? stdoutExpected = null)
        {
            var start = new ProcessStartInfo(executable) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var argument in arguments) start.ArgumentList.Add(argument);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(60));
            try { await process.WaitForExitAsync(timeout.Token); } catch { process.Kill(true); throw; }
            var text = await stdout; var error = await stderr;
            commands.Add(new { executable, arguments, exitCode = process.ExitCode, stdout = text, stderr = error });
            if (process.ExitCode != expected || stdoutExpected is not null && (text.Trim() != stdoutExpected || error != ""))
                throw new Exception($"Expected {expected}: {text}{error}");
            return text + error;
        }
        static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
    }
}
