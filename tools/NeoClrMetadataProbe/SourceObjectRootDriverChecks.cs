using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

using NeoCLR.Metadata.Experimental;

namespace NeoClrMetadataProbe;

internal static class SourceObjectRootDriverChecks
{
    internal static async Task Run(string core, string driver, string runtime, string source, string directory)
    {
        if (Directory.Exists(directory)) throw new IOException("Use a fresh evidence directory.");
        Directory.CreateDirectory(directory);
        var commands = new List<object>();
        async Task<string> Execute(string executable, string[] arguments, bool success, string? diagnostic = null)
        {
            var start = new ProcessStartInfo(executable) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var argument in arguments) start.ArgumentList.Add(argument);
            using var process = Process.Start(start) ?? throw new Exception("Could not start " + executable);
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            using var timeout = new CancellationTokenSource(TimeSpan.FromMinutes(2));
            try { await process.WaitForExitAsync(timeout.Token); }
            catch { process.Kill(entireProcessTree: true); throw; }
            var output = await stdout;
            var errors = await stderr;
            commands.Add(new { executable, arguments, exitCode = process.ExitCode, stdout = output, stderr = errors });
            if ((process.ExitCode == 0) != success || diagnostic is not null && !(output + errors).Contains(diagnostic, StringComparison.Ordinal))
                throw new Exception($"Unexpected command result: {executable}\n{output}\n{errors}");
            return output + errors;
        }
        string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path))).ToLowerInvariant();
        var artifact = Path.Combine(directory, "RootLibrary.dll");
        var passed = false;
        try
        {
            await Execute("dotnet", [driver, "neoclr", "--library", "--source-object-root", "--bootstrap-intrinsics",
                "--core-reference", core, "-o", artifact, source], true);
            using var metadata = JsonDocument.Parse(RuntimeAssemblyContainer.Read(File.ReadAllBytes(artifact)));
            var root = metadata.RootElement;
            var module = root.GetProperty("name").GetString();
            var display = root.GetProperty("functions").EnumerateArray().Single(f =>
                f.TryGetProperty("origin", out var origin) && origin.GetProperty("name").GetString() == "Display").GetProperty("name").GetString();
            var app = Path.Combine(directory, "App.neoil");
            var seed = Path.Combine(directory, "System.neoil");
            File.WriteAllText(app, $".module App\n.references ({module})\n.entry Main\n.function Main() -> String\ncall {display}()\nret\n.end\n");
            File.WriteAllText(seed, ".module System\n.references ()\n");
            var run = await Execute(runtime, ["run", app, "--module", artifact, "--system", seed, "--object-root", artifact, "--show-result"], true);
            if (run != "=> String(\"source root override\")\n") throw new Exception("Unexpected runtime result: " + run);
            await Execute(runtime, ["verify", app, "--module", artifact, "--system", seed, "--object-root", artifact], true);
            await Execute(runtime, ["disassemble", artifact, Path.Combine(directory, "RootLibrary.disassembly.txt")], true);
            var originalHash = Hash(artifact);
            await Execute("dotnet", [driver, "neoclr", "--library", "--source-object-root", "--core-reference", core, "-o", artifact, source], false, "Output already exists");
            if (Hash(artifact) != originalHash) throw new Exception("Existing artifact changed");
            foreach (var scenario in new[] { "missing-core", "missing-library", "duplicate-option", "missing-root", "generic-owner", "extra-slot" })
            {
                var output = Path.Combine(directory, scenario + ".dll");
                var input = source;
                var args = new List<string> { driver, "neoclr", "--source-object-root", "-o", output };
                if (scenario != "missing-core") args.AddRange(["--core-reference", core, "--bootstrap-intrinsics"]);
                if (scenario != "missing-library") args.Add("--library");
                if (scenario == "duplicate-option") args.Add("--source-object-root");
                if (scenario is "missing-root" or "generic-owner" or "extra-slot")
                {
                    input = Path.Combine(directory, scenario + ".rvn");
                    var text = File.ReadAllText(source);
                    File.WriteAllText(input, scenario switch
                    {
                        "missing-root" => "public class Empty { }",
                        "generic-owner" => text + "\npublic class Box<T> { }",
                        _ => text.Replace("protected init()", "virtual func Extra() -> int => 1\n    protected init()", StringComparison.Ordinal)
                    });
                }
                args.Add(input);
                await Execute("dotnet", args.ToArray(), false);
                if (File.Exists(output)) throw new Exception("Failed compilation published " + output);
            }
            passed = true;
            Console.WriteLine("PASS driver source Object root emission, runtime execution and publication controls");
        }
        finally
        {
            File.WriteAllText(Path.Combine(directory, "evidence.json"), JsonSerializer.Serialize(new
            {
                passed,
                scope = "driver-produced root library with neoIL host; Raven imported-root consumer remains pending",
                inputs = new[] { core, driver, runtime, source }.Select(path => new { path, sha256 = Hash(path) }),
                artifactSha256 = File.Exists(artifact) ? Hash(artifact) : null,
                commands
            }, new JsonSerializerOptions { WriteIndented = true }));
        }
    }
}
