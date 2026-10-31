using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;
using System.Text;
using System.Text.Json.Nodes;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

namespace NeoClrMetadataProbe;

// Ordinary compiler commands exercise owned union declarations on both runtimes.
internal static class UnionDeclarationDriverChecks
{
    internal static async Task Run(string driver, string runtime, string core, string seed, string output)
    {
        driver = Path.GetFullPath(driver);
        runtime = Path.GetFullPath(runtime);
        seed = Path.GetFullPath(seed);
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
                    static func EmptyValue() -> {{union}} => None
                }
                func Read(value: {{valueType}}) -> int {
                    return match value {
                        .Some(let payload) => payload
                        .None => 0
                        _ => 2
                    }
                }
                func Main() -> int {
                    var value: {{valueType}} = .Some(42)
                    let copy = value
                    value = {{valueType}}.EmptyValue()
                    System.Console.WriteLine(copy.ToString())
                    System.Console.WriteLine(value.ToString())
                    return Read(copy) + Read(value)
                }
                """);
            var clr = Path.Combine(output, name + ".dll");
            await Command("dotnet", [driver, "--framework", "net10.0", "--emit-core-types-only", "-o", clr, source], 0);
            var executed = await Command("dotnet", ["exec", "--runtimeconfig", Path.ChangeExtension(driver, ".runtimeconfig.json"), clr], 42);
            if (executed.Stdout.Replace("\r\n", "\n") != "Choice.Some(42)\nChoice.None\n" || executed.Stderr != "") throw new Exception("unexpected CLR output");
            var native = Path.Combine(output, name + ".native.dll");
            await Command("dotnet", [driver, "neoclr", "--core-reference", core, "--runtime-seed", seed, "-o", native, source], 0);
            await Command(runtime, ["verify", native, "--system", seed], 0);
            var nativeRun = await Command(runtime, ["run", native, "--system", seed], 42);
            if (nativeRun.Stdout != executed.Stdout || nativeRun.Stderr != "") throw new Exception("unexpected native output");
            var definition = NeoCLR.Metadata.Experimental.Model.AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(native));
            var carrier = definition.MainModule.Types.Single(t => t.Name == (generic ? "Choice`1" : "Choice"));
            var attributes = carrier.CustomAttributes;
            if (attributes.Count(a => a.AttributeType.Name == "UnionAttribute") != 1 ||
                attributes.Count(a => a.AttributeType.Name == "RavenUnionCaseAttribute") != 2)
                throw new Exception("union contract missing from native metadata");
            var librarySource = Path.Combine(output, name + ".Library.rvn");
            File.WriteAllText(librarySource, $$"""
                public union {{union}} {
                    case Some(value: {{payload}})
                    case None
                    static func EmptyValue() -> {{union}} => None
                }
                """);
            var consumerSource = Path.Combine(output, name + ".Consumer.rvn");
            File.WriteAllText(consumerSource, $$"""
                func Read(value: {{valueType}}) -> int {
                    return match value {
                        .Some(let payload) => payload
                        .None => 0
                        _ => 1
                    }
                }
                func Main() -> int {
                    var value: {{valueType}} = .Some(42)
                    let copy = value
                    value = {{valueType}}.EmptyValue()
                    return Read(copy) + Read(value)
                }
                """);
            foreach (var target in new[] { "dotnet", "native" })
            {
                var library = Path.Combine(output, name + ".Library." + target + ".dll");
                var consumer = Path.Combine(output, name + ".Consumer." + target + ".dll");
                if (target == "native")
                {
                    await Command("dotnet", [driver, "neoclr", "--core-reference", core, "--runtime-seed", seed, "--library", "-o", library, librarySource], 0);
                    await Command("dotnet", [driver, "neoclr", "--core-reference", core, "--runtime-seed", seed, "--reference", library, "-o", consumer, consumerSource], 0);
                    await Command(runtime, ["verify", consumer, "--module", library, "--system", seed], 0);
                    await Command(runtime, ["run", consumer, "--module", library, "--system", seed], 42);
                    var malformed = JsonNode.Parse(RuntimeAssemblyContainer.Read(File.ReadAllBytes(library)))!;
                    var declaration = malformed["types"]!.AsArray().Single(t => t!["origin"]!["name"]!.GetValue<string>() == (generic ? "Choice`1" : "Choice"));
                    var caseAttributes = declaration!["custom_attributes"]!.AsArray().Where(a => a!["arguments"]!.AsArray().Count == 3).ToArray();
                    caseAttributes[1]!["arguments"]![2]!["Int32"] = 0;
                    var badLibrary = Path.Combine(output, name + ".InvalidCases.dll");
                    File.WriteAllBytes(badLibrary, RuntimeAssemblyContainer.WriteBinary(Encoding.UTF8.GetBytes(malformed.ToJsonString()),
                        AssemblyDefinition.ReadAssembly(File.ReadAllBytes(core), false).Identity));
                    var rejectedOutput = Path.Combine(output, name + ".Rejected.dll");
                    var rejection = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "--runtime-seed", seed,
                        "--reference", badLibrary, "-o", rejectedOutput, consumerSource], 1);
                    if (!rejection.Stderr.Contains("conflicting native union cases") || File.Exists(rejectedOutput))
                        throw new Exception("invalid union metadata did not reject before publication");
                }
                else
                {
                    await Command("dotnet", [driver, "--framework", "net10.0", "--emit-core-types-only", "--output-type", "classlib", "-o", library, librarySource], 0);
                    await Command("dotnet", [driver, "--framework", "net10.0", "--emit-core-types-only", "--refs", library, "-o", consumer, consumerSource], 0);
                    await Command("dotnet", ["exec", "--runtimeconfig", Path.ChangeExtension(driver, ".runtimeconfig.json"), consumer], 42);
                }
                evidence.Add(new { name, target, librarySourceSha256 = Hash(librarySource), consumerSourceSha256 = Hash(consumerSource), librarySha256 = Hash(library), consumerSha256 = Hash(consumer), separateLibraryExecuted = true });
            }
            evidence.Add(new { name, sourceSha256 = Hash(source), dotnetAssemblySha256 = Hash(clr), nativeAssemblySha256 = Hash(native), dotnetExecuted = true, nativeExecuted = true });
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            scope = "Owned and separately compiled union execution, native semantic import and attribute preservation.",
            driverSha256 = Hash(driver),
            runtimeSha256 = Hash(runtime),
            coreSha256 = Hash(core),
            seedSha256 = Hash(seed),
            evidence,
            commands
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS owned and separately compiled plain/generic unions on CLR and NeoCLR");

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
