using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

namespace NeoClrMetadataProbe;

// Executes ordinary compiler commands, with no Compilation or emitter API shortcuts.
internal static class DualTargetDriverChecks
{
    internal static async Task Run(string driver, string runtime, string output, bool inventory)
    {
        driver = Path.GetFullPath(driver); runtime = Path.GetFullPath(runtime); output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        const string library = """
            namespace DriverContracts
            public interface Value<T> { val Current: T { get; } }
            public interface MutableValue<T> : Value<T> { func Set(value: T) }
            public class Box<T> : MutableValue<T> {
                public field stored: T
                public init(value: T) { stored = value }
                public val Current: T => stored
                public func Set(value: T) { stored = value }
            }
            """;
        const string consumer = """
            import DriverContracts.*
            func Read(value: Value<int>) -> int => value.Current
            func Main() -> int {
                let box = Box<int>(1)
                let alias = box
                let mutable: MutableValue<int> = box
                mutable.Set(41)
                if alias.stored != 41 { return 2 }
                alias.stored = 42
                if box.Current != 42 { return 3 }
                return Read(mutable)
            }
            """;
        const string hello = """
            func Greet() { System.Console.WriteLine("Hello World") }
            func Main() -> int { Greet(); return 42 }
            """;
        var commands = new List<object>();
        var cases = new List<object>();
        var passed = true;
        foreach (var native in new[] { false, true })
        {
            var target = native ? "neoclr" : "dotnet";
            var directory = Path.Combine(output, target); Directory.CreateDirectory(directory);
            foreach (var scenario in new[] { "hello", "library" })
            {
                try
                {
                    var mainSource = Path.Combine(directory, scenario + ".rvn");
                    var app = Path.Combine(directory, scenario + ".dll");
                    File.WriteAllText(mainSource, scenario == "hello" ? hello : consumer);
                    string? dependency = null;
                    if (scenario == "library")
                    {
                        var source = Path.Combine(directory, "Contracts.rvn");
                        dependency = Path.Combine(directory, "Contracts.dll");
                        File.WriteAllText(source, library);
                        await Compile(source, dependency, true, null);
                        File.Delete(source); // The consumer must import the artifact, not reuse source.
                    }
                    await Compile(mainSource, app, false, dependency);
                    if (native)
                        await Command(runtime, ["verify", app, .. dependency is null ? Array.Empty<string>() : new[] { "--module", dependency }], 0);
                    var result = native
                        ? await Command(runtime, ["run", app, .. dependency is null ? Array.Empty<string>() : new[] { "--module", dependency }], 42)
                        : await Command("dotnet", ["exec", "--runtimeconfig", Path.ChangeExtension(driver, ".runtimeconfig.json"), app], 42);
                    var expected = scenario == "hello" ? "Hello World\n" : "";
                    if (result.Stdout.Replace("\r\n", "\n") != expected || result.Stderr != "")
                        throw new Exception("unexpected stdout/stderr: " + result.Stdout + result.Stderr);
                    cases.Add(new { target, scenario, passed = true, error = (string?)null, appSha256 = Hash(app), dependencySha256 = dependency is null ? null : Hash(dependency) });
                }
                catch (Exception error)
                {
                    passed = false;
                    cases.Add(new { target, scenario, passed = false, error = error.Message });
                }
            }
            async Task Compile(string source, string destination, bool isLibrary, string? reference)
            {
                var args = new List<string> { driver };
                if (native)
                {
                    args.Add("neoclr");
                    if (isLibrary) args.Add("--library");
                    if (reference is not null) args.AddRange(["--reference", reference]);
                }
                else
                {
                    args.AddRange(["--framework", "net10.0", "--emit-core-types-only"]);
                    if (isLibrary) args.AddRange(["--output-type", "classlib"]);
                    if (reference is not null) args.AddRange(["--refs", reference]);
                }
                args.AddRange(["-o", destination, source]);
                await Command("dotnet", args.ToArray(), 0);
            }
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            passed,
            driverSha256 = Hash(driver),
            runtimeSha256 = Hash(runtime),
            runtimeConfigSha256 = Hash(Path.ChangeExtension(driver, ".runtimeconfig.json")),
            sourceSha256 = new { hello = TextHash(hello), library = TextHash(library), consumer = TextHash(consumer) },
            bootstrap = "Explicit host .NET primitive core; no System seed or reference-only control counts as execution",
            cases,
            commands
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        if (!passed && !inventory) throw new Exception("dual-target driver acceptance failed; see validation.json");
        Console.WriteLine(passed ? "PASS dual-target driver acceptance" : "RECORDED dual-target driver gaps");

        async Task<(string Stdout, string Stderr)> Command(string executable, string[] arguments, int expected)
        {
            var start = new ProcessStartInfo(executable) { RedirectStandardOutput = true, RedirectStandardError = true, WorkingDirectory = output };
            foreach (var argument in arguments) start.ArgumentList.Add(argument);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            await process.WaitForExitAsync();
            var text = await stdout; var errors = await stderr;
            commands.Add(new { executable, arguments, expectedExitCode = expected, exitCode = process.ExitCode, stdout = text, stderr = errors });
            if (process.ExitCode != expected) throw new Exception($"{executable} exit {process.ExitCode}, expected {expected}: {text}{errors}");
            return (text, errors);
        }
    }
    private static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
    private static string TextHash(string text) => Convert.ToHexString(SHA256.HashData(System.Text.Encoding.UTF8.GetBytes(text)));
}
