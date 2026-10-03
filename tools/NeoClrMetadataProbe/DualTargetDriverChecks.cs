using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

namespace NeoClrMetadataProbe;

// Executes ordinary compiler commands, with no Compilation or emitter API shortcuts.
internal static class DualTargetDriverChecks
{
    internal static async Task Run(string driver, string runtime, string core, string output, bool inventory, bool external = false, bool parameterModes = false)
    {
        core = Path.GetFullPath(core);
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
        string externalContracts = """
            namespace DriverContracts
            public interface Value<T> { val Current: T { get; } }
            public interface MutableValue<T> : Value<T> { func Set(value: T) }
            public interface ReadableValue<T> : Value<T> { }
            """;
        string externalImplementation = """
            namespace DriverContracts
            public interface CombinedValue<T> : MutableValue<T>, ReadableValue<T> { }
            public class Box<T> : CombinedValue<T> {
                public field stored: T
                public init(value: T) { stored = value }
                public val Current: T => stored
                public func Set(value: T) { stored = value }
            }
            """;
        string consumer = """
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
        if (parameterModes)
        {
            externalContracts = """
                namespace DriverContracts
                public interface Output<T> {
                    func Read(out value: T) -> bool
                    func Bump(ref value: T)
                }
                public interface Values<T> : Output<T> { }
                """;
            externalImplementation = """
                namespace DriverContracts
                public class Box : Values<int> {
                    public init() { }
                    public func Read(out value: int) -> bool {
                        value = 40
                        return true
                    }
                    public func Bump(ref value: int) { value = value + 2 }
                }
                """;
            consumer = """
                import DriverContracts.*
                func Main() -> int {
                    let box: Output<int> = Box()
                    var value = 0
                    if box.Read(out value) {
                        box.Bump(ref value)
                        return value
                    }
                    return 0
                }
                """;
        }
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
                    string? implementation = null;
                    if (scenario == "library")
                    {
                        var source = Path.Combine(directory, "Contracts.rvn");
                        dependency = Path.Combine(directory, "Contracts.dll");
                        File.WriteAllText(source, external ? externalContracts : library);
                        await Compile(source, dependency, true, null);
                        File.Delete(source); // The consumer must import the artifact, not reuse source.
                        if (external)
                        {
                            var implementationSource = Path.Combine(directory, "Implementation.rvn");
                            implementation = Path.Combine(directory, "Implementation.dll");
                            File.WriteAllText(implementationSource, externalImplementation);
                            await Compile(implementationSource, implementation, true, dependency);
                            File.Delete(implementationSource);
                        }
                    }
                    await Compile(mainSource, app, false, dependency, implementation);
                    var modules = new[] { dependency, implementation }.OfType<string>().SelectMany(p => new[] { "--module", p }).ToArray();
                    if (native)
                        await Command(runtime, ["verify", app, .. modules], 0);
                    var result = native
                        ? await Command(runtime, ["run", app, .. modules], 42)
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
            if (parameterModes)
            {
                try
                {
                    var source = Path.Combine(directory, "InvalidMode.rvn");
                    var rejected = Path.Combine(directory, "InvalidMode.dll");
                    File.WriteAllText(source, externalImplementation.Replace("Bump(ref value: int) { value = value + 2 }", "Bump(out value: int) { value = 2 }"));
                    string[] modeArguments = native
                        ? [driver, "neoclr", "--core-reference", core, "--library", "--reference", Path.Combine(directory, "Contracts.dll")]
                        : [driver, "--framework", "net10.0", "--emit-core-types-only", "--output-type", "classlib", "--refs", Path.Combine(directory, "Contracts.dll")];
                    await Command("dotnet", [.. modeArguments, "-o", rejected, source], 1);
                    if (File.Exists(rejected)) throw new Exception("incompatible parameter mode published output");
                    cases.Add(new { target, scenario = "incompatible-parameter-mode", passed = true });
                }
                catch (Exception error)
                {
                    passed = false;
                    cases.Add(new { target, scenario = "incompatible-parameter-mode", passed = false, error = error.Message });
                }
            }
            if (native && !external)
            {
                try
                {
                    var source = Path.Combine(directory, "Rejected.rvn");
                    var rejected = Path.Combine(directory, "Rejected.dll");
                    var dependency = Path.Combine(directory, "Contracts.dll");
                    File.WriteAllText(source, "func Main() -> int => 0");
                    async Task Reject(string[] extra, string? message = null)
                    {
                        var result = await Command("dotnet", [driver, "neoclr", "--core-reference", core, .. extra, "-o", rejected, source], 1);
                        if (File.Exists(rejected) || message is not null && !(result.Stdout + result.Stderr).Contains(message))
                            throw new Exception("rejection published output or lost diagnostic: " + result.Stdout + result.Stderr);
                    }
                    var duplicate = Path.Combine(directory, "Duplicate.dll");
                    File.Copy(dependency, duplicate);
                    await Reject(["--reference", dependency, "--reference", duplicate], "duplicate native assembly identity");
                    await Reject(["--reference", typeof(object).Assembly.Location]);
                    var malformed = Path.Combine(directory, "Malformed.dll");
                    File.WriteAllText(malformed, "not an assembly");
                    await Reject(["--reference", malformed]);
                    var relaySource = Path.Combine(directory, "Relay.rvn");
                    var relay = Path.Combine(directory, "Relay.dll");
                    File.WriteAllText(relaySource, "import DriverContracts.*\npublic func Create() -> Box<int> => Box<int>(42)");
                    await Compile(relaySource, relay, true, dependency);
                    File.Delete(relaySource);
                    await Reject(["--reference", relay], "missing or mismatched native dependency");
                    File.WriteAllText(source, "func Main() -> int { return (int)(double)42 }");
                    await Reject([], "NEOMETA001");
                    cases.Add(new { target, scenario = "native-reference-rejections", passed = true });
                }
                catch (Exception error)
                {
                    passed = false;
                    cases.Add(new { target, scenario = "native-reference-rejections", passed = false, error = error.Message });
                }
            }
            async Task Compile(string source, string destination, bool isLibrary, string? reference, string? secondReference = null)
            {
                var args = new List<string> { driver };
                if (native)
                {
                    args.AddRange(["neoclr", "--core-reference", core]);
                    if (isLibrary) args.Add("--library");
                    if (reference is not null) args.AddRange(["--reference", reference]);
                    if (secondReference is not null) args.AddRange(["--reference", secondReference]);
                }
                else
                {
                    args.AddRange(["--framework", "net10.0", "--emit-core-types-only"]);
                    if (isLibrary) args.AddRange(["--output-type", "classlib"]);
                    if (reference is not null) args.AddRange(["--refs", reference]);
                    if (secondReference is not null) args.AddRange(["--refs", secondReference]);
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
            sourceSha256 = new { hello = TextHash(hello), library = TextHash(library), consumer = TextHash(consumer), externalContracts = TextHash(externalContracts), externalImplementation = TextHash(externalImplementation) },
            bootstrap = "Explicit .NET host core / NeoCLR.CoreProbe bootstrap; no System seed or reference-only control counts as execution",
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
            using var timeout = new CancellationTokenSource(TimeSpan.FromMinutes(2));
            try { await process.WaitForExitAsync(timeout.Token); }
            catch (OperationCanceledException) { process.Kill(true); throw new TimeoutException("driver acceptance command timed out"); }
            var text = await stdout; var errors = await stderr;
            commands.Add(new { executable, arguments, expectedExitCode = expected, exitCode = process.ExitCode, stdout = text, stderr = errors });
            if (process.ExitCode != expected) throw new Exception($"{executable} exit {process.ExitCode}, expected {expected}: {text}{errors}");
            return (text, errors);
        }
    }
    private static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
    private static string TextHash(string text) => Convert.ToHexString(SHA256.HashData(System.Text.Encoding.UTF8.GetBytes(text)));
}
