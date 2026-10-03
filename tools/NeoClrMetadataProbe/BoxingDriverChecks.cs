using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

namespace NeoClrMetadataProbe;

internal enum StorageDriverScenario { Boxing, FieldAddresses, ReferenceOperations, ObjectDisplay, NullLiterals }

internal static class BoxingDriverChecks
{
    internal static async Task Run(string driver, string runtime, string core, string seed, string output, StorageDriverScenario scenario = StorageDriverScenario.Boxing)
    {
        driver = Path.GetFullPath(driver); runtime = Path.GetFullPath(runtime);
        core = Path.GetFullPath(core); seed = Path.GetFullPath(seed); output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var fieldAddresses = scenario == StorageDriverScenario.FieldAddresses;
        var nulls = scenario == StorageDriverScenario.NullLiterals;
        var display = scenario == StorageDriverScenario.ObjectDisplay;
        var references = scenario == StorageDriverScenario.ReferenceOperations;
        var stem = nulls ? "NullLiterals" : display ? "ObjectDisplay" : references ? "References" : fieldAddresses ? "FieldAddress" : "Box";
        var source = Path.Combine(output, stem + ".rvn");
        File.WriteAllText(source, nulls ? """
            func EmptyObject() -> object? => null
            func EmptyString() -> string? => null
            func IsMissing(value: string?) -> bool => value == null
            func Main() -> int {
                var empty: string? = "initial"
                empty = null
                if EmptyObject() != null {
                    return 1
                }
                if EmptyString() != null {
                    return 2
                }
                if empty != null {
                    return 3
                }
                if !IsMissing(null) {
                    return 4
                }
                return 42
            }
            """ : display ? """
            func Box<T>(value: T) -> object => value
            func Main() -> int {
                System.Console.WriteLine(Box(42).ToString())
                System.Console.WriteLine(Box("text").ToString())
                return 42
            }
            """ : references ? """
            func Box<T>(value: T) -> object => value
            func Read(value: object?) -> int {
                if value == null {
                    return 3
                }
                if value is string {
                    return 42
                }
                return 4
            }
            func Main() -> int {
                if Read(Box(42)) != 4 {
                    return 1
                }
                if Read(default(object?)) != 3 {
                    return 2
                }
                return Read(Box("text"))
            }
            """ : fieldAddresses ? """
            public struct Counter {
                public field Value: int
                public init(value: int) {
                    self.Value = value
                }
                func Increment() -> int {
                    Value = Value + 1
                    return Value
                }
            }
            public class Holder<T> {
                public field Value: T
                public init(value: T) {
                    self.Value = value
                }
            }
            func Main() -> int {
                let holder = Holder<Counter>(Counter(40))
                let alias = holder
                holder.Value.Increment()
                alias.Value.Increment()
                return holder.Value.Value
            }
            """ : """
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
            var assembly = Path.Combine(output, stem + (native ? ".native.dll" : ".clr.dll"));
            string[] args = native
                ? [driver, "neoclr", "--core-reference", core, "--runtime-seed", seed, "-o", assembly, source]
                : [driver, "--framework", "net10.0", "--emit-core-types-only", "-o", assembly, source];
            await Command("dotnet", args, 0);
            if (native) await Command(runtime, ["verify", assembly, "--system", seed], 0);
            await Command(native ? runtime : "dotnet", native
                ? ["run", assembly, "--system", seed]
                : ["exec", "--runtimeconfig", Path.ChangeExtension(driver, ".runtimeconfig.json"), assembly], 42, display ? "42\ntext" : scenario != StorageDriverScenario.Boxing ? "" : "boxed");
        }
        if (scenario == StorageDriverScenario.Boxing)
        {
            var rejected = Path.Combine(output, "MissingSeed.dll");
            var error = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "-o", rejected, source], 1);
            if (!error.Contains("unregistered dependency type: class object") || File.Exists(rejected)) throw new Exception("missing boxing seed was not rejected before publication");
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            scope = nulls ? "Dual-target typed null returns and initialized local (42)." : display ? "Dual-target core Object.ToString dispatch on generic boxed integer and string (42)." : references ? "Dual-target reference null checks and discard type tests return 42." : fieldAddresses ? "Dual-target nested generic field addresses preserve mutable storage and object aliases (42)." : "Dual-target ordinary-command boxing smoke; detailed value/identity assertions live in C# metadata and CLR conversion tests, not this discarded-result smoke.",
            driverSha256 = Hash(driver), runtimeSha256 = Hash(runtime), coreSha256 = Hash(core), seedSha256 = Hash(seed), sourceSha256 = Hash(source), commands
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine(nulls ? "PASS dual-target null literals" : display ? "PASS dual-target core Object display dispatch" : references ? "PASS dual-target null checks and type tests" : fieldAddresses ? "PASS dual-target nested field mutation and alias identity" : "PASS dual-target boxing smoke and missing-seed publication guard");

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
