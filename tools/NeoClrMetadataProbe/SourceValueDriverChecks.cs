using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

namespace NeoClrMetadataProbe;

// Declaration/receiver prerequisite for source unions, not a source-union completion gate.
internal static class SourceValueDriverChecks
{
    internal static async Task Run(string driver, string runtime, string core, string output, bool nested = false)
    {
        driver = Path.GetFullPath(driver); runtime = Path.GetFullPath(runtime); core = Path.GetFullPath(core);
        output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var source = """
            public struct Payload<T> {
                private var stored: T
                init(value: T) { stored = value }
                val Value: T => stored
            }
            public struct Carrier<T> {
                private var stored: Payload<T>
                private var tag: int
                init(value: Payload<T>) { stored = value; tag = 1 }
                val Tag: int => tag
                func Read() -> Payload<T> { return stored }
                func Set(value: Payload<T>) { stored = value }
                func Copy() -> Carrier<T> { return self }
            }
            public struct Empty {
                private var number: int
                val Number: int => number
            }
            public struct Plain {
                public field Value: int
                init(value: int) { Value = value }
            }
            func Main() -> int {
                var plain = Plain(41)
                let saved = plain
                plain.Value = 42
                if saved.Value != 41 { return 4 }
                if plain.Value != 42 { return 5 }
                let empty = Empty()
                if empty.Number != 0 { return 1 }
                var original = Carrier<int>(Payload<int>(40))
                var copy = original.Copy()
                copy.Set(Payload<int>(2))
                if original.Tag != 1 { return 2 }
                if copy.Tag != 1 { return 3 }
                let first = original.Read()
                let second = copy.Read()
                return first.Value + second.Value
            }
            """;
        if (nested) source = """
            public static class First {
                struct Payload<T> {
                    private var stored: T
                    init(value: T) { stored = value }
                    val Value: T => stored
                    func Set(value: T) { stored = value }
                    func Copy() -> First.Payload<T> { return self }
                }
            }
            public static class Second {
                internal struct Payload {
                    private var stored: int
                    init(value: int) { stored = value }
                    val Value: int => stored
                }
                class Reference {
                    private var stored: int
                    init(value: int) { stored = value }
                    val Value: int => stored
                }
                struct Empty {
                    private var number: int
                    val Number: int => number
                }
            }
            func Main() -> int {
                var original = First.Payload<int>(40)
                let copy = original.Copy()
                original.Set(9)
                let other = Second.Payload(2)
                let empty = Second.Empty()
                let reference = Second.Reference(42)
                if empty.Number != 0 { return 1 }
                if reference.Value != 42 { return 2 }
                if original.Value != 9 { return 3 }
                return copy.Value + other.Value
            }
            """;
        var commands = new List<object>();
        var results = new List<object>();
        foreach (var native in new[] { false, true })
        {
            var target = native ? "neoclr" : "dotnet";
            var path = Path.Combine(output, target + ".rvn");
            var assembly = Path.ChangeExtension(path, ".dll");
            File.WriteAllText(path, source);
            string[] flags = native ? ["neoclr", "--core-reference", core] : ["--framework", "net10.0", "--emit-core-types-only"];
            await Command("dotnet", [driver, .. flags, "-o", assembly, path], 0);
            if (native)
            {
                await Command(runtime, ["verify", assembly], 0);
                if (nested)
                {
                    var snapshot = NeoCLR.Metadata.Experimental.Model.AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(assembly));
                    var cases = snapshot.MainModule.Types.Where(t => t.Name is "Payload`1" or "Payload").ToArray();
                    if (cases.Length != 2 || cases.Any(t => t.DeclaringType is null) ||
                        cases.Single(t => t.Name == "Payload`1").DeclaringType!.Name != "First" ||
                        cases.Single(t => t.Name == "Payload").DeclaringType!.Name != "Second")
                        throw new Exception("emitted cases lost lexical ownership");
                }
            }
            var execution = native ? await Command(runtime, ["run", assembly], 42)
                : await Command("dotnet", ["exec", "--runtimeconfig", Path.ChangeExtension(driver, ".runtimeconfig.json"), assembly], 42);
            if (execution.Output != "" || execution.Error != "") throw new Exception("unexpected program output");
            results.Add(new { target, passed = true, assemblySha256 = Hash(assembly), sourceSha256 = Hash(path) });
        }
        var rejectedSource = Path.Combine(output, "Rejected.rvn");
        var rejectedOutput = Path.Combine(output, "Rejected.dll");
        File.WriteAllText(rejectedSource, "public interface Value { func Read() -> int }\npublic struct Box : Value { func Read() -> int => 42 }\nfunc Main() -> int => 0");
        var rejection = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "-o", rejectedOutput, rejectedSource], 1);
        if (!rejection.Error.Contains("NEOMETA001")) throw new Exception("missing capability diagnostic");
        if (File.Exists(rejectedOutput)) throw new Exception("unsupported value interface published output");
        if (nested)
        {
            File.WriteAllText(rejectedSource, "public class Outer<T> { struct Case { } }\nfunc Main() -> int => 0");
            var genericOwner = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "-o", rejectedOutput, rejectedSource], 1);
            if (!genericOwner.Error.Contains("NEOMETA001") || File.Exists(rejectedOutput)) throw new Exception("generic enclosing owner was not rejected before publication");
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            passed = true,
            scope = "Ordinary-driver source value declarations, inline payloads, constructors, accessors, mutation and self copies. Not separate library import or source union emission.",
            driverSha256 = Hash(driver),
            runtimeSha256 = Hash(runtime),
            coreSha256 = Hash(core),
            results,
            commands
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS source value declarations and independent inline copies execute on both targets");

        async Task<(string Output, string Error)> Command(string executable, string[] arguments, int expected)
        {
            var start = new ProcessStartInfo(executable) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var argument in arguments) start.ArgumentList.Add(argument);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
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
