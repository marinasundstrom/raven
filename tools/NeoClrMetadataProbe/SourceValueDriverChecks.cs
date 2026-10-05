using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

namespace NeoClrMetadataProbe;

// Declaration/receiver prerequisite for source unions, not a source-union completion gate.
internal static class SourceValueDriverChecks
{
    internal static async Task Run(string driver, string runtime, string core, string output, bool nested = false, bool byteDiscriminator = false, bool valueInterface = false)
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
            public struct PropertyPoint {
                var X: int
                var Y: int
                init(value: int) { X = value; self.Y = value + 1 }
                func Set(value: int) { X = value }
            }
            func Main() -> int {
                var point = PropertyPoint(41)
                let snapshot = point
                point.Set(42)
                if snapshot.X != 41 || snapshot.Y != 42 || point.X != 42 { return 6 }
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
        if (byteDiscriminator) source = """
            public struct Tagged {
                private var tag: byte
                init(value: byte) { tag = value }
                val Tag: byte => tag
            }
            func Echo(value: byte) -> byte => value
            func Narrow(value: int) -> byte => (byte)value
            func NarrowLong(value: long) -> byte => (byte)value
            func Main() -> int {
                let value = Tagged(255b)
                if value.Tag != 255 { return 1 }
                if Narrow(-1) != 255 { return 2 }
                if Narrow(256) != 0 { return 3 }
                if NarrowLong(-1L) != 255 { return 4 }
                let wrapped = Echo(Narrow(298))
                return wrapped
            }
            """;
        if (valueInterface) source = """
            public interface Counter { func Next() -> int }
            public struct ValueCounter : Counter {
                private var count: int
                init(value: int) { count = value }
                func Next() -> int { count = count + 1; return count }
            }
            func Main() -> int {
                var original = ValueCounter(40)
                var copy = original
                if original.Next() != 41 { return 1 }
                if copy.Next() != 41 { return 2 }
                return original.Next()
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
            if (byteDiscriminator)
            {
                var librarySource = Path.Combine(output, target + "Tags.rvn");
                var library = Path.ChangeExtension(librarySource, ".dll");
                File.WriteAllText(librarySource, """
                    public static class Tags {
                        static func Narrow(value: int) -> byte => (byte)value
                    }
                    """);
                var librarySourceHash = Hash(librarySource);
                string[] libraryFlags = native ? ["--library"] : ["--output-type", "classlib"];
                await Command("dotnet", [driver, .. flags, .. libraryFlags, "-o", library, librarySource], 0);
                File.Delete(librarySource);
                var consumerSource = Path.Combine(output, target + "Consumer.rvn");
                var consumer = Path.ChangeExtension(consumerSource, ".dll");
                File.WriteAllText(consumerSource, """
                    func Main() -> int {
                        if Tags.Narrow(-1) != 255 { return 1 }
                        return Tags.Narrow(298)
                    }
                    """);
                await Command("dotnet", [driver, .. flags, native ? "--reference" : "--refs", library, "-o", consumer, consumerSource], 0);
                var consumed = native ? await Command(runtime, ["run", consumer, "--module", library], 42)
                    : await Command("dotnet", ["exec", "--runtimeconfig", Path.ChangeExtension(driver, ".runtimeconfig.json"), consumer], 42);
                if (consumed.Output != "" || consumed.Error != "") throw new Exception("unexpected imported byte output");
                results.Add(new { target, scenario = "imported-byte", librarySourceSha256 = librarySourceHash, librarySha256 = Hash(library), consumerSha256 = Hash(consumer), sourceSha256 = Hash(consumerSource) });
            }
            results.Add(new { target, passed = true, assemblySha256 = Hash(assembly), sourceSha256 = Hash(path) });
        }
        var rejectedSource = Path.Combine(output, "Rejected.rvn");
        var rejectedOutput = Path.Combine(output, "Rejected.dll");
        File.WriteAllText(rejectedSource, "public interface Value { func Read() -> int }\npublic struct Box : Value { func Read() -> int => 42 }\nfunc Main() -> int { let value: Value = Box(); return value.Read() }");
        var rejection = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "-o", rejectedOutput, rejectedSource], 1);
        if (!rejection.Error.Contains("NEOMETA003") || !rejection.Error.Contains("explicit System core binding"))
            throw new Exception("missing value-boxing runtime-binding diagnostic");
        if (File.Exists(rejectedOutput)) throw new Exception("unbound value boxing published output");
        if (nested)
        {
            File.WriteAllText(rejectedSource, "public class Outer<T> { struct Case { } }\nfunc Main() -> int => 0");
            var genericOwner = await Command("dotnet", [driver, "neoclr", "--core-reference", core, "-o", rejectedOutput, rejectedSource], 1);
            if (!genericOwner.Error.Contains("NEOMETA001") || File.Exists(rejectedOutput)) throw new Exception("generic enclosing owner was not rejected before publication");
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            passed = true,
            scope = valueInterface ? "Ordinary-driver value-type interface declarations and concrete addressed calls, not boxed interface conversion." : byteDiscriminator ? "Ordinary-driver Byte signatures, fields, literals and numeric conversions execute on both targets. Source unions remain pending." : "Ordinary-driver source value declarations, inline payloads, constructors, accessors, mutation and self copies. Not separate library import or source union emission.",
            driverSha256 = Hash(driver),
            runtimeSha256 = Hash(runtime),
            coreSha256 = Hash(core),
            results,
            commands
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine(byteDiscriminator ? "PASS Byte discriminator storage and conversions execute on both targets" : "PASS source value declarations and independent inline copies execute on both targets");

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
