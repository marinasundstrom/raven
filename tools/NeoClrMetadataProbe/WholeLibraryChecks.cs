using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class WholeLibraryChecks
{
    internal static async Task Run(string sourceRoot, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        const string relative = "System/Globalization/Language.rvn";
        var source = File.ReadAllText(Path.Combine(sourceRoot, relative));
        const string entry = """
            class Box<T> {
                private var stored: T
                private init(value: T) { stored = value }
                static val Empty: Box<T> => Box<T>(default(T))
                val Value: T => stored
            }
            static class Settings {
                static var Values: int[] {
                    get => [0]
                    set { value[0] = 42 }
                }
                static func Get() -> int[] => Values
            }
            func Main() -> int {
                let values = Settings.Get()
                Settings.Values = values
                if Box<int>.Empty.Value != 0 { return 1 }
                if values[0] != 42 { return 2 }
                let first = System.Globalization.Language.Undetermined
                System.Console.WriteLine(first.Code)
                System.Console.WriteLine(System.Globalization.Language.Swedish.Code)
                System.Console.WriteLine(System.Globalization.Language.Hebrew.Code)
                return 42
            }
            """;
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var console = MetadataReference.CreateFromFile(typeof(Console).Assembly.Location);
        var references = new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location), console,
            MetadataReference.CreateFromFile(Assembly.Load("System.Runtime").Location) };
        foreach (var reverse in new[] { false, true })
        {
            var trees = new[] { SyntaxTree.ParseText(source, path: relative), SyntaxTree.ParseText(entry, path: "Main.rvn") };
            if (reverse) Array.Reverse(trees);
            var name = "WholeLanguage" + (reverse ? "Reverse" : "Forward");
            var compilation = Compilation.Create(name, trees, references, new CompilationOptions(OutputKind.ConsoleApplication));
            using var native = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native, new(new(name, new Version(1, 0, 0, 0)), core, [], console));
            if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
            var projection = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            var properties = projection.MainModule.Types.Single(t => t.Name == "Language").Properties;
            if (properties.Count != 4 || properties.Count(p => p.GetMethod?.IsStatic == true) != 3 ||
                properties.Single(p => p.Name == "Code").GetMethod?.IsStatic != false)
                throw new Exception("native property associations lost in projection");
            var path = Path.Combine(output, name + ".dll"); File.WriteAllBytes(path, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                using var process = Process.Start(start)!;
                var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync();
                var text = (await stdout).Replace("\r\n", "\n"); var error = await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42) || command == "run" && text != "und\nsv\nhe\n")
                    throw new Exception(command + ": " + text + error);
            }
            using var cli = new MemoryStream(); var emitted = compilation.Emit(cli);
            if (!emitted.Success) throw new Exception(string.Join("; ", emitted.Diagnostics));
            var loaded = Assembly.Load(cli.ToArray());
            var saved = Console.Out; using var captured = new StringWriter();
            try { Console.SetOut(captured); if (!Equals(loaded.EntryPoint!.Invoke(null, null), 42)) throw new Exception("CLI result"); }
            finally { Console.SetOut(saved); }
            if (captured.ToString().Replace("\r\n", "\n") != "und\nsv\nhe\n") throw new Exception("CLI output");
            var language = loaded.GetType("System.Globalization.Language")!;
            if (language.GetConstructors().Length != 0 || language.GetProperties().Count(p => p.GetMethod!.IsStatic) != 3)
                throw new Exception("Language metadata contract");
        }
        foreach (var unsupported in new[] {
            "class Storage { public static var Count: int = 0 }",
            "static class Storage { static val Count: int = 0 }"
        })
        {
            var compilation = Compilation.Create("StaticStorage", [SyntaxTree.ParseText(unsupported, path: "Storage.rvn")], references,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            if (compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error)) throw new Exception("invalid rejection fixture");
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new("StaticStorage", new Version(1, 0, 0, 0)), core, [], console));
            if (result.Success || image.Length != 0 || !result.Diagnostics.Any(d => d.Id == "NEOMETA001"))
                throw new Exception("static storage must reject before output");
        }
        File.WriteAllText(Path.Combine(output, "Language.rvn"), source);
        File.WriteAllText(Path.Combine(output, "Main.rvn"), entry);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            source = relative,
            wholeFile = true,
            sourceSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(source))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            consumerSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(entry))),
            cliResult = 42,
            nativeResult = 42,
            stdout = "und\nsv\nhe\n",
            sourceOrders = 2,
            staticSetter = true,
            genericStaticGetter = true,
            staticStorageRejections = 2,
            bootstrap = "host core and explicit console bridge; no native symbol importer",
            fullClassLibrary = false
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS unchanged Language class on CLI/native in both source orders: und/sv/he, 42");
    }
}
