using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeProfileChecks
{
    internal static async Task Run(string root, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var path = Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll");
        var reference = MetadataReference.CreateFromFile(path);
        var name = AssemblyName.GetAssemblyName(path);
        var core = new AssemblyIdentity(name.Name!, name.Version!, name.CultureName ?? "", Convert.ToHexString(name.GetPublicKeyToken() ?? []));
        var cases = new (string Name, string Source, int Result, string Output)[]
        {
            ("NativeProfileFunction", """
                func Increment(value: int) -> int => value + 2
                func Apply(callback: (int) -> int, value: int) -> int => callback(value)
                func Main() -> int {
                    let callback: (int) -> int = Increment
                    return Apply(callback, 40)
                }
                """, 42, ""),
            ("NativeProfileRefOut", """
                func Set(out value: int) { value = 40 }
                func Forward(out value: int) { Set(out value) }
                func Increment(ref value: int) { value = value + 2 }
                func Main() -> int {
                    Forward(out var value)
                    Increment(ref value)
                    return value
                }
                """, 42, ""),
            ("NativeProfileHello", """
                func Greet() {
                    System.Console.WriteLine("Hello World")
                }
                func Main() -> int {
                    Greet()
                    return 42
                }
                """, 42, "Hello World\n"),
            ("NativeProfileDispatch", InterfaceDispatchChecks.Contracts + "\n" + InterfaceDispatchChecks.Consumer, 42, ""),
            ("NativeProfileArray", """
                func Main() -> int {
                    let values: int[] = [19, 23]
                    var total = 0
                    for value in values { total = total + value }
                    return total
                }
                """, 42, ""),
            ("NativeProfileUnit", """
                func Main() {
                    System.Console.WriteLine("Hello World")
                }
                """, 0, "Hello World\n")
        };
        foreach (var item in cases)
        {
            var options = new NeoClrEmitOptions(new(item.Name, new Version(1, 0, 0, 0)), core, [], reference);
            var compilation = Compilation.Create(item.Name, [SyntaxTree.ParseText(item.Source)], [reference], CompilationOptions.NeoCLR);
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, options);
            if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
            var binary = Path.Combine(output, item.Name + ".dll");
            File.WriteAllBytes(binary, image.ToArray());
            File.WriteAllText(Path.Combine(output, item.Name + ".rvn"), item.Source);
            await Command("verify", binary, 0, null);
            await Command("run", binary, item.Result, item.Output);
            Reject(compilation, new(options.Identity, new(core.Name, new Version(99, 0, 0, 0)), [], reference));
            Reject(compilation, new(options.Identity, new("WrongCore", core.Version), [], reference));
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            bindingProfile = "CompilationOptions.NeoCLR with CLI declaration snapshot; no host core reference",
            coreReference = "api-docs/reference/NeoCLR.CoreProbe.dll",
            coreSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            cases = cases.Select(c => new { c.Name, c.Result, c.Output, verified = true }),
            rejected = new[] { "wrong core name", "wrong core version" },
            scope = "primitive/Unit/array/function/interface/ref/out emission and runtime loading; not implementation bootstrap or native metadata symbol import"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");

        static void Reject(Compilation compilation, NeoClrEmitOptions options)
        {
            using var output = new MemoryStream();
            output.WriteByte(73);
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, output, options);
            if (result.Success || !result.Diagnostics.Any(d => d.Id == "NEOMETA002") || output.Length != 1 || output.Position != 1 || output.ToArray()[0] != 73)
                throw new Exception("configuration rejection failed or wrote output");
        }
        async Task Command(string command, string binary, int exitCode, string? expected)
        {
            var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
            start.ArgumentList.Add(command); start.ArgumentList.Add(binary);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            await process.WaitForExitAsync();
            var text = (await stdout).Replace("\r\n", "\n"); var error = await stderr;
            if (process.ExitCode != exitCode || expected is not null && text != expected)
                throw new Exception(command + ": " + process.ExitCode + " " + text + error);
        }
    }
}
