using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using NeoClrMetadataProbe;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using AssemblyBuilder = NeoCLR.Metadata.Experimental.Model.AssemblyBuilder;
using AssemblyDefinition = NeoCLR.Metadata.Experimental.Model.AssemblyDefinition;

if (args.Length != 2) throw new ArgumentException("Usage: NeoClrMetadataProbe <neoclr executable> <fresh output directory>");
var runtime = Path.GetFullPath(args[0]);
var output = Path.GetFullPath(args[1]);
if (Directory.Exists(output)) throw new IOException("output directory must be fresh");
Directory.CreateDirectory(output);
var hostCore = typeof(object).Assembly.GetName();
var core = new AssemblyIdentity(hostCore.Name!, hostCore.Version!, hostCore.CultureName ?? "", Convert.ToHexString(hostCore.GetPublicKeyToken() ?? []));
var library = new AssemblyBuilder(new("MetadataProbeLibrary", new Version(1, 0, 0, 0)), core);
var twice = library.AddType("Example", "Math").AddMethod("Twice", 1);
twice.LoadArgument(0); twice.LoadConstant(2); twice.Multiply(); twice.Return();
var libraryPath = Path.Combine(output, "MetadataProbeLibrary.dll");
var nativeLibrary = Path.Combine(output, "MetadataProbeLibrary.neo.json");
File.WriteAllBytes(libraryPath, library.Write());
File.WriteAllBytes(nativeLibrary, library.WriteNativeAssembly());
var metadata = AssemblyDefinition.ReadAssembly(File.ReadAllBytes(libraryPath), false);
const string source = """
func Offset(value: int) -> int {
    return value + 2
}
func Main() -> int {
    return Offset(Example.Math.Twice(20))
}
""";
File.WriteAllText(Path.Combine(output, "Program.rvn"), source);
byte[] Compile(string code)
{
    var tree = SyntaxTree.ParseText(code);
    var compilation = Compilation.Create("MetadataProbeApp", [tree], [
        MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
        MetadataReference.CreateFromFile(libraryPath)], new CompilationOptions(OutputKind.ConsoleApplication));
    return Int32Emitter.Emit(compilation, tree, core, library, metadata);
}
var image = Compile(source);
var application = Path.Combine(output, "MetadataProbeApp.neo.json");
File.WriteAllBytes(application, image);
try { Compile(source.Replace("value + 2", "value / 2")); throw new Exception("unsupported operation accepted"); }
catch (InvalidDataException error) when (error.Message.Contains("NEOMETA001")) { Console.WriteLine("PASS unsupported operation diagnostic"); }
try { Compile(source.Replace("Example.Math.Twice", "Example.Math.Missing")); throw new Exception("binding error accepted"); }
catch (InvalidDataException error) when (!error.Message.Contains("NEOMETA001")) { Console.WriteLine("PASS compiler binding diagnostic"); }
await Command(0, "verify", application, "--module", nativeLibrary);
var result = await Command(42, "run", application, "--module", nativeLibrary, "--show-result");
if (!result.Contains("=> Int32(42)")) throw new Exception("wrong runtime result");
File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
{
    date = "2026-09-30",
    result = 42,
    source = "Raven public semantic operations",
    metadataLibraryIndependent = true,
    semanticLoader = "existing .NET provider over API-produced PE",
    emitter = "experimental operations adapter to native format 5",
    productionTargetIntegrated = false,
    nativeMetadataLoader = false,
    unsupportedOperationRejected = true,
    bindingErrorRejected = true,
    compilerSha256 = Hash(typeof(Compilation).Assembly.Location),
    metadataApiSha256 = Hash(typeof(AssemblyBuilder).Assembly.Location),
    runtimeSha256 = Hash(runtime),
    applicationSha256 = Hash(application),
    dependencySha256 = Hash(nativeLibrary)
}, new JsonSerializerOptions { WriteIndented = true }) + "\n");
Console.WriteLine("PASS Raven semantic operations -> separate metadata library -> neoCLR: 42");

static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path))).ToLowerInvariant();
async Task<string> Command(int expected, params string[] arguments)
{
    var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false };
    foreach (var argument in arguments) start.ArgumentList.Add(argument);
    using var process = Process.Start(start)!;
    var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
    using var timeout = new CancellationTokenSource(TimeSpan.FromMinutes(2));
    try { await process.WaitForExitAsync(timeout.Token); }
    catch (OperationCanceledException) { process.Kill(true); throw new TimeoutException("runtime probe timed out"); }
    string text = await stdout + await stderr;
    if (process.ExitCode != expected) throw new Exception($"runtime returned {process.ExitCode}, expected {expected}: {text}");
    Console.WriteLine("PASS " + arguments[0] + " (exit " + process.ExitCode + ")");
    return text;
}
