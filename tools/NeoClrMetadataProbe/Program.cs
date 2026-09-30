using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;
using System.Text.Json.Nodes;

using RuntimeAssemblyContainer = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer;
using NeoCLR.Metadata.Experimental.Model;

using NeoClrMetadataProbe;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.NeoClr;

using AssemblyBuilder = NeoCLR.Metadata.Experimental.Model.AssemblyBuilder;
using AssemblyDefinition = NeoCLR.Metadata.Experimental.Model.AssemblyDefinition;

if (args.Length == 5 && args[0] == "--system-symbols")
{
    await SystemSymbolChecks.Run(args[1], args[2], args[3], args[4]);
    return;
}

if (args.Length is < 2 or > 4 || (args.Length == 3 && args[2] != "--hello-only") || (args.Length == 4 && args[2] != "--driver"))
    throw new ArgumentException("Usage: NeoClrMetadataProbe <neoclr executable> <fresh output directory> [--hello-only | --driver rvnc.dll]");
var runtime = Path.GetFullPath(args[0]);
var output = Path.GetFullPath(args[1]);
if (Directory.Exists(output)) throw new IOException("output directory must be fresh");
Directory.CreateDirectory(output);
var hostCore = typeof(object).Assembly.GetName();
var core = new AssemblyIdentity(hostCore.Name!, hostCore.Version!, hostCore.CultureName ?? "", Convert.ToHexString(hostCore.GetPublicKeyToken() ?? []));
if (args.Length == 3)
{
    await HelloWorldChecks.Run(core, output, Command);
    return;
}
const string arithmeticSource = """
public static class Arithmetic {
    static func Multiply(value: int, factor: int) -> int {
        return value * factor
    }
}
""";
File.WriteAllText(Path.Combine(output, "Arithmetic.rvn"), arithmeticSource);
var arithmeticCompilation = Compilation.Create("ArithmeticDependency", [SyntaxTree.ParseText(arithmeticSource, path: "Arithmetic.rvn")],
    [MetadataReference.CreateFromFile(typeof(object).Assembly.Location)], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
var arithmeticOptions = new NeoClrEmitOptions(new("ArithmeticDependency", new Version(1, 0, 0, 0)), core, []);
using var arithmeticOutput = new MemoryStream();
var arithmeticResult = NeoClrCompilationEmitter.EmitMetadataAssembly(arithmeticCompilation, arithmeticOutput, arithmeticOptions);
if (!arithmeticResult.Success) throw new Exception(string.Join("\n", arithmeticResult.Diagnostics));
var nativeArithmetic = Path.Combine(output, "ArithmeticDependency.neo.json");
File.WriteAllBytes(nativeArithmetic, RuntimeAssemblyContainer.Read(arithmeticOutput.ToArray()));
var arithmeticMetadata = NativeAssemblyDefinition.ReadAssembly(File.ReadAllBytes(nativeArithmetic));
var arithmeticReferencePath = Path.Combine(output, "ArithmeticDependency.dll");
File.WriteAllBytes(arithmeticReferencePath, arithmeticOutput.ToArray());
var arithmeticReference = MetadataReference.CreateFromFile(arithmeticReferencePath);
var arithmeticDefinition = RuntimeAssemblyContainer.ReadCliProjection(File.ReadAllBytes(arithmeticReferencePath));
const string librarySource = """
public static class MathLibrary {
    static func Twice() -> int {
        return 7
    }
    static func Twice(value: int) -> int {
        return Multiply(value, 2)
    }
    static func Multiply(value: int, factor: int) -> int {
        return Arithmetic.Multiply(value, factor)
    }
}
""";
File.WriteAllText(Path.Combine(output, "Library.rvn"), librarySource);
var libraryCompilation = Compilation.Create("MetadataProbeLibrary", [SyntaxTree.ParseText(librarySource, path: "Library.rvn")],
    [MetadataReference.CreateFromFile(typeof(object).Assembly.Location), arithmeticReference], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
var libraryOptions = new NeoClrEmitOptions(new("MetadataProbeLibrary", new Version(1, 0, 0, 0)), core, [new NeoClrMetadataDependency(arithmeticReference, arithmeticDefinition, core)]);
using var libraryOutput = new MemoryStream();
var libraryResult = NeoClrCompilationEmitter.EmitMetadataAssembly(libraryCompilation, libraryOutput, libraryOptions);
if (!libraryResult.Success) throw new Exception(string.Join("\n", libraryResult.Diagnostics));
LibraryChecks.Run(libraryCompilation, librarySource, libraryOptions, RuntimeAssemblyContainer.Read(libraryOutput.ToArray()));
var libraryPath = Path.Combine(output, "MetadataProbeLibrary.dll");
var nativeLibrary = Path.Combine(output, "MetadataProbeLibrary.neo.json");
File.WriteAllBytes(nativeLibrary, RuntimeAssemblyContainer.Read(libraryOutput.ToArray()));
var nativeMetadata = NativeAssemblyDefinition.ReadAssembly(File.ReadAllBytes(nativeLibrary));
if (nativeMetadata.References.Count != 1 || !nativeMetadata.References[0].Equals(arithmeticMetadata.Identity))
    throw new Exception("native outer dependency identity not preserved");
File.WriteAllBytes(libraryPath, libraryOutput.ToArray());
var metadata = RuntimeAssemblyContainer.ReadCliProjection(File.ReadAllBytes(libraryPath));
const string source = """
func Offset(value: int) -> int {
    return value + 2
}
func Main() -> int {
    return Offset(MathLibrary.Twice(20))
}
""";
File.WriteAllText(Path.Combine(output, "Program.rvn"), source);
var libraryReference = MetadataReference.CreateFromFile(libraryPath);
var emitOptions = new NeoClrEmitOptions(new("MetadataProbeApp", new Version(1, 0, 0, 0)), core,
    [new NeoClrMetadataDependency(libraryReference, metadata, core)]);
Compilation CreateCompilation(string code)
{
    var tree = SyntaxTree.ParseText(code);
    return Compilation.Create("MetadataProbeApp", [tree], [
        MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
        libraryReference], new CompilationOptions(OutputKind.ConsoleApplication));
}
using var nativeOutput = new MemoryStream();
var applicationCompilation = CreateCompilation(source);
if (applicationCompilation.GetTypeByMetadataName("Arithmetic") is not null)
    throw new Exception("implementation-only transitive type leaked into application binding");
if (metadata.MainModule.AssemblyReferences.Any(r => r.Identity.Equals(arithmeticMetadata.Identity)))
    throw new Exception("primitive reference projection leaked an implementation dependency");
var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(applicationCompilation, nativeOutput, emitOptions);
if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
var image = nativeOutput.ToArray();
var application = Path.Combine(output, "MetadataProbeApp.dll");
File.WriteAllBytes(application, image);
AdapterChecks.Run(CreateCompilation, source, emitOptions);
var multiFileImages = MultiFileChecks.Run(CreateCompilation(source), source, emitOptions, output);
await Command(0, "verify", application, "--module", libraryPath, "--module", arithmeticReferencePath);
var result = await Command(42, "run", application, "--module", libraryPath, "--module", arithmeticReferencePath, "--show-result");
if (!result.Contains("=> Int32(42)")) throw new Exception("wrong runtime result");
var multiFilePaths = new List<string>();
for (int i = 0; i < multiFileImages.Length; i++)
{
    var path = Path.Combine(output, $"MultiFile{i}.dll");
    File.WriteAllBytes(path, RuntimeAssemblyContainer.WriteBinary(multiFileImages[i], core));
    multiFilePaths.Add(path);
    await Command(0, "verify", path, "--module", libraryPath, "--module", arithmeticReferencePath);
    var multiResult = await Command(42, "run", path, "--module", libraryPath, "--module", arithmeticReferencePath, "--show-result");
    if (!multiResult.Contains("=> Int32(42)")) throw new Exception("wrong multi-file runtime result");
}
var missingDependency = await Command(1, "verify", application);
if (!missingDependency.Contains("missing referenced module")) throw new Exception("missing dependency diagnostic unavailable");
var wrongRevision = JsonNode.Parse(File.ReadAllBytes(nativeLibrary))!;
wrongRevision["revision"] = "2.0.0.0";
var wrongRevisionPath = Path.Combine(output, "WrongRevision.neo.json");
File.WriteAllText(wrongRevisionPath, wrongRevision.ToJsonString());
var mismatchedDependency = await Command(1, "verify", application, "--module", wrongRevisionPath, "--module", arithmeticReferencePath);
if (!mismatchedDependency.Contains("module revision mismatch")) throw new Exception("wrong revision diagnostic unavailable");
var missingTransitive = await Command(1, "verify", application, "--module", libraryPath);
if (!missingTransitive.Contains("missing referenced module")) throw new Exception("missing transitive dependency diagnostic unavailable");
var wrongArithmetic = JsonNode.Parse(File.ReadAllBytes(nativeArithmetic))!;
wrongArithmetic["revision"] = "2.0.0.0";
var wrongArithmeticPath = Path.Combine(output, "WrongArithmeticRevision.neo.json");
File.WriteAllText(wrongArithmeticPath, wrongArithmetic.ToJsonString());
var wrongTransitive = await Command(1, "verify", application, "--module", libraryPath, "--module", wrongArithmeticPath);
if (!wrongTransitive.Contains("module revision mismatch")) throw new Exception("wrong transitive revision diagnostic unavailable");
await Command(0, "verify", application, "--module", arithmeticReferencePath, "--module", libraryPath);
var reverseOrder = await Command(42, "run", application, "--module", arithmeticReferencePath, "--module", libraryPath, "--show-result");
if (!reverseOrder.Contains("=> Int32(42)")) throw new Exception("reversed module order returned wrong result");
var helloPaths = await HelloWorldChecks.Run(core, output, Command);
var namespacePaths = await NamespaceChecks.Run(core, output, Command);
await SharedLoweringChecks.Run(core, output, Command);
if (args.Length == 4) await DriverChecks.Run(Path.GetFullPath(args[3]), output, Command);
File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
{
    date = "2026-10-01",
    result = 42,
    sharedDotNetAndNativeBodyLowering = true,
    compilerLoweredBodyInput = true,
    sharedCallableIdentityResolution = true,
    sharedSourceCallablePlans = true,
    nativeAssemblyFunctionOwnershipAndEmptyTypes = true,
    implicitInt32Returns = true,
    namedUnitCall = true,
    sharedUnitFunctionsMethodsAndEntry = true,
    sharedCallableSignatureDeclarations = true,
    helloWorldDirectAndFunctionCall = true,
    unitEntryPointsAndZeroExit = true,
    compilerDriverNativeCommand = args.Length == 4,
    compilerDriverSha256 = args.Length == 4 ? Hash(args[3]) : null,
    namespacedLibrariesAndBothFileOrders = true,
    namespacePeSha256 = namespacePaths.Select(Hash).ToArray(),
    unitHelpersExplicitAndImplicitReturns = true,
    importedUnitAndInt32Overloads = true,
    unsupportedUnitCallsPreserveOutput = true,
    helloWorldPeSha256 = helloPaths.Select(Hash).ToArray(),
    source = "Raven compiler-lowered bound bodies",
    metadataLibraryIndependent = true,
    importedReadOnlyDependency = true,
    emitterRequiresDependencyBuilder = false,
    semanticLoader = "existing .NET provider over projected native declarations",
    nativeDependencyInput = true,
    dependencyCompiledFromRaven = true,
    libraryLocalCallAndOverload = true,
    libraryVisibilityChecksPassed = true,
    transitiveDependencyCompiledFromRaven = true,
    transitiveTypeAbsentFromApplicationBinding = true,
    missingTransitiveDependencyRejected = true,
    wrongTransitiveRevisionRejected = true,
    bothRuntimeModuleOrdersExecuted = true,
    transitiveDependencySha256 = Hash(nativeArithmetic),
    transitiveReferenceProjectionSha256 = Hash(arithmeticReferencePath),
    missingDependencyRejected = true,
    wrongRevisionRejected = true,
    referenceOnlyProjection = true,
    referenceProjectionSha256 = Hash(libraryPath),
    emitter = "compiler-owned opt-in adapter to PE/#Neo native format 5",
    diagnosticAndStreamContractsChecked = true,
    multiFileCrossFunctionCalls = true,
    bothFileOrdersExecuted = true,
    laterFileDiagnosticChecked = true,
    multiFileApplicationSha256 = multiFilePaths.Select(Hash).ToArray(),
    adapterSha256 = Hash(typeof(NeoClrCompilationEmitter).Assembly.Location),
    productionTargetIntegrated = false,
    nativeMetadataLoader = true,
    nativeCompilerSymbolProvider = false,
    nativePayloadEncoding = "CBOR execution schema 2, semantic format 5",
    parsingSpeedupMeasured = false, // This correctness probe does not run the separate runtime benchmark.
    runtimePayloadRequiresJsonParsing = false,
    runtimeContainer = "PE/#Neo required native execution section 256 schema 2",
    samePeFilesUsedForCompilerReferencesAndRuntime = true,
    arbitraryCilExecution = false,
    unsupportedOperationRejected = true,
    bindingErrorRejected = true,
    compilerSha256 = Hash(typeof(Compilation).Assembly.Location),
    metadataApiSha256 = Hash(typeof(AssemblyBuilder).Assembly.Location),
    runtimeSha256 = Hash(runtime),
    applicationSha256 = Hash(application),
    dependencySha256 = Hash(libraryPath)
}, new JsonSerializerOptions { WriteIndented = true }) + "\n");
Console.WriteLine("PASS Raven compiler-lowered bodies -> separate metadata library -> neoCLR: 42");

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
