using System.Diagnostics;
using System.Security.Cryptography;
using System.Reflection;
using System.Text.Json;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

using MetadataReference = Raven.CodeAnalysis.MetadataReference;
using AssemblyBuilder = NeoCLR.Metadata.Experimental.Model.AssemblyBuilder;

namespace NeoClrMetadataProbe;

internal static class NativeSymbolChecks
{
    internal static async Task RunRuntime(string corePath, string runtime, string system, string output)
    {
        Run(corePath, output);
        var results = new List<object>();
        foreach (var (app, dependency) in new[] { ("NativeConsumer", "NativeSymbols"), ("RavenNativeConsumer", "RavenNativeLibrary"), ("NativeTypeConsumer", "NativeTypeLibrary"), ("NativeFieldConsumer", "NativeTypeLibrary"), ("ExternalNativeConsumer", "NativeHolderLibrary"), ("NativeInterfaceConsumer", "NativeInterfaceLibrary"), ("NativeGenericConsumer", "NativeGenericLibrary") })
        {
            var appPath = Path.Combine(output, app + ".dll");
            var dependencyPath = Path.Combine(output, dependency + ".dll");
            var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var arg in new[] { "run", appPath, "--module", dependencyPath, "--system", system, "--show-result" }) start.ArgumentList.Add(arg);
            var additionalDependency = app == "ExternalNativeConsumer" ? "NativePayloadLibrary" : app == "NativeInterfaceConsumer" ? "NativeInterfaceStorageLibrary" : null;
            if (additionalDependency is not null)
            {
                start.ArgumentList.Add("--module");
                start.ArgumentList.Add(Path.Combine(output, additionalDependency + ".dll"));
            }
            using var process = Process.Start(start) ?? throw new Exception("runtime did not start");
            var stdoutTask = process.StandardOutput.ReadToEndAsync();
            var stderrTask = process.StandardError.ReadToEndAsync();
            using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(60));
            try { await process.WaitForExitAsync(timeout.Token); }
            catch { process.Kill(entireProcessTree: true); throw; }
            var stdout = await stdoutTask;
            var stderr = await stderrTask;
            if (process.ExitCode != 42 || (stdout + stderr).Trim() != "=> Int32(42)") throw new Exception($"native runtime failed: {process.ExitCode} {stdout} {stderr}");
            results.Add(new { app, dependency, exitCode = process.ExitCode, stdout, stderr, appSha256 = Hash(appPath), dependencySha256 = Hash(dependencyPath), additionalDependency, additionalDependencySha256 = additionalDependency is not null ? Hash(Path.Combine(output, additionalDependency + ".dll")) : null });
        }
        File.WriteAllText(Path.Combine(output, "runtime-validation.json"), JsonSerializer.Serialize(new
        {
            passed = true,
            runtimeSha256 = Hash(runtime),
            systemSha256 = Hash(system),
            coreSha256 = Hash(corePath),
            results,
            scope = "native primitive/nominal signature import and explicit dependency execution; explicit CLI primitive core retained"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS native metadata import, emission and runtime execution");
        static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
    }

    internal static void Run(string corePath, string output)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var name = AssemblyName.GetAssemblyName(corePath);
        var core = new AssemblyIdentity(name.Name!, name.Version!, name.CultureName ?? "", Convert.ToHexString(name.GetPublicKeyToken() ?? []));
        var builder = new AssemblyBuilder(new("NativeSymbols", new Version(1, 0, 0, 0)), core);
        var echo = builder.AddFunction("Example", "Echo", new MethodSignature(PrimitiveType.Int32, [PrimitiveType.Int32]));
        echo.LoadArgument(0); echo.Return();
        var boolean = builder.AddFunction("Example", "Echo", new MethodSignature(PrimitiveType.Boolean, [PrimitiveType.Boolean]));
        boolean.LoadArgument(0); boolean.Return();
        var hidden = builder.AddFunction("Example", "Hidden", new MethodSignature(PrimitiveType.Int32, []), MethodVisibility.Internal);
        hidden.LoadConstant(0); hidden.Return();
        var image = RuntimeAssemblyContainer.WriteBinary(builder.WriteNativeAssembly(), core);
        File.WriteAllBytes(Path.Combine(output, "NativeSymbols.dll"), image);
        var native = NeoClrMetadataReference.ReadAssembly(image);
        var cliCore = MetadataReference.CreateFromFile(corePath);
        Compilation Create(string source, params MetadataReference[] references) => Compilation.Create("NativeConsumer",
            [SyntaxTree.ParseText(source)], references, CompilationOptions.NeoCLR);
        const string source = "import Example.*\nfunc Main() -> int { return Echo(42) }";
        foreach (var refs in new MetadataReference[][] { [cliCore, native], [native, cliCore] })
        {
            var compilation = Create(source, refs);
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception(string.Join("; ", errors.Select(d => d.ToString())));
            var tree = compilation.SyntaxTrees.Single();
            var call = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
            var model = compilation.GetSemanticModel(tree);
            var method = model.GetSymbolInfo(call).Symbol as IMethodSymbol;
            if (method is null || method.ContainingType is not null || method.ContainingAssembly?.Name != "NativeSymbols" || method.ReturnType.SpecialType != SpecialType.System_Int32 ||
                model.GetTypeInfo(call).Type?.SpecialType != SpecialType.System_Int32 || !ReferenceEquals(method, model.GetSymbolInfo(call).Symbol)) throw new Exception("native method identity/type mismatch");
            if (!ReferenceEquals(method.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(native))) throw new Exception("assembly identity mismatch");
            using var cliOutput = new MemoryStream();
            if (compilation.Emit(cliOutput).Success || cliOutput.Length != 0) throw new Exception("CLI emitter admitted native reference");
            using var nativeOutput = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, nativeOutput, new(new("NativeConsumer", new Version(1, 0, 0, 0)), core, [new(native, native.Definition, core)]));
            if (!emitted.Success || nativeOutput.Length == 0) throw new Exception("native call emission failed: " + string.Join("; ", emitted.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "NativeConsumer.dll"), nativeOutput.ToArray());
            using var unboundOutput = new MemoryStream();
            var unbound = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, unboundOutput,
                new(new("NativeConsumer", new Version(1, 0, 0, 0)), core, []));
            if (unbound.Success || unboundOutput.Length != 0 || !unbound.Diagnostics.Any(d => d.Id == "NEOMETA001")) throw new Exception("unbound native dependency admitted");
            using var invalidOutput = new MemoryStream();
            var mismatched = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, invalidOutput,
                new(new("NativeConsumer", new Version(1, 0, 0, 0)), core, [new(native, NeoClrMetadataReference.ReadAssembly(image).Definition, core)]));
            if (mismatched.Success || invalidOutput.Length != 0 || !mismatched.Diagnostics.Any(d => d.Id == "NEOMETA002")) throw new Exception("native snapshot mismatch admitted");
            var other = Create(source, refs); _ = other.GetDiagnostics();
            if (ReferenceEquals(compilation.GetAssemblyOrModuleSymbol(native), other.GetAssemblyOrModuleSymbol(native))) throw new Exception("symbols leaked across compilations");
        }
        foreach (var invalid in new[] { "import Example.*\nfunc Main() -> int { return Hidden() }", "import Example.*\nfunc Main() -> int { return Echo(\"wrong\") }" })
            if (!Create(invalid, cliCore, native).GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error)) throw new Exception("invalid native call accepted");
        if (!Create(source, cliCore, native, NeoClrMetadataReference.ReadAssembly(image)).GetDiagnostics().Any(d => d.Id == "RAVT003")) throw new Exception("duplicate identity accepted");
        var dotnet = Compilation.Create("WrongTarget", [SyntaxTree.ParseText(source)], [cliCore, native], CompilationOptions.DotNet);
        if (!dotnet.GetDiagnostics().Any(d => d.Id == "RAVT003")) throw new Exception("native reference admitted by .NET target");
        var caller = new AssemblyBuilder(new("RequiresNativeSymbols", new Version(1, 0, 0, 0)), core);
        var forward = caller.AddFunction("Forward"); forward.LoadConstant(42); forward.Call(echo); forward.Return();
        var dependency = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteBinary(caller.WriteNativeAssembly(), core));
        if (!Create(source, cliCore, dependency).GetDiagnostics().Any(d => d.Id == "RAVT003")) throw new Exception("missing native dependency accepted");
        var differentVersion = new AssemblyBuilder(new("NativeSymbols", new Version(2, 0, 0, 0)), core);
        var versionMethod = differentVersion.AddFunction("Version"); versionMethod.LoadConstant(2); versionMethod.Return();
        var versionReference = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteBinary(differentVersion.WriteNativeAssembly(), core));
        if (!Create(source, cliCore, dependency, versionReference).GetDiagnostics().Any(d => d.Id == "RAVT003")) throw new Exception("wrong dependency version accepted");
        var versions = Create("func Main() -> int { return 42 }", cliCore, native, versionReference);
        if (versions.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error)) throw new Exception("distinct native versions rejected");
        if (SymbolEqualityComparer.Default.Equals(versions.GetAssemblyOrModuleSymbol(native), versions.GetAssemblyOrModuleSymbol(versionReference))) throw new Exception("native versions collapsed by name");
        var complete = Create(source, cliCore, dependency, native);
        if (complete.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error)) throw new Exception("registered native dependency failed");
        var assembly = (IAssemblySymbol)complete.GetAssemblyOrModuleSymbol(dependency)!;
        if (!ReferenceEquals(assembly.Modules.Single().ReferencedAssemblySymbols.Single(), complete.GetAssemblyOrModuleSymbol(native))) throw new Exception("dependency symbol identity mismatch");
        const string librarySource = "namespace SourceLibrary\npublic func Twice(value: int) -> int { return value + value }";
        File.WriteAllText(Path.Combine(output, "RavenNativeLibrary.rvn"), librarySource);
        var producer = Compilation.Create("RavenNativeLibrary", [SyntaxTree.ParseText(librarySource)], [cliCore],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var producerOutput = new MemoryStream();
        var produced = NeoClrCompilationEmitter.EmitMetadataAssembly(producer, producerOutput,
            new(new("RavenNativeLibrary", new Version(1, 0, 0, 0)), core, []));
        if (!produced.Success) throw new Exception("native source producer failed: " + string.Join("; ", produced.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "RavenNativeLibrary.dll"), producerOutput.ToArray());
        var producedReference = NeoClrMetadataReference.ReadAssembly(producerOutput.ToArray());
        var sourceConsumer = Compilation.Create("RavenNativeConsumer",
            [SyntaxTree.ParseText("import SourceLibrary.*\nfunc Main() -> int { return Twice(21) }")],
            [cliCore, producedReference], CompilationOptions.NeoCLR);
        using var consumerOutput = new MemoryStream();
        var consumed = NeoClrCompilationEmitter.EmitMetadataAssembly(sourceConsumer, consumerOutput,
            new(new("RavenNativeConsumer", new Version(1, 0, 0, 0)), core, [new(producedReference, producedReference.Definition, core)]));
        if (!consumed.Success) throw new Exception("native source consumer failed: " + string.Join("; ", consumed.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "RavenNativeConsumer.dll"), consumerOutput.ToArray());
        NativeTypeChecks.Run(cliCore, core, output);
        ExternalNativeChecks.Run(cliCore, core, output);
        NativeInterfaceChecks.Run(cliCore, core, output);
        NativeGenericSymbolChecks.Run(cliCore, core, output);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new { passed = true, nativeSha256 = Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), coreSha256 = Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(File.ReadAllBytes(corePath))), cases = new[] { "native generic static/namespace calls, inference, forwarding, overloads and method-scoped vectors", "external nominal method/constructor/field signatures", "primitive and external-class array signatures and aliasing", "native property identity, instance/static calls and setter accessibility", "native namespace overloads", "both reference orders", "semantic type and symbol identity", "compilation isolation", "accessibility", "invalid argument", "CLI emission leaves output empty", "native call emission", "Raven-produced native library read and consumed", "native snapshot mismatch leaves output empty", "duplicate identity", "wrong target", "missing dependency", "registered dependency", "exact version identity", "native class/field symbols, nominal function/method/constructor signatures, overloads, stateful instance calls and primitive/nominal field load/store" }, scope = "direct native dependency symbols with explicit CLI primitive core; native call emitted; runtime execution validated separately" }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS direct native semantic imports");
    }
}
