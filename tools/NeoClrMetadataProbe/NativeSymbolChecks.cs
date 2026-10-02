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
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, nativeOutput, new(new("NativeConsumer", new Version(1, 0, 0, 0)), core, []));
            if (emitted.Success || nativeOutput.Length != 0 || !emitted.Diagnostics.Any(d => d.Id == "NEOMETA001")) throw new Exception("native call adapter boundary not diagnosed: " + string.Join("; ", emitted.Diagnostics));
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
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new { passed = true, nativeSha256 = Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), coreSha256 = Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(File.ReadAllBytes(corePath))), cases = new[] { "native namespace overloads", "both reference orders", "semantic type and symbol identity", "compilation isolation", "accessibility", "invalid argument", "unsupported emission leaves output empty", "duplicate identity", "wrong target", "missing dependency", "registered dependency", "exact version identity" }, scope = "direct native dependency symbols with explicit CLI primitive core; native emission not yet exercised" }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS direct native semantic imports");
    }
}
