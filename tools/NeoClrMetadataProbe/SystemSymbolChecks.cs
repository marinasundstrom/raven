using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class SystemSymbolChecks
{
    internal static async Task Run(string runtime, string driver, string systemPath, string output)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var system = NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(systemPath));
        var function = system.Functions.Single(f => f.Name == "System.Math.Min" && f.TryGetStaticInt32Signature(out var n) && n == 2);
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var identity = new AssemblyIdentity("SystemSymbolView", new Version(1, 0, 0, 0));
        var projectionPath = Path.Combine(output, "SystemSymbolView.dll");
        File.WriteAllBytes(projectionPath, system.CreateStaticInt32ReferenceAssembly(identity, core, [function]));
        var reference = MetadataReference.CreateFromFile(projectionPath);
        const string source = "func Main() -> int { return System.Math.Min(42, 99) }";
        var sourcePath = Path.Combine(output, "SystemConsumer.rvn");
        File.WriteAllText(sourcePath, source);
        var tree = SyntaxTree.ParseText(source, path: sourcePath);
        var compilation = Compilation.Create("SystemConsumer", [tree], [MetadataReference.CreateFromFile(typeof(object).Assembly.Location), reference],
            new CompilationOptions(OutputKind.ConsoleApplication));
        Check(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "native symbols did not bind: " + string.Join("; ", compilation.GetDiagnostics()));
        var call = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = compilation.GetSemanticModel(tree).GetSymbolInfo(call).Symbol as IMethodSymbol;
        Check(method is not null && SymbolEqualityComparer.Default.Equals(method.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(reference)) &&
            method.Parameters.Length == 2 && method.ReturnType.SpecialType == SpecialType.System_Int32, "call did not bind to the native projection");
        var symbols = new NeoClrSystemSymbols(reference, identity.Name, system, [function]);
        var options = new NeoClrEmitOptions(new("SystemConsumer", new Version(1, 0, 0, 0)), core, [], systemSymbols: symbols);
        using var image = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, options);
        Check(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var application = Path.Combine(output, "SystemConsumer.dll");
        File.WriteAllBytes(application, image.ToArray());
        await Command(runtime, 0, "verify", application, "--system", systemPath);
        Check((await Command(runtime, 42, "run", application, "--system", systemPath, "--show-result")).Contains("=> Int32(42)"), "native System result");
        using var rejected = new MemoryStream(); rejected.WriteByte(77);
        var noBinding = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, rejected, new(options.Identity, core, []));
        Check(!noBinding.Success && noBinding.Diagnostics.Any(d => d.Id == "NEOMETA001") && rejected.ToArray().SequenceEqual(new byte[] { 77 }), "unbound native call emitted");
        var cliApp = Path.Combine(output, "DriverSystemConsumer.dll");
        await Command("dotnet", 0, driver, "neoclr", "--system-symbols", systemPath, "--system-method", "System.Math.Min/2", "-o", cliApp, sourcePath);
        await Command(runtime, 0, "verify", cliApp, "--system", systemPath);
        Check((await Command(runtime, 42, "run", cliApp, "--system", systemPath, "--show-result")).Contains("=> Int32(42)"), "driver native System result");
        var badOutput = Path.Combine(output, "Rejected.dll");
        await Command("dotnet", 1, driver, "neoclr", "--system-symbols", systemPath, "--system-method", "System.Math.Abs/1", "-o", badOutput, sourcePath);
        await Command("dotnet", 1, driver, "neoclr", "--system-symbols", systemPath, "--system-method", "System.Math.Max/2", "-o", badOutput, sourcePath);
        await Command("dotnet", 1, driver, "neoclr", "--system-symbols", systemPath, "--system-method", "System.Date.DaysBeforeYear/1", "-o", badOutput, sourcePath);
        Check(!File.Exists(badOutput), "unsupported System selection created output");
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            date = "2026-09-30",
            systemModule = system.ModuleName,
            typeCount = system.TypeNames.Count,
            functionCount = system.Functions.Count,
            selectedFunction = function.Name,
            selectedTableIndex = function.TableIndex,
            result = 42,
            nativeSymbolBinding = true,
            fullCoreImport = false,
            existingDotNetImporterReused = true,
            systemSha256 = Hash(systemPath),
            driverSha256 = Hash(driver),
            applicationSha256 = Hash(application),
            cliApplicationSha256 = Hash(cliApp),
            runtimeSha256 = Hash(runtime)
        }, new JsonSerializerOptions { WriteIndented = true }));
        Console.WriteLine($"PASS native System symbols: {system.TypeNames.Count} types/{system.Functions.Count} functions inventoried; selected callable binds and executes through API and rvnc");
    }
    private static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path))).ToLowerInvariant();
    private static async Task<string> Command(string executable, int expected, params string[] arguments)
    {
        var start = new ProcessStartInfo(executable) { RedirectStandardOutput = true, RedirectStandardError = true };
        foreach (var arg in arguments) start.ArgumentList.Add(arg);
        using var process = Process.Start(start)!;
        var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
        await process.WaitForExitAsync();
        var text = await stdout + await stderr;
        Check(process.ExitCode == expected, $"exit {process.ExitCode}, expected {expected}: {text}");
        return text;
    }
    private static void Check(bool value, string message) { if (!value) throw new Exception(message); }
}
