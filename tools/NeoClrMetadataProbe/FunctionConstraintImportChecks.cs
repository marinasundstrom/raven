using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class FunctionConstraintImportChecks
{
    internal static void Run(string corePath)
    {
        var bytes = File.ReadAllBytes(corePath);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var core = AssemblyDefinition.ReadAssembly(bytes, false).Identity;
        var builder = new AssemblyBuilder(new("FunctionConstraintImport", new(1, 0, 0, 0)), core);
        builder.AddGenericClass("Example", "Signal", ["T"]).SetSpecialConstraints(0, TypeParameterConstraints.FunctionType);
        builder.AddGenericClass("Example", "Unconstrained", ["T"]);
        var keep = builder.AddFunction("Keep", new(SignatureType.MethodParameter(0), [SignatureType.MethodParameter(0)], ["F"]));
        keep.SetSpecialConstraints(0, TypeParameterConstraints.FunctionType);
        keep.LoadArgument(0); keep.Return();
        var reference = NeoClrMetadataReference.ReadAssembly(NativeModuleContainer.WriteLibraryBinary(builder), bootstrap);
        Compilation Create(string source) => Compilation.Create("Consumer", [SyntaxTree.ParseText(source)],
            [bootstrap.Reference, reference],
            CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name).WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null));
        var compilation = Create("");
        compilation.GetDiagnostics();
        var assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
        var parameter = assembly.GetTypeByMetadataName("Example.Signal`1")!.TypeParameters.Single();
        Check(parameter.ConstraintKind == TypeParameterConstraintKind.FunctionType, "native function constraint was erased");
        Check(parameter.SatisfiesConstraints(parameter), "constrained symbolic argument cannot forward");
        var free = assembly.GetTypeByMetadataName("Example.Unconstrained`1")!.TypeParameters.Single();
        Check(!free.SatisfiesConstraints(parameter), "unconstrained symbolic argument accepted");
        foreach (var argument in new[] { "int", "System.Func<int, int>", "System.Action", "Invokable" })
        {
            var invalid = Create($$"""
                class Invokable { func Invoke() -> int => 42 }
                func Reject(value: Example.Signal<{{argument}}>) -> int => 0
                """);
            var diagnostics = invalid.GetDiagnostics();
            Check(diagnostics.Any(d => d.Id == "RAV0320"),
                "function constraint ignored for " + argument + ": " + string.Join("; ", diagnostics));
            Check(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error && d.Id != "RAV0320"),
                "unrelated failure for " + argument + ": " + string.Join("; ", diagnostics));
        }
        var valid = Create("func Accept(value: Example.Signal<(int) -> int>) -> int => 42");
        Check(!valid.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "structural function argument rejected");
        var method = Create("func Keep<F>(callback: F) -> F where F: func => callback");
        Check(!method.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "function-constrained method binding failed");
        using var methodImage = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(method, methodImage,
            new(new("Consumer", new(1, 0, 0, 0)), core, [new(reference, core)]));
        Check(emitted.Success && methodImage.Length > 0,
            "method constraint emission failed: " + string.Join("; ", emitted.Diagnostics));
        var emittedDefinition = AssemblyDefinition.ReadNativeAssembly(methodImage.ToArray()).MainModule.Functions.Single(f => f.Name == "Keep");
        Check(emittedDefinition.SpecialConstraints[0] == TypeParameterConstraints.FunctionType, "emitted method constraint erased");
        var importedMethod = Create("func Forward<F>(f: F) -> F where F: func => Keep<F>(f)");
        Check(!importedMethod.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "imported function constraint forwarding failed");
        using var consumerImage = new MemoryStream();
        var consumerEmit = NeoClrCompilationEmitter.EmitMetadataAssembly(importedMethod, consumerImage,
            new(new("Consumer", new(1, 0, 0, 0)), core, [new(reference, core)]));
        Check(consumerEmit.Success, "imported constrained method emission failed: " + string.Join("; ", consumerEmit.Diagnostics));
        foreach (var source in new[] { "func Bad() -> int => Keep<int>(42)", "func Bad<F>(f: F) -> F => Keep<F>(f)" })
            Check(Create(source).GetDiagnostics().Any(d => d.Id == "RAV0320"), "imported method constraint not enforced");
        var ordinary = Create("func Accept(value: Example.Unconstrained<int>) -> int => 42");
        Check(!ordinary.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "unconstrained import regressed: " + string.Join("; ", ordinary.GetDiagnostics()));
        Console.WriteLine("PASS native function constraint import, symbolic forwarding and invalid argument diagnostics");
    }

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}
