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
        var ordinary = Create("func Accept(value: Example.Unconstrained<int>) -> int => 42");
        Check(!ordinary.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "unconstrained import regressed: " + string.Join("; ", ordinary.GetDiagnostics()));
        Console.WriteLine("PASS native function constraint import, symbolic forwarding and invalid argument diagnostics");
    }

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}
