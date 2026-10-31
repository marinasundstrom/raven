using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;
using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

namespace NeoClrMetadataProbe;

internal static class ExplicitMethodChecks
{
    internal static void Run(string corePath, string output)
    {
        var bytes = File.ReadAllBytes(corePath);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var core = AssemblyDefinition.ReadAssembly(bytes, false).Identity;
        const string source = """
            public interface Left { func Read() -> int }
            public interface Right { func Read() -> bool }
            public class Both : Left, Right {
                func Left.Read() -> int => 42
                func Right.Read() -> bool => true
                func Read() -> int => 7
            }
            func Main() -> int {
                let value = Both()
                let left: Left = value
                let right: Right = value
                if !right.Read() || value.Read() != 7 { return 1 }
                return left.Read()
            }
            """;
        Compilation Create(string text) => Compilation.Create("ExplicitMethods", [SyntaxTree.ParseText(text)],
            [bootstrap.Reference], CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name).WithRuntimeTypeOfContract(null));
        using var image = new MemoryStream();
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(Create(source), image,
            new(new("ExplicitMethods", new(1, 0, 0, 0)), core, [], bootstrapReference: bootstrap.Reference));
        if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
        var snapshot = AssemblyDefinition.ReadNativeAssembly(image.ToArray());
        var methods = snapshot.MainModule.Types.Single(t => t.Name == "Both").Methods;
        foreach (var name in new[] { "Left.Read", "Right.Read" })
        {
            var method = methods.Single(m => m.Name.EndsWith(name));
            if ((method.Attributes & 7) != 1 || method.ExplicitInterfaceImplementations.Count != 1) throw new Exception("Explicit method lost private visibility");
        }
        var hidden = Create(source.Replace("func Read() -> int => 7", ""));
        if (!hidden.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error))
            throw new Exception("Explicit-only method leaked into ordinary member lookup");
        Directory.CreateDirectory(output);
        File.WriteAllBytes(Path.Combine(output, "ExplicitMethods.dll"), image.ToArray());
        Console.WriteLine("PASS native explicit method emission and visibility");
    }
}
