using System.Reflection;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeFunctionNullabilityChecks
{
    internal static void Run(string corePath)
    {
        var coreReference = MetadataReference.CreateFromFile(corePath);
        var host = AssemblyName.GetAssemblyName(corePath);
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        const string source = """
            public class Failure { }
            public interface Writer {
                func Write<T>(state: T, formatter: (T, Failure?) -> string?)
            }
            """;
        var library = Compilation.Create("NullableCallbacks", [SyntaxTree.ParseText(source)], [coreReference], CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(library, image, new(new("NullableCallbacks", new(1, 0, 0, 0)), core, []));
        if (!emitted.Success) throw new Exception(string.Join("; ", emitted.Diagnostics));
        var reference = NeoClrMetadataReference.ReadAssembly(image.ToArray());
        const string consumer = """
            func Forward<T>(writer: Writer, state: T, formatter: (T, Failure?) -> string?) {
                writer.Write<T>(state, formatter)
            }
            """;
        var compilation = Compilation.Create("Consumer", [SyntaxTree.ParseText(consumer)], [coreReference, reference], CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("; ", errors.AsEnumerable()));
        var assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
        var method = assembly.GetTypeByMetadataName("Writer")!.GetMembers("Write").OfType<IMethodSymbol>().Single();
        var invoke = ((INamedTypeSymbol)method.Parameters[1].Type).GetMembers("Invoke").OfType<IMethodSymbol>().Single();
        if (!invoke.Parameters[1].Type.IsNullable || !invoke.ReturnType.IsNullable)
            throw new Exception("nested callback nullability was lost");
        using var result = new MemoryStream();
        emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, result, new(new("Consumer", new(1, 0, 0, 0)), core, [new(reference, core)]));
        if (!emitted.Success) throw new Exception(string.Join("; ", emitted.Diagnostics));
        Console.WriteLine("PASS imported generic callback parameter/result nullability and forwarding");
    }
}
