using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

using MetadataReference = Raven.CodeAnalysis.MetadataReference;

namespace NeoClrMetadataProbe;

internal static class ExternalNativeChecks
{
    internal static void Run(MetadataReference coreReference, AssemblyIdentity core, string output)
    {
        var payload = Library("NativePayloadLibrary", """
            namespace External
            public class Payload {
                public field Value: int
                public init(value: int) {
                    self.Value = value
                }
            }
            """, []);
        var holder = Library("NativeHolderLibrary", """
            namespace External
            public class Holder {
                public field Item: Payload
                public init(item: Payload) {
                    self.Item = item
                }
                public static func Pass(value: Payload) -> Payload => value
            }
            """, [payload]);
        const string source = """
            import External.*
            func Main() -> int {
                let first = Payload(1)
                let holder = Holder(Holder.Pass(first))
                holder.Item = Payload(42)
                if first.Value != 1 { return 2 }
                return holder.Item.Value
            }
            """;
        File.WriteAllText(Path.Combine(output, "ExternalNativeConsumer.rvn"), source);
        foreach (var references in new MetadataReference[][] { [coreReference, holder, payload], [payload, holder, coreReference] })
        {
            var compilation = Compilation.Create("ExternalNativeConsumer", [SyntaxTree.ParseText(source)], references, CompilationOptions.NeoCLR);
            Check(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", compilation.GetDiagnostics()));
            var a = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(payload)!;
            var b = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(holder)!;
            var payloadType = a.GetTypeByMetadataName("External.Payload")!;
            var holderType = b.GetTypeByMetadataName("External.Holder")!;
            var pass = holderType.GetMembers("Pass").OfType<IMethodSymbol>().Single();
            Check(ReferenceEquals(pass.ReturnType, payloadType) && ReferenceEquals(pass.Parameters[0].Type, payloadType) &&
                ReferenceEquals(holderType.GetMembers("Item").OfType<IFieldSymbol>().Single().Type, payloadType) &&
                ReferenceEquals(holderType.InstanceConstructors.Single().Parameters[0].Type, payloadType), "external canonical signature symbols");
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new("ExternalNativeConsumer", new Version(1, 0, 0, 0)), core, [new(payload, payload.Definition, core), new(holder, holder.Definition, core)]));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "ExternalNativeConsumer.dll"), image.ToArray());
        }
        var empty = new AssemblyBuilder(payload.Definition.Identity, core);
        empty.AddClass("External", "Different");
        var missingType = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteBinary(empty.WriteNativeAssembly(), core));
        var wrong = new AssemblyBuilder(new("NativePayloadLibrary", new Version(2, 0, 0, 0)), core);
        wrong.AddClass("External", "Payload");
        var wrongVersion = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteBinary(wrong.WriteNativeAssembly(), core));
        foreach (var references in new MetadataReference[][] { [coreReference, holder], [coreReference, holder, wrongVersion], [coreReference, holder, missingType], [coreReference, holder, payload, payload] })
        {
            var invalid = Compilation.Create("InvalidExternal", [SyntaxTree.ParseText("func Main() -> int { return 0 }")], references, CompilationOptions.NeoCLR);
            Check(invalid.GetDiagnostics().Any(d => d.Id == "RAVT003"), "invalid signature dependency accepted");
        }
        Console.WriteLine("PASS external native nominal signatures and exact dependency diagnostics");

        NeoClrMetadataReference Library(string name, string text, NeoClrMetadataReference[] dependencies)
        {
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(text)], [coreReference, .. dependencies],
                CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            using var image = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new(name, new Version(1, 0, 0, 0)), core, dependencies.Select(d => new NeoClrMetadataDependency(d, d.Definition, core)).ToArray()));
            Check(emitted.Success, name + ": " + string.Join("; ", emitted.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, name + ".dll"), image.ToArray());
            File.WriteAllText(Path.Combine(output, name + ".rvn"), text);
            return NeoClrMetadataReference.ReadAssembly(image.ToArray());
        }
    }
    private static void Check(bool value, string message) { if (!value) throw new Exception(message); }
}
