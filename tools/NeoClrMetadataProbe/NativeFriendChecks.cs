using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeFriendChecks
{
    internal static void Run(string corePath, string output)
    {
        var bytes = File.ReadAllBytes(corePath);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var core = AssemblyDefinition.ReadAssembly(bytes, false).Identity;
        var provider = new AssemblyBuilder(new("FriendProvider", new(1, 0, 0, 0)), core);
        var marker = provider.AddClass("System.Runtime.CompilerServices", "InternalsVisibleToAttribute");
        var ctor = marker.AddConstructor(new MethodSignature(PrimitiveType.Void, [PrimitiveType.String]));
        ctor.GetILGenerator().Fail("Friend metadata must not execute its constructor");
        provider.AddCustomAttribute(new(ctor.Definition, [new(PrimitiveType.String, "FriendConsumer")]));
        var hidden = provider.AddClass("Tests", "Hidden", visibility: TypeVisibility.Internal);
        var answer = hidden.AddMethod("Answer", new(PrimitiveType.Int32, []), MethodVisibility.Internal);
        answer.LoadConstant(35);
        answer.Return();
        hidden.AddField("Value", PrimitiveType.Int32, FieldVisibility.Internal);
        hidden.AddConstructor(new MethodSignature(PrimitiveType.Void, []), MethodVisibility.Internal).Return();
        var reference = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(provider), bootstrap);
        var syntax = SyntaxTree.ParseText("""
            func Main() -> int {
                let value = Tests.Hidden()
                value.Value = 7
                return value.Value + Tests.Hidden.Answer()
            }
            """);
        Compilation Create(string name) => Compilation.Create(name, [syntax], [bootstrap.Reference, reference],
            CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name).WithRuntimeTypeOfContract(null));
        Directory.CreateDirectory(output);
        File.WriteAllBytes(Path.Combine(output, "Provider.neo.json"), provider.WriteNativeAssembly());
        foreach (var name in new[] { "FriendConsumer", "Unrelated" })
        {
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.Emit(Create(name), image,
                new(new(name, new(1, 0, 0, 0)), core, [new NeoClrMetadataDependency(reference, core)], bootstrapReference: bootstrap.Reference));
            if (name == "Unrelated")
            {
                if (result.Success || !result.Diagnostics.Any(d => d.Id == "RAV0500") || image.Length != 0)
                    throw new Exception("Unrelated native consumer must fail without output");
            }
            else
            {
                if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
                File.WriteAllBytes(Path.Combine(output, "Consumer.neo.json"), image.ToArray());
            }
        }
        Console.WriteLine("PASS native friend-only type/constructor/field/method emission and unrelated-consumer rejection");
    }
}
