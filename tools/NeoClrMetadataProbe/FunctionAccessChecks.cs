using System.Reflection;

using RuntimeAssemblyContainer = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

using AssemblyBuilder = NeoCLR.Metadata.Experimental.Model.AssemblyBuilder;

namespace NeoClrMetadataProbe;

internal static class FunctionAccessChecks
{
    internal static async Task Run(AssemblyIdentity core, string output, Func<int, string[], Task<string>> command)
    {
        var primitive = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        var sources = new[] {
            """
            internal func AddOne(value: int) -> int => value + 1
            func Hidden() -> int => AddOne(20)
            public func Exported() -> int => Hidden() * 2
            """,
            """
            public static class Facade {
                public static func Value() -> int => Exported()
            }
            """
        };
        var trees = sources.Select((source, index) => SyntaxTree.ParseText(source, path: $"FunctionLibrary{index}.rvn")).ToArray();
        foreach (var reverse in new[] { false, true })
        {
            var name = "FunctionLibrary" + (reverse ? "Reverse" : "Forward");
            var library = Compilation.Create(name, reverse ? trees.Reverse().ToArray() : trees, [primitive],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            using var image = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(library, image,
                new(new(name, new Version(1, 0, 0, 0)), core, []));
            Check(emitted.Success, string.Join("; ", emitted.Diagnostics));
            var path = Path.Combine(output, name + ".dll");
            File.WriteAllBytes(path, image.ToArray());
            var snapshot = RuntimeAssemblyContainer.ReadCliProjection(image.ToArray());
            foreach (var function in new[] { "AddOne", "Hidden", "Exported" })
            {
                var definition = snapshot.MainModule.Methods.Single(m => m.Name == function);
                var expected = function == "Exported" ? MethodAttributes.Public : MethodAttributes.Assembly;
                Check(((MethodAttributes)definition.Attributes & MethodAttributes.MemberAccessMask) == expected, "function access projection: " + function);
            }
            var reference = MetadataReference.CreateFromFile(path);
            var consumerName = name + "Consumer";
            var consumer = Compilation.Create(consumerName, [SyntaxTree.ParseText("func Main() -> int => Facade.Value()")],
                [primitive, reference], new CompilationOptions(OutputKind.ConsoleApplication));
            using var application = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(consumer, application,
                new(new(consumerName, new Version(1, 0, 0, 0)), core, [new(reference, snapshot, core)]));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            var applicationPath = Path.Combine(output, consumerName + ".dll");
            File.WriteAllBytes(applicationPath, application.ToArray());
            await command(0, ["verify", applicationPath, "--module", path]);
            await command(42, ["run", applicationPath, "--module", path]);
            var raw = new AssemblyBuilder(new(name + "Denied", new Version(1, 0, 0, 0)), core);
            var entry = raw.AddFunction("Main");
            entry.Call(raw.ImportReference(snapshot.MainModule.Methods.Single(m => m.Name == "Hidden"), core));
            entry.Return(); raw.EntryPoint = entry;
            var denied = Path.Combine(output, name + "Denied.dll");
            File.WriteAllBytes(denied, RuntimeAssemblyContainer.WriteBinary(raw.WriteNativeAssembly(), core));
            Check((await command(1, ["verify", denied, "--module", path])).Contains("method access denied"), "external internal function access");
        }
        Console.WriteLine("PASS public/internal assembly functions, both file orders, public facade and native access enforcement");
    }

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}
