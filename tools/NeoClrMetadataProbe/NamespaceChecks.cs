using RuntimeAssemblyContainer = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NamespaceChecks
{
    internal static async Task<string[]> Run(AssemblyIdentity core, string output, Func<int, string[], Task<string>> command)
    {
        var primitive = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        string[] sources = [
            """
            namespace Example {
                namespace First {
                    internal static class Hidden {
                        public static func Value() -> int { Example.Second.Math.Value() }
                    }
                    public static class Math {
                        public static func Value() -> int {
                            return Hidden.Value() + 2
                        }
                    }
                }
            }
            """,
            """
            namespace Example.Second
            public static class Math {
                public static func Value() -> int {
                    return 20
                }
            }
            """
        ];
        var trees = sources.Select((s, i) => SyntaxTree.ParseText(s, path: $"NamespaceLibrary{i}.rvn")).ToArray();
        var paths = new List<string>();
        foreach (var reverse in new[] { false, true })
        {
            var name = "NamespaceLibrary" + (reverse ? "Reverse" : "Forward");
            var library = Compilation.Create(name, reverse ? trees.Reverse().ToArray() : trees, [primitive],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var options = new NeoClrEmitOptions(new(name, new Version(1, 0, 0, 0)), core, []);
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(library, image, options);
            Check(result.Success, string.Join("\n", result.Diagnostics));
            var path = Path.Combine(output, name + ".dll");
            File.WriteAllBytes(path, image.ToArray());
            paths.Add(path);
            var snapshot = RuntimeAssemblyContainer.ReadCliProjection(image.ToArray());
            Check(snapshot.MainModule.Types.Where(t => t.Name == "Math").Select(t => t.Namespace).Order().SequenceEqual(
                new[] { "Example.First", "Example.Second" }), "namespaces were not preserved in the reference projection");
            var reference = MetadataReference.CreateFromFile(path);
            var forbidden = Compilation.Create(name + "Forbidden", [SyntaxTree.ParseText("func Main() -> int { Example.First.Hidden.Value() }")],
                [primitive, reference], new CompilationOptions(OutputKind.ConsoleApplication));
            Check(forbidden.GetDiagnostics().Any(d => d.Id == "RAV0500"), "external internal type access must fail: " + string.Join("; ", forbidden.GetDiagnostics()));
            // The raw metadata API deliberately permits references without source access checks.
            // The runtime must reject the same forbidden call after binary loading.
            var raw = new AssemblyBuilder(new(name + "Denied", new Version(1, 0, 0, 0)), core);
            var entry = raw.AddFunction("Main");
            entry.Call(raw.ImportReference(snapshot.MainModule.Types.Single(t => t.Name == "Hidden").Methods.Single(), core));
            entry.Return(); raw.EntryPoint = entry;
            var deniedPath = Path.Combine(output, name + "Denied.dll");
            File.WriteAllBytes(deniedPath, RuntimeAssemblyContainer.WriteBinary(raw.WriteNativeAssembly(), core));
            Check((await command(1, ["verify", deniedPath, "--module", path])).Contains("type access denied"), "runtime internal type access enforcement");
            const string consumerSource = """
                import Example.First.*
                func Main() -> int {
                    return Math.Value() + Example.Second.Math.Value()
                }
                """;
            var consumerName = name + "Consumer";
            var consumer = Compilation.Create(consumerName, [SyntaxTree.ParseText(consumerSource)], [primitive, reference],
                new CompilationOptions(OutputKind.ConsoleApplication));
            using var consumerImage = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(consumer, consumerImage,
                new(new(consumerName, new Version(1, 0, 0, 0)), core, [new(reference, snapshot, core)]));
            Check(emitted.Success, string.Join("\n", emitted.Diagnostics));
            var consumerPath = Path.Combine(output, consumerName + ".dll");
            File.WriteAllBytes(consumerPath, consumerImage.ToArray());
            File.WriteAllText(Path.Combine(output, consumerName + ".rvn"), consumerSource);
            paths.Add(consumerPath);
            await command(0, ["verify", consumerPath, "--module", path]);
            Check((await command(42, ["run", consumerPath, "--module", path, "--show-result"])).Contains("=> Int32(42)"), "namespace call result");
        }
        for (var i = 0; i < sources.Length; i++) File.WriteAllText(Path.Combine(output, $"NamespaceLibrary{i}.rvn"), sources[i]);
        // Namespace-owned free functions have no native namespace contract yet; never flatten them.
        foreach (var source in new[] {
            "namespace Example\nfunc Main() -> int { return 0 }",
            "namespace Example { public static class Outer { public static class Inner { } } }"
        })
        {
            var tree = SyntaxTree.ParseText(source, path: "RejectedNamespace.rvn");
            var compilation = Compilation.Create("RejectedNamespace", [tree], [primitive], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            using var stream = new MemoryStream();
            stream.WriteByte(77);
            var rejected = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, stream,
                new(new("RejectedNamespace", new Version(1, 0, 0, 0)), core, []));
            Check(!rejected.Success && rejected.Diagnostics.Any(d => d.Id == "NEOMETA001" && ReferenceEquals(d.Location.SourceTree, tree)) &&
                stream.Position == 1 && stream.ToArray().SequenceEqual(new byte[] { 77 }), "namespace rejection: " + string.Join("; ", rejected.Diagnostics));
        }
        Console.WriteLine("PASS nested/file namespaces, same-name types, imported and qualified calls, both file orders, namespace rejection contracts");
        return paths.ToArray();
    }

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}
