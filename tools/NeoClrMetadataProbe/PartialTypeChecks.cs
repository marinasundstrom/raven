using System.Reflection;

using NeoCLR.Metadata.Experimental.Model;

using RuntimeAssemblyContainer = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class PartialTypeChecks
{
    internal static async Task Run(AssemblyIdentity core, string output, Func<int, string[], Task<string>> command)
    {
        string[] sources = [
            """
            namespace Example {
                public static partial class Program {
                    public static func Main() -> int { Value(20) + Value() }
                }
            }
            """,
            """
            namespace Example {
                public static partial class Program {
                    public static func Value(value: int) -> int { value * 2 }
                    public static func Value() -> int { 2 }
                }
            }
            """,
            "namespace Example { public static partial class Program { } }"
        ];
        var trees = sources.Select((s, i) => SyntaxTree.ParseText(s, path: $"Partial{i}.rvn")).ToArray();
        var primitive = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        for (var i = 0; i < sources.Length; i++) File.WriteAllText(Path.Combine(output, $"Partial{i}.rvn"), sources[i]);
        foreach (var reverse in new[] { false, true })
        {
            var name = "PartialTypes" + (reverse ? "Reverse" : "Forward");
            var compilation = Compilation.Create(name, reverse ? trees.Reverse().ToArray() : trees, [primitive],
                new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
            using var cli = new MemoryStream();
            var result = compilation.Emit(cli);
            Check(result.Success, string.Join("\n", result.Diagnostics));
            var assembly = Assembly.Load(cli.ToArray());
            Check(Equals(assembly.EntryPoint!.Invoke(null, null), 42), "partial .NET result");
            Check(assembly.GetTypes().Count(t => t.FullName == "Example.Program") == 1, "partial .NET type identity");
            using var native = new MemoryStream();
            var options = new NeoClrEmitOptions(new(name, new Version(1, 0, 0, 0)), core, []);
            result = compilation.Emit(native, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options)));
            Check(result.Success, string.Join("\n", result.Diagnostics));
            var projection = RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            var type = projection.MainModule.Types.Single(t => t.Namespace == "Example" && t.Name == "Program");
            Check(type.Methods.Count == 3, "partial native members missing or duplicated");
            var path = Path.Combine(output, name + ".dll");
            File.WriteAllBytes(path, native.ToArray());
            await command(0, ["verify", path]);
            await command(42, ["run", path]);
        }
        var unsupported = SyntaxTree.ParseText("""
            namespace Example {
                public static partial class Program {
                    public static val Number: int => 42
                }
            }
            """, path: "UnsupportedPart.rvn");
        foreach (var reverse in new[] { false, true })
        {
            var parts = trees.Append(unsupported).ToArray();
            var compilation = Compilation.Create("RejectedPartial", reverse ? parts.Reverse().ToArray() : parts, [primitive],
                new CompilationOptions(OutputKind.ConsoleApplication));
            using var stream = new MemoryStream();
            stream.WriteByte(77);
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, stream,
                new(new("RejectedPartial", new Version(1, 0, 0, 0)), core, []));
            Check(!result.Success && result.Diagnostics.Any(d => d.Id == "NEOMETA001" && ReferenceEquals(d.Location.SourceTree, unsupported)) &&
                stream.Position == 1 && stream.ToArray().SequenceEqual(new byte[] { 77 }), "unsupported partial member must reject without output: " + string.Join("; ", result.Diagnostics));
        }
        Console.WriteLine("PASS partial static type identity, overloads, both file orders and unsupported-part diagnostics");
    }

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}
