using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class HelloWorldChecks
{
    internal static async Task<string[]> Run(AssemblyIdentity core, string output, Func<int, string[], Task<string>> command)
    {
        var console = MetadataReference.CreateFromFile(typeof(Console).Assembly.Location);
        var references = new[] {
            MetadataReference.CreateFromFile(typeof(object).Assembly.Location), console,
            MetadataReference.CreateFromFile(System.Reflection.Assembly.Load("System.Runtime").Location)
        };
        string[] sources = [
            """
            func Main() -> int {
                System.Console.WriteLine("Hello World")
                return 0
            }
            """,
            """
            func Greet() -> int {
                System.Console.WriteLine("Hello World")
                return 0
            }
            func Main() -> int {
                return Greet()
            }
            """
        ];
        var paths = new List<string>();
        for (int i = 0; i < sources.Length; i++)
        {
            var name = "HelloWorld" + i;
            Compilation Compile(string source) => Compilation.Create(name,
                [SyntaxTree.ParseText(source, path: name + ".rvn")], references, new CompilationOptions(OutputKind.ConsoleApplication));
            var options = new NeoClrEmitOptions(new(name, new Version(1, 0, 0, 0)), core, [], console);
            var compilation = Compile(sources[i]);
            using var image = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, options);
            if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
            var path = Path.Combine(output, name + ".dll"); paths.Add(path);
            File.WriteAllText(Path.Combine(output, name + ".rvn"), sources[i]);
            File.WriteAllBytes(path, image.ToArray());
            await command(0, ["verify", path]);
            if ((await command(0, ["run", path])).Replace("\r\n", "\n") != "Hello World\n")
                throw new Exception("unexpected Hello World stdout");
            Reject(compilation, new(options.Identity, core, []));
            Reject(compilation, new(options.Identity, core, [], references[0]));
            Reject(compilation, new(options.Identity, core, [], MetadataReference.CreateFromFile(typeof(Console).Assembly.Location)), "NEOMETA002");
            Reject(Compile(sources[i].Replace("WriteLine(\"Hello World\")", "WriteLine(42)")), options);
            Reject(Compile(sources[i].Replace("WriteLine(\"Hello World\")", "Write(\"Hello World\")")), options);
        }
        Console.WriteLine("PASS Hello World directly and via an entry-point function call");
        return paths.ToArray();
    }
    private static void Reject(Compilation compilation, NeoClrEmitOptions options, string diagnostic = "NEOMETA001")
    {
        using var output = new MemoryStream(); output.WriteByte(77);
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, output, options);
        if (result.Success || !result.Diagnostics.Any(d => d.Id == diagnostic) || output.Length != 1 || output.Position != 1)
            throw new Exception("unsupported console mapping did not preserve failed output");
    }
}
