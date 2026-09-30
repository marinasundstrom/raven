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
            """,
            """
            func Greet() {
                System.Console.WriteLine("Hello World")
            }
            func Main() -> int {
                Greet()
                return 0
            }
            """,
            """
            public static class Greetings {
                public static func Greet(value: int) {
                    System.Console.WriteLine("Hello World")
                    return
                }
            }
            func Main() -> int {
                Greetings.Greet(42)
                return 0
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
            if (i == 3) Reject(Compile(sources[i].Replace("Greet(42)", "Greet(value: 42)")), options);
        }
        const string librarySource = """
            public static class Greetings {
                public static func Greet(value: int) {
                    System.Console.WriteLine("Hello World")
                }
                public static func Greet() -> int {
                    return 0
                }
            }
            """;
        var library = Compilation.Create("GreetingLibrary",
            [SyntaxTree.ParseText(librarySource, path: "GreetingLibrary.rvn")], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var libraryResult = NeoClrCompilationEmitter.EmitMetadataAssembly(library, libraryImage,
            new(new("GreetingLibrary", new Version(1, 0, 0, 0)), core, [], console));
        if (!libraryResult.Success) throw new Exception(string.Join("\n", libraryResult.Diagnostics));
        var libraryBytes = libraryImage.ToArray();
        var libraryPath = Path.Combine(output, "GreetingLibrary.dll");
        File.WriteAllBytes(libraryPath, libraryBytes);
        File.WriteAllText(Path.Combine(output, "GreetingLibrary.rvn"), librarySource);
        var dependency = MetadataReference.CreateFromFile(libraryPath);
        var definition = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(libraryBytes);
        var greet = definition.MainModule.Types.Single(t => t.Name == "Greetings").Methods.Single(m =>
            m.Name == "Greet" && m.TryGetStaticInt32Signature(out var count, out var returnsValue) && count == 1 && !returnsValue);
        if (greet is null) throw new Exception("no-result method projection missing");
        const string consumerSource = """
            func Main() -> int {
                Greetings.Greet(42)
                return Greetings.Greet()
            }
            """;
        Compilation Consumer(string source) => Compilation.Create("GreetingConsumer",
            [SyntaxTree.ParseText(source, path: "GreetingConsumer.rvn")], [.. references, dependency],
            new CompilationOptions(OutputKind.ConsoleApplication));
        var consumerOptions = new NeoClrEmitOptions(new("GreetingConsumer", new Version(1, 0, 0, 0)), core,
            [new NeoClrMetadataDependency(dependency, definition, core)]);
        using var consumerImage = new MemoryStream();
        var consumerResult = NeoClrCompilationEmitter.EmitMetadataAssembly(Consumer(consumerSource), consumerImage, consumerOptions);
        if (!consumerResult.Success) throw new Exception(string.Join("\n", consumerResult.Diagnostics));
        var consumerPath = Path.Combine(output, "GreetingConsumer.dll");
        File.WriteAllBytes(consumerPath, consumerImage.ToArray());
        File.WriteAllText(Path.Combine(output, "GreetingConsumer.rvn"), consumerSource);
        await command(0, ["verify", consumerPath, "--module", libraryPath]);
        if ((await command(0, ["run", consumerPath, "--module", libraryPath])).Replace("\r\n", "\n") != "Hello World\n")
            throw new Exception("unexpected imported no-result call output");
        Reject(Consumer(consumerSource.Replace("Greetings.Greet(42)", "Greetings.Greet()")), consumerOptions);
        Reject(Consumer(consumerSource.Replace("Greetings.Greet(42)", "System.Console.WriteLine(42)")), consumerOptions);
        Reject(Consumer("func Main() { }"), consumerOptions);
        paths.Add(libraryPath);
        paths.Add(consumerPath);
        Console.WriteLine("PASS Hello World, Unit helpers, explicit/implicit returns, imported Unit and Int32 overloads, rejected discarded values");
        return paths.ToArray();
    }
    private static void Reject(Compilation compilation, NeoClrEmitOptions options, string diagnostic = "NEOMETA001")
    {
        using var output = new MemoryStream(); output.WriteByte(77);
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, output, options);
        if (result.Success || !result.Diagnostics.Any(d => d.Id == diagnostic) || output.Length != 1 || output.Position != 1)
            throw new Exception("unsupported native source did not preserve failed output: " + string.Join("; ", result.Diagnostics));
    }
}
