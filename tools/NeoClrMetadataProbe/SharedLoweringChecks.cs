using System.Reflection;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class SharedLoweringChecks
{
    internal static async Task Run(AssemblyIdentity core, string output, Func<int, string[], Task<string>> command)
    {
        await RunCase("SharedLowering", """
            public static class Arithmetic {
                public static func Main() -> int {
                    System.Console.WriteLine("Shared Hello")
                    return Twice(20 - 1) + 4
                }
                public static func Twice(value: int) -> int {
                    return value * 2
                }
            }
            """, "Shared Hello", 42);
        await RunCase("SharedUnitFunctions", """
            func Main() {
                Greet()
            }
            func Greet() {
                System.Console.WriteLine("Shared Hello")
            }
            """, "Shared Hello", null);
        await RunCase("SharedUnitMethods", """
            public static class Hello {
                public static func Main() {
                    Greet(42)
                    return
                }
                public static func Greet(value: int) {
                    System.Console.WriteLine("Shared Hello")
                    return
                }
            }
            """, "Shared Hello", null);
        await RunCase("SharedEmptyUnit", "func Main() { }", "", null);
        Console.WriteLine("PASS shared lowering on .NET and neoCLR: Int32 arithmetic/calls, Unit functions/methods, explicit/implicit returns and empty entry");

        async Task RunCase(string name, string source, string expectedOutput, int? expectedResult)
        {
            var console = MetadataReference.CreateFromFile(typeof(Console).Assembly.Location);
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(source)],
                [MetadataReference.CreateFromFile(typeof(object).Assembly.Location), console,
                    MetadataReference.CreateFromFile(Assembly.Load("System.Runtime").Location)],
                new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
            using var cli = new MemoryStream();
            var cliResult = compilation.Emit(cli);
            if (!cliResult.Success) throw new Exception(string.Join("\n", cliResult.Diagnostics));
            var entry = Assembly.Load(cli.ToArray()).EntryPoint ?? throw new Exception("missing CLI entry");
            if (entry.ReturnType != (expectedResult.HasValue ? typeof(int) : typeof(void)))
                throw new Exception("shared lowering CLI return signature: " + name);
            var previousOutput = Console.Out;
            using var captured = new StringWriter();
            try
            {
                Console.SetOut(captured);
                if (!Equals(entry.Invoke(null, null), expectedResult))
                    throw new Exception("shared lowering .NET result: " + name);
            }
            finally { Console.SetOut(previousOutput); }
            if (captured.ToString().Trim() != expectedOutput) throw new Exception("shared lowering .NET console output: " + name);
            using var native = new MemoryStream();
            var options = new NeoClrEmitOptions(new(name, new Version(1, 0, 0, 0)), core, [], console);
            var nativeResult = compilation.Emit(native, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options)));
            if (!nativeResult.Success) throw new Exception(string.Join("\n", nativeResult.Diagnostics));
            var path = Path.Combine(output, name + ".dll");
            File.WriteAllBytes(path, native.ToArray());
            File.WriteAllText(Path.Combine(output, name + ".rvn"), source);
            await command(0, ["verify", path]);
            var executed = await command(expectedResult ?? 0, ["run", path]);
            if (executed.Trim() != expectedOutput) throw new Exception("shared lowering native console output: " + name + ": " + executed);
        }
    }
}
