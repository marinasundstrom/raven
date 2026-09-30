using System.Reflection;
using System.Text.Json;

using RuntimeAssemblyContainer = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer;

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
                    Greet(value: 42)
                    return
                }
                public static func Greet(value: int) {
                    System.Console.WriteLine("Shared Hello")
                    return
                }
            }
            """, "Shared Hello", null);
        await RunCase("SharedImplicitValueReturn", """
            func Main() -> int {
                Arithmetic.Value(20)
            }
            public static class Arithmetic {
                public static func Value(value: int) -> int {
                    (value + 1) * 2
                }
            }
            """, "", 42);
        await RunCase("SharedCallableIdentities", """
            public static class Empty { }
            func Main() -> int {
                Value(5) + Value(5) + Alpha.Value(10) + Beta.Value(10) + Alpha.Value()
            }
            func Value(value: int) -> int {
                value
            }
            public static class Alpha {
                public static func Value(value: int) -> int {
                    value + 1
                }
                public static func Value() -> int {
                    9
                }
            }
            public static class Beta {
                public static func Value(value: int) -> int {
                    value + 2
                }
            }
            """, "", 42);
        await RunCase("SharedLocals", """
            func Main() -> int {
                let start = 20
                var result = Twice(start)
                result = result + 2
                result
            }
            func Twice(value: int) -> int {
                let factor = 2
                value * factor
            }
            """, "", 42);
        await RunCase("SharedControlFlow", """
            func Main() -> int {
                Accumulate(6)
            }
            func Accumulate(limit: int) -> int {
                var index = 0
                var result = 0
                while index < limit {
                    if index < 3 {
                        System.Console.WriteLine("Flow")
                        result = result + 5
                    } else {
                        result = result + 9
                    }
                    index = index + 1
                }
                return result
            }
            """, "Flow\nFlow\nFlow", 42);
        await RunCase("SharedEmptyUnit", "func Main() { }", "", null);
        Console.WriteLine("PASS shared lowering on .NET and neoCLR: Int32 arithmetic/calls, Unit functions/methods, explicit/implicit returns, named Unit call and empty entry");

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
            if (name == "SharedCallableIdentities")
            {
                using var metadata = JsonDocument.Parse(RuntimeAssemblyContainer.Read(native.ToArray()));
                var functions = metadata.RootElement.GetProperty("functions").EnumerateArray().ToArray();
                if (functions.Length != 5 || functions.Count(f => f.GetProperty("owner").ValueKind == JsonValueKind.Null) != 2)
                    throw new Exception("native source plans lost assembly/type ownership");
                if (metadata.RootElement.GetProperty("types").GetArrayLength() != 3)
                    throw new Exception("native declaration planning lost an empty type");
            }
            var path = Path.Combine(output, name + ".dll");
            File.WriteAllBytes(path, native.ToArray());
            File.WriteAllText(Path.Combine(output, name + ".rvn"), source);
            await command(0, ["verify", path]);
            var executed = await command(expectedResult ?? 0, ["run", path]);
            if (executed.Trim() != expectedOutput) throw new Exception("shared lowering native console output: " + name + ": " + executed);
        }
    }
}
