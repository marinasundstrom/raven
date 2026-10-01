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
        await RunCase("InternalHelper", """
            func Main() -> int { Hidden.Value() }
            internal static class Hidden {
                public static func Value() -> int { 42 }
            }
            """, "", 42);
        await RunCase("SharedStrings", """
            func Main() -> int {
                var message = ""
                message = Text.Choose(true)
                System.Console.WriteLine(message)
                if true {
                    System.Console.WriteLine(Text.Choose(false))
                }
                Text.Choose(true)
                return 42
            }
            public static class Text {
                public static func Echo(value: string) -> string { value }
                public static func Choose(selected: bool) -> string {
                    if selected {
                        return Echo("Hej 🌍 café")
                    }
                    return Echo("Done")
                }
            }
            """, "Hej 🌍 café\nDone", 42);
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
        await RunCase("SharedLoopExits", """
            func Main() -> int {
                var index = 0
                var result = 0
                while true {
                    index = index + 1
                    if index == 3 {
                        continue
                    }
                    if index >= 7 {
                        break
                    }
                    if !(index != 6) {
                        result = result + 6
                    } else {
                        if index <= 5 {
                            result = result + index
                        }
                    }
                }
                return result * 2 + 6
            }
            """, "", 42);
        await RunCase("SharedPrimitiveSignatures", """
            func Main() -> int {
                Helpers.Choose(42, Helpers.Identity(Helpers.Positive(1)))
            }
            public static class Helpers {
                public static func Positive(value: int) -> bool {
                    value > 0
                }
                public static func Identity(value: bool) -> bool {
                    value
                }
                public static func Identity(value: int) -> int {
                    value
                }
                public static func Choose(value: int, selected: bool) -> int {
                    if selected {
                        return Identity(value)
                    }
                    return 0
                }
            }
            """, "", 42);
        await RunCase("SharedBooleanLocals", """
            public static class Selection {
                public static func Main() -> int {
                    var selected = Positive(1)
                    var result = 0
                    if selected != false {
                        result = 40
                    }
                    selected = !selected
                    if selected == false {
                        result = result + 2
                    }
                    return result
                }
                public static func Positive(value: int) -> bool {
                    value > 0
                }
            }
            """, "", 42);
        await RunCase("SharedShortCircuit", """
            func Main() -> int {
                var selected = false && Mark()
                var result = 0
                selected = true || Mark()
                if selected {
                    result = 40
                }
                selected = true && Mark()
                selected = false || Mark()
                if (false || selected) && (true || Mark()) {
                    result = result + 2
                }
                return result
            }
            func Mark() -> bool {
                System.Console.WriteLine("Evaluated")
                return true
            }
            """, "Evaluated\nEvaluated", 42);
        await RunCase("SharedDiscardedResults", """
            func Main() -> int {
                Number()
                Predicate()
                Finish()
                return 42
            }
            func Number() -> int {
                System.Console.WriteLine("Number")
                return 7
            }
            func Predicate() -> bool {
                System.Console.WriteLine("Predicate")
                return true
            }
            func Finish() {
                System.Console.WriteLine("Finish")
            }
            """, "Number\nPredicate\nFinish", 42);
        await RunCase("SharedInt64", """
            public static class Wide {
                public static func Main() -> int {
                    let value: long = Widen(42)
                    let high = 4294967296L
                    let total = value + high
                    let maximum = 9223372036854775807L
                    let minimum = 0L - maximum - 1L
                    if Narrow(maximum) != (0 - 1) { return 1 }
                    if Narrow(minimum) != 0 { return 2 }
                    if Widen(0 - 1) < 0L {
                        return Narrow(total)
                    }
                    return 0
                }
                public static func Widen(value: int) -> long { return value }
                public static func Narrow(value: long) -> int { return (int)value }
            }
            """, "", 42);
        await RunCase("SharedUnaryIntegers", """
            public static class Unary {
                public static func Main() -> int {
                    let minimum = -9223372036854775807L - 1L
                    if Negate64(minimum) != minimum { return 1 }
                    if Negate32(-2147483647 - 1) != (-2147483647 - 1) { return 2 }
                    return Positive(Negate32(-21)) + (int)Complement64(-22L)
                }
                public static func Negate32(value: int) -> int { return -value }
                public static func Negate64(value: long) -> long { return -value }
                public static func Complement32(value: int) -> int { return ~value }
                public static func Complement64(value: long) -> long { return ~value }
                public static func Positive(value: int) -> int { return +value }
            }
            """, "", 42);
        await RunCase("SharedBitwise", """
            func Main() -> int {
                if And32(-1, 42) != 42 { return 1 }
                if Or32(-2147483647 - 1, 42) != (-2147483647 - 1 + 42) { return 2 }
                if Xor32(-1, 0) != -1 { return 3 }
                if And64(-1L, 4294967296L) != 4294967296L { return 4 }
                if Or64(4294967296L, 42L) != 4294967338L { return 5 }
                if Xor64(-1L, -1L) != 0L { return 6 }
                return Xor32(And32(63, 40), Or32(0, 2))
            }
            func And32(a: int, b: int) -> int { a & b }
            func Or32(a: int, b: int) -> int { a | b }
            func Xor32(a: int, b: int) -> int { a ^ b }
            func And64(a: long, b: long) -> long { a & b }
            func Or64(a: long, b: long) -> long { a | b }
            func Xor64(a: long, b: long) -> long { a ^ b }
            """, "", 42);
        await RunCase("SharedShifts", """
            func Main() -> int {
                if Left32(1, 31) != (-2147483647 - 1) { return 1 }
                if Right32(-2, 1) != -1 { return 2 }
                if Left64(1L, 40) != 1099511627776L { return 3 }
                if Right64(-9223372036854775807L - 1L, 63) != -1L { return 4 }
                if Left64(9223372036854775807L, 1) != -2L { return 5 }
                if Right64(1099511627776L, 40) != 1L { return 6 }
                if Left32(42, 0) != 42 { return 7 }
                return Left32(21, 1)
            }
            func Left32(value: int, count: int) -> int { value << count }
            func Right32(value: int, count: int) -> int { value >> count }
            func Left64(value: long, count: int) -> long { value << count }
            func Right64(value: long, count: int) -> long { value >> count }
            """, "", 42);
        await RunCase("SharedMethodVisibility", """
            func Main() -> int { Facade.Value() }
            public static class Helpers {
                private static func Hidden() -> int { 20 }
                internal static func Internal() -> int { Hidden() + 1 }
            }
            public static class Facade {
                public static func Value() -> int { Helpers.Internal() * 2 }
            }
            """, "", 42);
        await RunCase("SharedArrowBodies", """
            func Main() -> int => Helpers.Value(20)
            public static class Helpers {
                public static func Value(value: int) -> int => Twice(value + 1)
                private static func Twice(value: int) -> int => value * 2
            }
            """, "", 42);
        await RunCase("SharedArrowPrimitives", """
            func Main() -> int {
                Helpers.Print(Helpers.Text("Arrow 🌍"))
                Helpers.Finish()
                if Helpers.Positive(1) && Helpers.Widen(42) == 42L && Helpers.Wide(5000000000L) == 5000000001L {
                    return 42
                }
                return 0
            }
            public static class Helpers {
                public static func Wide(value: long) -> long => value + 1L
                public static func Widen(value: int) -> long => value
                public static func Positive(value: int) -> bool => value > 0
                public static func Text(value: string) -> string => value
                public static func Print(value: string) => System.Console.WriteLine(value)
                public static func Finish() => Empty()
                private static func Empty() { }
            }
            """, "Arrow 🌍", 42);
        await RunCase("SharedArrowUnitEntry", """
            func Main() => Greet()
            func Greet() => System.Console.WriteLine("Arrow Hello")
            """, "Arrow Hello", null);
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
