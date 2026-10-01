using System.Reflection;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class IntegerArithmeticChecks
{
    internal static async Task Run(AssemblyIdentity core, string output, Func<int, string[], Task<string>> command)
    {
        await Check("Quotients", """
            func Main() -> int {
                if Divide(-43, 2) != -21 { return 1 }
                if Divide(43, -2) != -21 { return 2 }
                if Divide(-43, -2) != 21 { return 3 }
                if Wide(-4294967338L, 2L) != -2147483669L { return 4 }
                if Wide(1L, 2L) != 0L { return 5 }
                return Divide(85, 2)
            }
            func Divide(left: int, right: int) -> int { left / right }
            func Wide(left: long, right: long) -> long { left / right }
            """, null, null);
        await Check("Remainders", """
            func Main() -> int {
                if Remainder(-43, 2) != -1 { return 1 }
                if Remainder(43, -2) != 1 { return 2 }
                if Remainder(-43, -2) != -1 { return 3 }
                if Wide(-4294967339L, 2L) != -1L { return 4 }
                if Wide(1L, 2L) != 1L { return 5 }
                return Remainder(85, 43)
            }
            func Remainder(left: int, right: int) -> int { left % right }
            func Wide(left: long, right: long) -> long { left % right }
            """, null, null);
        foreach (var wide in new[] { false, true })
        {
            var type = wide ? "long" : "int";
            var minimum = wide ? "(-9223372036854775807L - 1L)" : "(-2147483647 - 1)";
            var suffix = wide ? "L" : "";
            await Check("DivideZero" + type, Source("1" + suffix, "0" + suffix), typeof(DivideByZeroException), "division by zero");
            await Check("DivideOverflow" + type, Source(minimum, "-1" + suffix), typeof(ArithmeticException), "overflow");
            await Check("RemainderZero" + type, Source("1" + suffix, "0" + suffix).Replace("left / right", "left % right"), typeof(DivideByZeroException), "division by zero");
            await Check("RemainderOverflow" + type, Source(minimum, "-1" + suffix).Replace("left / right", "left % right"), typeof(ArithmeticException), "overflow");
            string Source(string left, string right) => $$"""
                func Main() -> int {
                    Divide({{left}}, {{right}})
                    return 42
                }
                func Divide(left: {{type}}, right: {{type}}) -> {{type}} {
                    left / right
                }
                """;
        }
        Console.WriteLine("PASS signed Int32/Int64 division/remainder results and execution faults on .NET and binary neoCLR");

        async Task Check(string name, string source, Type? fault, string? nativeMessage)
        {
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(source)],
                [MetadataReference.CreateFromFile(typeof(object).Assembly.Location)],
                new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
            using var cli = new MemoryStream();
            var emitted = compilation.Emit(cli);
            if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
            try
            {
                var result = Assembly.Load(cli.ToArray()).EntryPoint!.Invoke(null, null);
                if (fault is not null || !Equals(result, 42)) throw new Exception(name + " did not produce expected CLI result/fault");
            }
            catch (TargetInvocationException error) when (fault is not null && error.InnerException is not null && fault.IsInstanceOfType(error.InnerException)) { }
            using var native = new MemoryStream();
            var options = new NeoClrEmitOptions(new(name, new Version(1, 0, 0, 0)), core, []);
            var resultNative = compilation.Emit(native, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options)));
            if (!resultNative.Success) throw new Exception(string.Join("\n", resultNative.Diagnostics));
            var path = Path.Combine(output, name + ".dll");
            File.WriteAllBytes(path, native.ToArray());
            File.WriteAllText(Path.Combine(output, name + ".rvn"), source);
            await command(0, ["verify", path]);
            var actual = await command(fault is null ? 42 : 1, ["run", path]);
            if (nativeMessage is not null && !actual.Contains(nativeMessage)) throw new Exception(name + ": " + actual);
        }
    }
}
