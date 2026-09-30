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
        const string source = """
            public static class Arithmetic {
                public static func Main() -> int {
                    System.Console.WriteLine("Shared Hello")
                    return Twice(20 - 1) + 4
                }
                public static func Twice(value: int) -> int {
                    return value * 2
                }
            }
            """;
        var console = MetadataReference.CreateFromFile(typeof(Console).Assembly.Location);
        var compilation = Compilation.Create("SharedLowering", [SyntaxTree.ParseText(source)],
            [MetadataReference.CreateFromFile(typeof(object).Assembly.Location), console,
                MetadataReference.CreateFromFile(Assembly.Load("System.Runtime").Location)],
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
        using var cli = new MemoryStream();
        var cliResult = compilation.Emit(cli);
        if (!cliResult.Success) throw new Exception(string.Join("\n", cliResult.Diagnostics));
        var previousOutput = Console.Out;
        using var captured = new StringWriter();
        try
        {
            Console.SetOut(captured);
            if (!Equals(Assembly.Load(cli.ToArray()).GetType("Arithmetic")!.GetMethod("Main")!.Invoke(null, null), 42))
                throw new Exception("shared lowering .NET result");
        }
        finally { Console.SetOut(previousOutput); }
        if (captured.ToString().Trim() != "Shared Hello") throw new Exception("shared lowering .NET console output");
        using var native = new MemoryStream();
        var options = new NeoClrEmitOptions(new("SharedLowering", new Version(1, 0, 0, 0)), core, [], console);
        var nativeResult = compilation.Emit(native, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options)));
        if (!nativeResult.Success) throw new Exception(string.Join("\n", nativeResult.Diagnostics));
        var path = Path.Combine(output, "SharedLowering.dll");
        File.WriteAllBytes(path, native.ToArray());
        File.WriteAllText(Path.Combine(output, "SharedLowering.rvn"), source);
        await command(0, ["verify", path]);
        var executed = await command(42, ["run", path, "--show-result"]);
        if (!executed.Contains("=> Int32(42)") || !executed.Contains("Shared Hello"))
            throw new Exception("shared lowering native result");
        Console.WriteLine("PASS same compilation and shared body lowering execute to 42 through .NET and neoCLR builders");
    }
}
