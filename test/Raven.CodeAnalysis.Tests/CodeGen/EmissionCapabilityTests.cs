using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class EmissionCapabilityTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release, false)]
    [InlineData(OptimizationLevel.Release, true)]
    [InlineData(OptimizationLevel.Debug, false)]
    [InlineData(OptimizationLevel.Debug, true)]
    public void SignedDivisionAndRemainderPreserveDotNetResultsAndFaults(OptimizationLevel optimization, bool remainder)
    {
        var compilation = Create("""
            public static class Arithmetic {
                public static func Divide(left: int, right: int) -> int { left / right }
                public static func Wide(left: long, right: long) -> long { left / right }
            }
            """.Replace("left / right", remainder ? "left % right" : "left / right"), optimization);
        foreach (var method in Methods(compilation))
            Assert.True(Lower(compilation, method, ReflectionEmitCapabilities.Shared, out _, out var failure), failure?.Detail);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var type = Assembly.Load(output.ToArray()).GetType("Arithmetic")!;
        Assert.Equal(remainder ? -1 : -21, type.GetMethod("Divide")!.Invoke(null, [-43, 2]));
        Assert.Equal(remainder ? -1L : -2147483669L, type.GetMethod("Wide")!.Invoke(null, [-4294967339L, 2L]));
        Assert.IsType<DivideByZeroException>(Assert.Throws<TargetInvocationException>(() => type.GetMethod("Divide")!.Invoke(null, [1, 0])).InnerException);
        Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => type.GetMethod("Wide")!.Invoke(null, [long.MinValue, -1L])).InnerException);
    }

    [Theory]
    [InlineData("&", 40)]
    [InlineData("|", 63)]
    [InlineData("^", 23)]
    public void IntegerBitwiseBodiesUseSharedPlan(string operation, int expected)
    {
        var compilation = Create($$"""
            public static class Bits {
                public static func Narrow(a: int, b: int) -> int { a {{operation}} b }
                public static func Wide(a: long, b: long) -> long { a {{operation}} b }
            }
            """);
        foreach (var method in Methods(compilation))
            Assert.True(Lower(compilation, method, ReflectionEmitCapabilities.Shared, out _, out var failure), failure?.Detail);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var type = Assembly.Load(output.ToArray()).GetType("Bits")!;
        Assert.Equal(expected, type.GetMethod("Narrow")!.Invoke(null, [63, 40]));
        Assert.Equal((long)expected, type.GetMethod("Wide")!.Invoke(null, [63L, 40L]));
    }

    [Fact]
    public void RestrictedInstructionProfileRejectsBeforeReturningAPlan()
    {
        var compilation = Create("public static class C { public static func Value(value: int) -> int { value / 2 } }");
        var restricted = new EmissionCapabilities([EmissionPrimitiveType.Int32], [LinearInstructionKind.Argument, LinearInstructionKind.Constant, LinearInstructionKind.Return]);
        Assert.False(Lower(compilation, Methods(compilation).Single(), restricted, out var body, out var failure));
        Assert.Null(body);
        Assert.Equal("value / 2", failure!.Syntax.ToString());
        Assert.Contains("Divide", failure.Detail);
    }

    [Theory]
    [InlineData("public static func Value(value: string) -> string { value }", "signature")]
    [InlineData("public static func Value() -> int { let text = \"hello\"; return 42 }", "local type")]
    [InlineData("public static func Value() -> bool { 1 < 2 }", "signature")]
    public void RestrictedTypeProfileRejectsUnsupportedContracts(string method, string detail)
    {
        var compilation = Create("public static class C { " + method + " }");
        var restricted = new EmissionCapabilities([EmissionPrimitiveType.NoResult, EmissionPrimitiveType.Int32], Enum.GetValues<LinearInstructionKind>());
        Assert.False(Lower(compilation, Methods(compilation).Single(), restricted, out var body, out var failure));
        Assert.Null(body);
        Assert.Contains(detail, failure!.Detail);
    }

    [Fact]
    public void CapabilityInputsAreCopiedAndDoNotEnableFutureUnknownInstructions()
    {
        var types = new[] { EmissionPrimitiveType.Int32 };
        var instructions = new[] { LinearInstructionKind.Return };
        var capabilities = new EmissionCapabilities(types, instructions);
        types[0] = EmissionPrimitiveType.String; instructions[0] = LinearInstructionKind.Divide;
        Assert.True(capabilities.Allows(EmissionPrimitiveType.Int32));
        Assert.True(capabilities.Allows(LinearInstructionKind.Return));
        Assert.False(capabilities.Allows(EmissionPrimitiveType.String));
        Assert.False(capabilities.Allows(LinearInstructionKind.Divide));
        Assert.False(capabilities.Allows((LinearInstructionKind)999));
    }

    private static Compilation Create(string source, OptimizationLevel optimization = OptimizationLevel.Release)
        => Compilation.Create("Capabilities" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
    private static IEnumerable<MethodDeclarationSyntax> Methods(Compilation compilation)
        => compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>();
    private static bool Lower(Compilation compilation, MethodDeclarationSyntax method, EmissionCapabilities capabilities,
        out LinearMethodBody? body, out LinearBodyFailure? failure)
    {
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        return LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!, _ => false,
            out body, out failure, capabilities);
    }
}
