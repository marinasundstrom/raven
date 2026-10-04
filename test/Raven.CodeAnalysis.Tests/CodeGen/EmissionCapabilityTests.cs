using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class EmissionCapabilityTests
{
    [Fact]
    public void ClosedFamilyAdmissionIsExplicitAndOrdinaryDotNetStillExecutes()
    {
        var compilation = Create("""
            public sealed class Root {
                public field Number: int
                protected init(number: int) { Number = number }
            }
            public class Child : Root {
                public init(): base(42) {}
                public func Read() -> int => Number
            }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var root = compilation.GetTypeByMetadataName("Root")!;
        Assert.False(SourceTypePlan.TryCreate(root, out _, ReflectionEmitCapabilities.Shared));
        var enabled = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), [],
            [EmissionDeclarationKind.RootClass, EmissionDeclarationKind.Constructor],
            [Accessibility.Public], [Accessibility.Public], [],
            allowsRootClassSignatures: true, allowsLocalClassInheritance: true,
            allowsClosedClassFamilies: true, allowsProtectedConstructors: true);
        Assert.True(SourceTypePlan.TryCreate(root, out var plan, enabled));
        Assert.True(plan!.IsClosedHierarchy);
        Assert.True(SourceCallablePlan.TryCreate(root.InstanceConstructors.Single(), out _, enabled));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var assembly = Assembly.Load(output.ToArray());
        var emittedRoot = assembly.GetType("Root")!;
        Assert.True(emittedRoot.IsAbstract);
        Assert.False(emittedRoot.IsSealed);
        Assert.True(emittedRoot.GetConstructors(BindingFlags.NonPublic | BindingFlags.Instance).Single().IsFamily);
        var child = assembly.GetType("Child")!;
        Assert.Equal(42, child.GetMethod("Read")!.Invoke(Activator.CreateInstance(child), null));
    }

    [Theory]
    [InlineData(false, 42)]
    [InlineData(true, 17)]
    public void MatchInitializerEarlyReturnPreservesMethodControlFlow(bool present, int expected)
    {
        var compilation = Create("""
            public union Choice {
                case Item(value: int)
                case Missing
            }
            public static class Choices {
                public static func Run(present: bool) -> int {
                    let choice: Choice = if present { .Item(16) } else { .Missing }
                    let value = match choice {
                        .Item(let item) => item
                        .Missing => return 42
                    }
                    return value + 1
                }
            }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var type = Assembly.Load(output.ToArray()).GetType("Choices")!;
        Assert.Equal(expected, type.GetMethod("Run")!.Invoke(null, [present]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Debug)]
    [InlineData(OptimizationLevel.Release)]
    public void FieldAssignmentEvaluatesReceiverOnceBeforeBranchingValue(OptimizationLevel optimization)
    {
        var compilation = Create("""
            public class Holder {
                public field Value: int = 0
                private var trace: int = 0
                public init() {}
                public func Next() -> Holder {
                    trace = trace * 10 + 1
                    return self
                }
                public func Read() -> int {
                    trace = trace * 10 + 2
                    return 40
                }
                public func Run() -> int {
                    Next().Value = if Read() == 40 { 42 } else { 0 }
                    if trace != 12 { return -1 }
                    return Value
                }
            }
            """, optimization);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        foreach (var method in Methods(compilation))
            Assert.True(Lower(compilation, method, ReflectionEmitCapabilities.Shared, out _, out var failure), method.Identifier.Text + ": " + failure?.Detail);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var type = Assembly.Load(output.ToArray()).GetType("Holder")!;
        Assert.Equal(42, type.GetMethod("Run")!.Invoke(Activator.CreateInstance(type), null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Debug)]
    [InlineData(OptimizationLevel.Release)]
    public void RefAndOutUseSharedEmissionAndPreserveMutation(OptimizationLevel optimization)
    {
        var compilation = Create("""
            public static class RefOperations {
                public static func Set(out value: int) { value = 40 }
                public static func Forward(out value: int) { Set(out value) }
                public static func Increment(ref value: int) { value = value + 2 }
                public static func Run() -> int {
                    Forward(out var value)
                    Increment(ref value)
                    return value
                }
            }
            """, optimization);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        foreach (var method in Methods(compilation))
            Assert.True(Lower(compilation, method, ReflectionEmitCapabilities.Shared, out _, out var failure), failure?.Detail);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var type = Assembly.Load(output.ToArray()).GetType("RefOperations")!;
        Assert.Equal(42, type.GetMethod("Run")!.Invoke(null, null));
        Assert.True(type.GetMethod("Set")!.GetParameters()[0].IsOut);
    }

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

    [Theory]
    [InlineData(OptimizationLevel.Release, "<<", -168)]
    [InlineData(OptimizationLevel.Debug, "<<", -168)]
    [InlineData(OptimizationLevel.Release, ">>", -42)]
    [InlineData(OptimizationLevel.Debug, ">>", -42)]
    public void SignedShiftsPreserveWidthAndInt32Counts(OptimizationLevel optimization, string operation, int expected)
    {
        var compilation = Create($$"""
            public static class Shifts {
                public static func Narrow(value: int, count: int) -> int { value {{operation}} count }
                public static func Wide(value: long, count: int) -> long { value {{operation}} count }
            }
            """, optimization);
        foreach (var method in Methods(compilation))
            Assert.True(Lower(compilation, method, ReflectionEmitCapabilities.Shared, out _, out var failure), failure?.Detail);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var type = Assembly.Load(output.ToArray()).GetType("Shifts")!;
        Assert.Equal(expected, type.GetMethod("Narrow")!.Invoke(null, [-84, 1]));
        Assert.Equal((long)expected, type.GetMethod("Wide")!.Invoke(null, [-84L, 1]));
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
