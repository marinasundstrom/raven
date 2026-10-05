using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedLinearBodyTests
{
    [Fact]
    public void ValueBlockReturnRejectsTheSharedPlan()
    {
        var compilation = Create("""
            public static class Blocks {
                public static func Value(flag: bool) -> int {
                    let chosen = 2 + (if flag {
                        if flag { return 42 }
                        0
                    } else { 1 })
                    return chosen
                }
            }
            """, OptimizationLevel.Release);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        var syntax = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, out var plan, ReflectionEmitCapabilities.Shared));
        Assert.False(plan!.TryLowerBody(compilation, _ => false, out var body, out var failure, ReflectionEmitCapabilities.Shared));
        Assert.Null(body);
        Assert.Equal("value block cannot exit its enclosing expression", failure!.Detail);
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void MatchValueBlockCanReturnFromMethod(OptimizationLevel optimization)
    {
        var compilation = Create("""
            public static class Blocks {
                public static func Value(flag: bool) -> int {
                    return match flag {
                        true => {
                            if flag { return 42 }
                            0
                        }
                        false => 21
                    }
                }
            }
            """, optimization);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var method = Emit(compilation).GetType("Blocks")!.GetMethod("Value")!;
        Assert.Equal(42, method.Invoke(null, [true]));
        Assert.Equal(21, method.Invoke(null, [false]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ValueBlockReturnsAtStatementBoundaries(OptimizationLevel optimization)
    {
        var compilation = Create("""
            public static class Blocks {
                public static func Value(flag: bool, direct: bool) -> int {
                    if direct {
                        return if flag {
                            if flag { return 42 }
                            0
                        } else { 21 }
                    }
                    let chosen = if flag {
                        if flag { return 42 }
                        0
                    } else { 20 }
                    return chosen + 1
                }
            }
            """, optimization);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        var syntax = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, out var plan, ReflectionEmitCapabilities.Shared));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        var method = Emit(compilation).GetType("Blocks")!.GetMethod("Value")!;
        foreach (var direct in new[] { true, false })
        {
            Assert.Equal(42, method.Invoke(null, [true, direct]));
            Assert.Equal(21, method.Invoke(null, [false, direct]));
        }
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ValueBlockControlFlowKeepsEnclosingOperands(OptimizationLevel optimization)
    {
        const string source = """
            public static class Blocks {
                public static func Value(flag: bool) -> int {
                    return 2 + (if flag {
                        var value = 0
                        var index = 0
                        while index < 10 {
                            index = index + 1
                            if index == 2 { continue }
                            if index == 5 { break }
                            value = value + 10
                        }
                        if value == 30 { value = value + 10 } else { value = 0 }
                        value
                    } else {
                        var value = 20
                        if value > 0 { value = value - 1 }
                        value
                    })
                }
            }
            """;
        var compilation = Create(source, optimization);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        var syntax = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, out var plan, ReflectionEmitCapabilities.Shared));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail + ": " + failure?.Syntax);
        var method = Emit(compilation).GetType("Blocks")!.GetMethod("Value")!;
        Assert.Equal(42, method.Invoke(null, [true]));
        Assert.Equal(21, method.Invoke(null, [false]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ValueBlocksKeepBranchLocalsAndAssignments(OptimizationLevel optimization)
    {
        const string source = """
            public static class Blocks {
                public static func Value(flag: bool, input: int) -> int {
                    var outer = 1
                    let chosen = if flag {
                        var local = input
                        local = local + 1
                        outer = 2
                        Ignore(local)
                        local * 2
                    } else {
                        let local = input - 1
                        outer = 3
                        local
                    }
                    return chosen + outer
                }
                private static func Ignore(value: int) -> int => value
            }
            """;
        var compilation = Create(source, optimization);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        var syntax = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
        Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, out var plan, ReflectionEmitCapabilities.Shared));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        var method = Emit(compilation).GetType("Blocks")!.GetMethod("Value")!;
        Assert.Equal(42, method.Invoke(null, [true, 19]));
        Assert.Equal(21, method.Invoke(null, [false, 19]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ConditionalValuesSelectOneBranchAndPreserveTypes(OptimizationLevel optimization)
    {
        const string source = """
            public static class Choices {
                public static func Number(flag: bool, value: int) -> int {
                    let chosen = if flag { value + 1 } else { 100 / value }
                    return chosen * 2
                }
                public static func Wide(flag: bool) -> long => if flag { 5000000000L } else { -1L }
                public static func Flag(flag: bool) -> bool => if flag { false } else { true }
                public static func Text(flag: bool) -> string => if flag { "Hej 🌍" } else { "Other" }
                public static func Nested(flag: bool, inner: bool) -> int => if flag { (if inner { 42 } else { 7 }) } else { 3 }
            }
            """;
        var compilation = Create(source, optimization);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        foreach (var syntax in compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), plan.MetadataName + ": " + failure?.Detail);
        }
        var type = Emit(compilation).GetType("Choices")!;
        Assert.Equal(42, type.GetMethod("Number")!.Invoke(null, [true, 20]));
        Assert.Equal(2, type.GetMethod("Number")!.Invoke(null, [true, 0]));
        Assert.Equal(40, type.GetMethod("Number")!.Invoke(null, [false, 5]));
        foreach (var flag in new[] { false, true })
        {
            Assert.Equal(flag ? 5000000000L : -1L, type.GetMethod("Wide")!.Invoke(null, [flag]));
            Assert.Equal(!flag, type.GetMethod("Flag")!.Invoke(null, [flag]));
            Assert.Equal(flag ? "Hej 🌍" : "Other", type.GetMethod("Text")!.Invoke(null, [flag]));
        }
        Assert.Equal(42, type.GetMethod("Nested")!.Invoke(null, [true, true]));
        Assert.Equal(7, type.GetMethod("Nested")!.Invoke(null, [true, false]));
        Assert.Equal(3, type.GetMethod("Nested")!.Invoke(null, [false, true]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void BooleanBitOperatorsPreserveTruthTables(OptimizationLevel optimization)
    {
        const string source = """
            public static class Logic {
                public static func And(left: bool, right: bool) -> bool => left & right
                public static func Or(left: bool, right: bool) -> bool => left | right
                public static func Xor(left: bool, right: bool) -> bool => left ^ right
            }
            """;
        var compilation = Create(source, optimization);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        foreach (var syntax in compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        }
        var type = Emit(compilation).GetType("Logic")!;
        foreach (var left in new[] { false, true })
            foreach (var right in new[] { false, true })
            {
                Assert.Equal(left & right, type.GetMethod("And")!.Invoke(null, [left, right]));
                Assert.Equal(left | right, type.GetMethod("Or")!.Invoke(null, [left, right]));
                Assert.Equal(left ^ right, type.GetMethod("Xor")!.Invoke(null, [left, right]));
            }
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void StaticArithmeticAndCallsExecuteOnBothGeneratorPaths(OptimizationLevel optimization)
    {
        const string source = """
            public static class Arithmetic {
                public static func Value(value: int) -> int {
                    return Twice(value - 1) + 4
                }
                public static func Twice(value: int) -> int {
                    return value * 2
                }
            }
            """;
        var compilation = Create(source, optimization);
        var method = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!,
            model, method.Body!, _ => false, out var lowered, out var failure));
        Assert.NotNull(lowered);
        Assert.Null(failure);
        var assembly = Emit(compilation);
        Assert.Equal(42, assembly.GetType("Arithmetic")!.GetMethod("Value")!.Invoke(null, [20]));
        Assert.Equal(unchecked(int.MaxValue * 2), assembly.GetType("Arithmetic")!.GetMethod("Twice")!.Invoke(null, [int.MaxValue]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void BooleanSignaturesPreserveParametersResultsAndOverloads(OptimizationLevel optimization)
    {
        const string source = """
            public static class Predicates {
                public static func Identity(value: bool) -> bool {
                    value
                }
                public static func Identity(value: int) -> int {
                    value
                }
                public static func Positive(value: int) -> bool {
                    value > 0
                }
                public static func Choose(value: int, selected: bool) -> int {
                    if Identity(selected) {
                        return Identity(value)
                    }
                    return 0
                }
            }
            """;
        var compilation = Create(source, optimization);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        foreach (var declaration in compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(declaration)!,
                model, declaration.Body!, _ => false, out _, out var failure), failure?.Detail + ": " + failure?.Syntax);
        }
        var type = Emit(compilation).GetType("Predicates")!;
        Assert.Equal(true, type.GetMethod("Positive")!.Invoke(null, [1]));
        Assert.Equal(false, type.GetMethod("Identity", [typeof(bool)])!.Invoke(null, [false]));
        Assert.Equal(42, type.GetMethod("Choose")!.Invoke(null, [42, true]));
        Assert.Equal(0, type.GetMethod("Choose")!.Invoke(null, [42, false]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void BooleanLocalsRetainTypeAcrossAssignmentAndConditions(OptimizationLevel optimization)
    {
        const string source = """
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
            """;
        var compilation = Create(source, optimization);
        var declaration = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
        var model = compilation.GetSemanticModel(declaration.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(declaration)!,
            model, declaration.Body!, _ => false, out _, out var failure), failure?.Detail + ": " + failure?.Syntax);
        Assert.Equal(42, Emit(compilation).GetType("Selection")!.GetMethod("Main")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ShortCircuitConditionsAndAssignmentsSkipRightOperands(OptimizationLevel optimization)
    {
        const string source = """
            public static class Logic {
                public static func Main() -> int {
                    var selected = false && FailIfEvaluated()
                    selected = true || FailIfEvaluated()
                    if (false || selected) && (true || FailIfEvaluated()) {
                        return 42
                    }
                    return 0
                }
                public static func FailIfEvaluated() -> bool {
                    throw System.InvalidOperationException("short-circuited operand evaluated")
                }
            }
            """;
        var compilation = Create(source, optimization);
        var declaration = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
        var model = compilation.GetSemanticModel(declaration.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(declaration)!,
            model, declaration.Body!, _ => false, out _, out var failure), failure?.Detail);
        Assert.Equal(42, Emit(compilation).GetType("Logic")!.GetMethod("Main")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void StatementCallsDiscardPrimitiveResultsButNotUnit(OptimizationLevel optimization)
    {
        const string source = """
            public static class Calls {
                public static func Main() -> int {
                    Number()
                    Predicate()
                    Finish()
                    return 42
                }
                public static func Number() -> int { return 7 }
                public static func Predicate() -> bool { return true }
                public static func Finish() { }
            }
            """;
        var compilation = Create(source, optimization);
        var declaration = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
        var model = compilation.GetSemanticModel(declaration.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(declaration)!,
            model, declaration.Body!, _ => false, out _, out var failure), failure?.Detail);
        Assert.Equal(42, Emit(compilation).GetType("Calls")!.GetMethod("Main")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void Int64LocalsAndSignedConversionsPreserveBoundaryValues(OptimizationLevel optimization)
    {
        const string source = """
            public static class Wide {
                public static func Main() -> int {
                    let value: long = Widen(42)
                    let high = 4294967296L
                    let total = value + high
                    if Widen(0 - 1) < 0L {
                        return Narrow(total)
                    }
                    return 0
                }
                public static func Widen(value: int) -> long { return value }
                public static func Narrow(value: long) -> int { return (int)value }
            }
            """;
        var compilation = Create(source, optimization);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        foreach (var declaration in compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
            Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(declaration)!,
                model, declaration.Body!, _ => false, out _, out var failure), failure?.Detail);
        var type = Emit(compilation).GetType("Wide")!;
        Assert.Equal(42, type.GetMethod("Main")!.Invoke(null, null));
        foreach (var value in new[] { int.MinValue, -1, 0, int.MaxValue })
            Assert.Equal((long)value, type.GetMethod("Widen")!.Invoke(null, [value]));
        foreach (var value in new[] { long.MinValue, -1L, 4294967338L, long.MaxValue })
            Assert.Equal(unchecked((int)value), type.GetMethod("Narrow")!.Invoke(null, [value]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void UnaryIntegersPreserveWidthAndWrapNegation(OptimizationLevel optimization)
    {
        const string source = """
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
            """;
        var compilation = Create(source, optimization);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        foreach (var declaration in compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
            Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(declaration)!,
                model, declaration.Body!, _ => false, out _, out var failure), failure?.Detail);
        var type = Emit(compilation).GetType("Unary")!;
        Assert.Equal(42, type.GetMethod("Main")!.Invoke(null, null));
        foreach (var value in new[] { int.MinValue, -1, 0, int.MaxValue })
        {
            Assert.Equal(unchecked(-value), type.GetMethod("Negate32")!.Invoke(null, [value]));
            Assert.Equal(~value, type.GetMethod("Complement32")!.Invoke(null, [value]));
            Assert.Equal(value, type.GetMethod("Positive")!.Invoke(null, [value]));
        }
        foreach (var value in new[] { long.MinValue, -1L, 0L, long.MaxValue })
        {
            Assert.Equal(unchecked(-value), type.GetMethod("Negate64")!.Invoke(null, [value]));
            Assert.Equal(~value, type.GetMethod("Complement64")!.Invoke(null, [value]));
        }
    }

    [Fact]
    public void NumericConversionsAreSupportedBySharedAndDotNetGenerators()
    {
        const string source = """
            public static class Arithmetic {
                public static func Convert(value: int) -> int {
                    return (int)(double)value
                }
            }
            """;
        var compilation = Create(source, OptimizationLevel.Release);
        var method = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!,
            model, method.Body!, _ => false, out var lowered, out var failure));
        Assert.NotNull(lowered);
        Assert.Null(failure);
        Assert.Equal(42, Emit(compilation).GetType("Arithmetic")!.GetMethod("Convert")!.Invoke(null, [42]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void UnitFunctionsAndClassHelpersKeepVoidSignatures(OptimizationLevel optimization)
    {
        const string source = """
            func Main() {
                Greet()
            }
            func Greet() {
                Helpers.Finish(value: 42)
                return
            }
            public static class Helpers {
                public static func Finish(value: int) {
                }
            }
            """;
        var compilation = Compilation.Create("SharedUnit" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        foreach (var declaration in compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            var symbol = (IMethodSymbol)model.GetDeclaredSymbol(declaration)!;
            Assert.True(LinearMethodBody.TryLower(symbol, model, declaration.Body!,
                _ => false, out _, out var failure), failure?.Detail);
        }
        var assembly = Emit(compilation);
        Assert.Equal(typeof(void), assembly.EntryPoint!.ReturnType);
        Assert.Null(assembly.EntryPoint.Invoke(null, null));
        Assert.Equal(typeof(void), assembly.GetType("Helpers")!.GetMethod("Finish")!.ReturnType);
    }

    [Fact]
    public void Int32AssemblyFunctionsCallHelpersThroughTheSharedPath()
    {
        const string source = """
            func Main() -> int {
                return Twice(20) + 2
            }
            func Twice(value: int) -> int {
                return value * 2
            }
            """;
        var compilation = Compilation.Create("SharedFunctions", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ImplicitValueReturnUsesCompilerLowering(OptimizationLevel optimization)
    {
        const string source = """
            public static class Arithmetic {
                public static func Value(value: int) -> int {
                    (value + 1) * 2
                }
            }
            """;
        var compilation = Create(source, optimization);
        var method = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!,
            model, method.Body!, _ => false, out _, out var failure), failure?.Detail);
        Assert.Equal(42, Emit(compilation).GetType("Arithmetic")!.GetMethod("Value")!.Invoke(null, [20]));
    }

    [Fact]
    public void ForwardCallsKeepOverloadsAndOwnersDistinctAcrossEmissions()
    {
        const string source = """
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
            """;
        var compilation = Compilation.Create("CallableIdentities", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void InitializedLocalsAndAssignmentExecute(OptimizationLevel optimization)
    {
        const string source = """
            public static class Arithmetic {
                public static func Value(value: int) -> int {
                    let start = value
                    var result = start * 2
                    result = result + 2
                    result
                }
            }
            """;
        var compilation = Create(source, optimization);
        var method = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure), failure?.Detail);
        Assert.Equal(42, Emit(compilation).GetType("Arithmetic")!.GetMethod("Value")!.Invoke(null, [20]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void BranchesAndLoopUseLoweredControlFlow(OptimizationLevel optimization)
    {
        const string source = """
            func Main() -> int {
                Accumulate(6)
            }
            func Accumulate(limit: int) -> int {
                var index = 0
                var result = 0
                while index < limit {
                    if index < 3 {
                        result = result + 5
                    } else {
                        result = result + 9
                    }
                    index = index + 1
                }
                return result
            }
            """;
        var compilation = Compilation.Create("Flow" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, model, syntax.Body!,
                _ => false, out _, out var failure), failure?.Detail);
        }
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void NegatedComparisonsAndLoopExitsExecute(OptimizationLevel optimization)
    {
        const string source = """
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
            """;
        var compilation = Compilation.Create("LoopExits" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        var tree = compilation.SyntaxTrees[0]; var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, model, syntax.Body!,
            _ => false, out _, out var failure), failure?.Detail);
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void StringCallsLocalsAndBranchesPreserveUnicode(OptimizationLevel optimization)
    {
        const string source = """
            public static class Text {
                public static func Echo(value: string) -> string {
                    var result = ""
                    result = value
                    return result
                }
                public static func Choose(selected: bool) -> string {
                    if selected {
                        return Echo("Hej 🌍 café")
                    }
                    return Echo("")
                }
            }
            """;
        var compilation = Create(source, optimization);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        foreach (var declaration in compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
            Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(declaration)!, model, declaration.Body!,
                _ => false, out _, out var failure), failure?.Detail);
        var choose = Emit(compilation).GetType("Text")!.GetMethod("Choose")!;
        Assert.Equal("Hej 🌍 café", choose.Invoke(null, [true]));
        Assert.Equal("", choose.Invoke(null, [false]));
    }

    [Theory]
    [InlineData("int", "42", "let", true)]
    [InlineData("long", "4294967296", "let", true)]
    [InlineData("bool", "true", "let", true)]
    [InlineData("byte", "42", "let", true)]
    [InlineData("int", "42", "var", false)]
    public void PrimitiveCaptureAdmissionPreservesOrdinaryExecution(string type, string value, string binding, bool admitted)
    {
        var source = $$"""
            public static class Capture {
                public static func Run() -> int {
                    {{binding}} captured: {{type}} = {{value}}
                    let callback: () -> {{type}} = () => captured
                    if callback() == {{value}} {
                        return 42
                    }
                    return 1
                }
            }
            """;
        var compilation = Create(source, OptimizationLevel.Release);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>(), allowsFunctionValues: true);
        var success = LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure, capabilities);
        Assert.Equal(admitted, success);
        if (!admitted)
            Assert.Equal("closure capture requires an immutable reference or supported primitive local", failure!.Detail);
        Assert.Equal(42, Emit(compilation).GetType("Capture")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void PromotedIntegerOperandsPreserveOrdinaryExecution()
    {
        var compilation = Create("""
            public static class Numbers {
                public static func Run() -> long {
                    let wide: long = 4294967296
                    let small: byte = 42
                    let signed = -1
                    if small + wide != 4294967338 { return 1 }
                    if wide + signed != 4294967295 { return 2 }
                    if signed < wide { return wide + small }
                    return 3
                }
            }
            """, OptimizationLevel.Release);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>());
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure, capabilities), failure?.Detail);
        Assert.Equal(4294967338L, Emit(compilation).GetType("Numbers")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ObjectHashDispatchRequiresExplicitCapability(bool enabled)
    {
        var compilation = Create("""
            public static class Hashes {
                public static func Hash(value: object) -> int { return value.GetHashCode() }
            }
            """, OptimizationLevel.Release);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true, allowsExternalInstanceCalls: true, allowsObjectHashDispatch: enabled);
        var success = LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, model, syntax.Body!,
            _ => false, out _, out var failure, capabilities);
        Assert.True(success == enabled, failure?.Detail);

        var input = new object();
        Assert.Equal(input.GetHashCode(), Emit(compilation).GetType("Hashes")!.GetMethod("Hash")!.Invoke(null, [input]));
    }

    [Fact]
    public void BoundStringEqualityOperatorPreservesContentSemantics()
    {
        var compilation = Create("""
            public static class TextEquality {
                public static func Equal(left: string, right: string) -> bool {
                    return left == right
                }
            }
            """, OptimizationLevel.Release);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>());
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure, capabilities), failure?.Detail);
        var equal = Emit(compilation).GetType("TextEquality")!.GetMethod("Equal")!;
        Assert.Equal(true, equal.Invoke(null, ["café", new string("café".ToCharArray())]));
        Assert.Equal(false, equal.Invoke(null, ["A", "a"]));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ValueResultReceiversRequireManagedStorage(bool managedStorage)
    {
        var compilation = Create("""
            public static class ValueResults {
                public static func Compare(text: string) -> int {
                    return text.Length.CompareTo(4)
                }
            }
            """, OptimizationLevel.Release);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsExternalReferenceSignatures: true, allowsExternalInstanceCalls: true,
            declarations: [EmissionDeclarationKind.PropertyAccessor],
            allowsExternalValueSignatures: true, allowsExternalValueInstanceCalls: true, allowsManagedReferences: managedStorage);
        var success = LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure, capabilities);
        Assert.True(success == managedStorage, failure?.Detail);
        var compare = Emit(compilation).GetType("ValueResults")!.GetMethod("Compare")!;
        Assert.Equal(0, compare.Invoke(null, ["four"]));
        Assert.Equal(-1, compare.Invoke(null, ["one"]));
        Assert.Equal(1, compare.Invoke(null, ["longer"]));
    }

    [Fact]
    public void ValueGetterReceiverMutationDoesNotWriteBackOnDotNet()
    {
        var compilation = Create("""
            struct Counter {
                var Value: int = 0
                func Bump() -> int {
                    Value += 1
                    return Value
                }
            }
            class Copies {
                field Stored: Counter = default(Counter)
                var Reads: int = 0
                val Copy: Counter {
                    get {
                        Reads += 1
                        return Stored
                    }
                }
            }
            public static class CopyTest {
                public static func Run() -> int {
                    let copies = Copies()
                    if copies.Copy.Bump() != 1 || copies.Reads != 1 || copies.Stored.Value != 0 {
                        return 1
                    }
                    if copies.Stored.Bump() != 1 || copies.Stored.Value != 1 {
                        return 2
                    }
                    return 42
                }
            }
            """, OptimizationLevel.Release);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Equal(42, Emit(compilation).GetType("CopyTest")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ExplicitDiscardsEvaluateValuesAndNoResultCalls(OptimizationLevel optimization)
    {
        var compilation = Create("""
            public static class Discards {
                public static func Main() -> int {
                    _ = Number()
                    _ = Predicate()
                    _ = Finish()
                    _ = ()
                    _ = "discarded"
                    return 42
                }
                public static func Number() -> int { return 7 }
                public static func Predicate() -> bool { return true }
                public static func Finish() { }
            }
            """, optimization);
        var declaration = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
        var model = compilation.GetSemanticModel(declaration.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(declaration)!,
            model, declaration.Body!, _ => false, out _, out var failure), failure?.Detail);
        Assert.Equal(42, Emit(compilation).GetType("Discards")!.GetMethod("Main")!.Invoke(null, null));
    }

    private static Compilation Create(string source, OptimizationLevel optimization)
        => Compilation.Create("SharedBody" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));

    private static Assembly Emit(Compilation compilation)
    {
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        return Assembly.Load(output.ToArray());
    }
}
