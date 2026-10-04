using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class PropagationCodeGenTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void BinaryPropagationPreservesEvaluationAndEarlyReturn(bool fail)
    {
        const string code = """
            import System.*
            class Harness {
                private static var count: int = 0
                private static func Left() -> int {
                    count = count * 10 + 1
                    return 10
                }
                private static func Read(fail: bool) -> Result<int, string> {
                    count = count * 10 + 2
                    if fail { return .Error("failed") }
                    return .Ok(7)
                }
                private static func Right() -> int {
                    count = count * 10 + 3
                    return 2
                }
                private static func Run(fail: bool) -> Result<int, string> {
                    let value = Left() + Read(fail)? * Right()
                    count = count * 10 + 4
                    return .Ok(value)
                }
                public static func Check(fail: bool) -> bool {
                    let result = Run(fail)
                    if fail { return count == 12 && result is .Error("failed") }
                    return count == 1234 && result is .Ok(24)
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("binary-propagation", [SyntaxTree.ParseText(code)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        Assert.Equal(true, loaded.Assembly.GetType("Harness")!.GetMethod("Check")!.Invoke(null, [fail]));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void LocalAssignmentPropagationEvaluatesOnceAndReturnsBeforeFollowingMutation(bool fail)
    {
        const string code = """
            import System.*
            class Harness {
                private static var count: int = 0
                private static func Read(fail: bool) -> Result<int, string> {
                    count += 1
                    if fail { return .Error("failed") }
                    return .Ok(7)
                }
                private static func Run(fail: bool) -> Result<int, string> {
                    var value = 3
                    value = value + Read(fail)?
                    count += 10
                    return .Ok(value)
                }
                public static func Check(fail: bool) -> bool {
                    let result = Run(fail)
                    if fail { return count == 1 && result is .Error("failed") }
                    return count == 11 && result is .Ok(10)
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("assignment-propagation", [SyntaxTree.ParseText(code)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        Assert.Equal(true, loaded.Assembly.GetType("Harness")!.GetMethod("Check")!.Invoke(null, [fail]));
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void ConditionalPropagationPreservesSelectedBranchAndEarlyReturn(bool chooseFirst, bool fail)
    {
        const string code = """
            import System.*
            class Harness {
                private static var count: int = 0
                private static func Read(fail: bool, value: int) -> Result<int, string> {
                    count = count * 10 + value
                    if fail { return .Error("failed") }
                    return .Ok(value)
                }
                private static func Run(chooseFirst: bool, fail: bool) -> Result<int, string> {
                    let value = 10 + if chooseFirst {
                        count = 1
                        Read(fail, 2)?
                    } else {
                        count = 3
                        Read(fail, 4)?
                    }
                    count = count * 10 + 5
                    return .Ok(value)
                }
                public static func Check(chooseFirst: bool, fail: bool) -> bool {
                    let result = Run(chooseFirst, fail)
                    if fail {
                        let expected = if chooseFirst { 12 } else { 34 }
                        return count == expected && result is .Error("failed")
                    }
                    if chooseFirst { return count == 125 && result is .Ok(12) }
                    return count == 345 && result is .Ok(14)
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("conditional-propagation", [SyntaxTree.ParseText(code)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        Assert.Equal(true, loaded.Assembly.GetType("Harness")!.GetMethod("Check")!.Invoke(null, [chooseFirst, fail]));
    }

    [Fact]
    public void DiscardedPropagationEvaluatesOnceAndStopsOnError()
    {
        const string code = """
            import System.*
            class Harness {
                private static var count: int = 0
                private static func Read(fail: bool) -> Result<int, string> {
                    count += 1
                    if fail { return .Error("failed") }
                    return .Ok(7)
                }
                private static func Run(fail: bool) -> Result<int, string> {
                    _ = Read(fail)?
                    count += 10
                    return .Ok(42)
                }
                public static func Check() -> bool {
                    let ok = Run(false)
                    if count != 11 || !(ok is .Ok(42)) { return false }
                    let error = Run(true)
                    return count == 12 && error is .Error("failed")
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("discard-propagation", [SyntaxTree.ParseText(code)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        Assert.Equal(true, loaded.Assembly.GetType("Harness")!.GetMethod("Check")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void CustomCarrier_PropagationUsesContractForEarlyReturn(bool validCarrier)
    {
        const string code = """
import System.*

record struct IntAttempt(Value: int, Error: string?, IsSuccess: bool)
    : System.IPropagatable<IntAttempt, int, string> {
    static func Success(value: int) -> IntAttempt => IntAttempt(value, null, true)
    static func Failure(error: string) -> IntAttempt => IntAttempt(default, error, false)

    func TryGetOutput(out output: int) -> bool {
        output = Value
        return IsSuccess
    }

    func TryGetResidual(out residual: string) -> bool {
        residual = ""
        if Error is string error {
            residual = error
        }
        return !IsSuccess
    }

    static func FromResidual(residual: string) -> IntAttempt => Failure(residual)
}

class Harness {
    private static func Failure() -> IntAttempt => IntAttempt.Failure("stopped")

    private static func Propagate() -> IntAttempt {
        let value = Failure()?
        return IntAttempt.Success(value + 1)
    }

    public static func Check() -> bool {
        let result = Propagate()
        return !result.IsSuccess && result.Error == "stopped"
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(validCarrier ? code : code.Replace("return !IsSuccess", "return false"));
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create(
            "custom-carrier-propagation",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var harnessType = loaded.Assembly.GetType("Harness", throwOnError: true)!;
        var check = harnessType.GetMethod("Check", BindingFlags.Public | BindingFlags.Static)!;

        if (validCarrier)
            Assert.Equal(true, check.Invoke(null, null));
        else
            Assert.IsType<NullReferenceException>(Assert.Throws<TargetInvocationException>(() => check.Invoke(null, null)).InnerException);
    }

    [Fact]
    public void CustomCarrier_QuestionDotPropagatesBeforeMemberAccess()
    {
        const string code = """
import System.*

record struct StringAttempt(Value: string?, Error: string?, IsSuccess: bool)
    : System.IPropagatable<StringAttempt, string, string> {
    static func Success(value: string) -> StringAttempt => StringAttempt(value, null, true)
    static func Failure(error: string) -> StringAttempt => StringAttempt(null, error, false)

    func TryGetOutput(out output: string) -> bool {
        output = ""
        if Value is string value {
            output = value
        }
        return IsSuccess
    }

    func TryGetResidual(out residual: string) -> bool {
        residual = ""
        if Error is string error {
            residual = error
        }
        return !IsSuccess
    }

    static func FromResidual(residual: string) -> StringAttempt => Failure(residual)
}

record struct IntAttempt2(Value: int, Error: string?, IsSuccess: bool)
    : System.IPropagatable<IntAttempt2, int, string> {
    static func Success(value: int) -> IntAttempt2 => IntAttempt2(value, null, true)
    static func Failure(error: string) -> IntAttempt2 => IntAttempt2(default, error, false)

    func TryGetOutput(out output: int) -> bool {
        output = Value
        return IsSuccess
    }

    func TryGetResidual(out residual: string) -> bool {
        residual = ""
        if Error is string error {
            residual = error
        }
        return !IsSuccess
    }

    static func FromResidual(residual: string) -> IntAttempt2 => Failure(residual)
}

class Harness {
    private static func Length(value: StringAttempt) -> IntAttempt2 {
        let length = value?.Length
        return IntAttempt2.Success(length)
    }

    public static func CheckSuccess() -> bool {
        let result = Length(StringAttempt.Success("raven"))
        return result.IsSuccess && result.Value == 5
    }

    public static func CheckFailure() -> bool {
        let result = Length(StringAttempt.Failure("stopped"))
        return !result.IsSuccess && result.Error == "stopped"
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create(
            "custom-carrier-question-dot-propagation",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var harnessType = loaded.Assembly.GetType("Harness", throwOnError: true)!;

        Assert.Equal(true, harnessType.GetMethod("CheckSuccess", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
        Assert.Equal(true, harnessType.GetMethod("CheckFailure", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
    }

    [Fact]
    public void InterfaceConformingGenericStructUnion_PropagationMaterializesEmptyCaseCarrier()
    {
        const string code = """
interface IOptional {}

union Option<T>: IOptional {
    case Some(value: T)
    case None
}

class Harness {
    private static func NoneValue() -> Option<int> {
        return .None
    }

    private static func PropagateNone() -> Option<int> {
        let value = NoneValue()?
        return .Some(value)
    }

    public static func Check() -> bool {
        return PropagateNone() is .None
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(
            "struct-union-propagation",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var harnessType = runtimeAssembly.GetType("Harness", throwOnError: true)!;
        var check = harnessType.GetMethod("Check", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(true, check.Invoke(null, null));
    }
}
