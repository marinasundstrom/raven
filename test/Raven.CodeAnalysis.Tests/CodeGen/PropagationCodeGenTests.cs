using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class PropagationCodeGenTests
{
    [Fact]
    public void CustomCarrier_PropagationUsesContractForEarlyReturn()
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

        var syntaxTree = SyntaxTree.ParseText(code);
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

        Assert.Equal(true, check.Invoke(null, null));
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
