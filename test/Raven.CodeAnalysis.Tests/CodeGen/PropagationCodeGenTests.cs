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
