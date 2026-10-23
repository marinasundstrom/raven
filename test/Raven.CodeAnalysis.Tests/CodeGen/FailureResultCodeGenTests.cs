using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class FailureResultCodeGenTests
{
    private const string Contracts = """
        namespace System {
            public interface Propagatable<S, T, E> {
                func TryGetOutput(out value: T) -> bool
                func TryGetResidual(out error: E) -> bool
            }
            public union Result<T, E>: Propagatable<Result<T, E>, T, E> {
                case Ok(T)
                case Failure(E)
                func TryGetOutput(out value: T) -> bool {
                    value = default
                    if self is Ok(let output) {
                        value = output
                        return true
                    }
                    return false
                }
                func TryGetResidual(out error: E) -> bool {
                    error = default
                    if self is Failure(let failure) {
                        error = failure
                        return true
                    }
                    return false
                }
                static func FromResidual(error: E) -> Result<T, E> => Failure(error)
            }
        }
        """;

    [Fact]
    public void FailureSupportsPropagationCarrierAccessAndTryExpressions()
    {
        const string source = """
            import System.*
            class Runner {
                static func Forward(input: Result<int, string>) -> Result<int, string> {
                    let value = input?
                    return .Ok(value + 1)
                }
                static func Run(fail: bool) -> int {
                    let input: Result<string, string> = if fail { .Failure("failure") } else { .Ok("hello") }
                    let lengths = input?.Length
                    let forwarded = Forward(lengths)
                    return forwarded match {
                        .Ok(let value) => value
                        .Failure(let error) => -error.Length
                    }
                }
                static func TryParse(text: string) -> bool {
                    let result = try Convert.ToInt32(text)
                    return result is .Failure(_)
                }
            }
            """;
        var references = TestMetadataReferences.Default;
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimePropagationContract(new RuntimePropagationContract("FailureCases", "System.Propagatable`3"));
        var compilation = Compilation.Create("FailureCases",
            [SyntaxTree.ParseText(Contracts), SyntaxTree.ParseText(source)], references, options);
        using var pe = new MemoryStream();
        var emitted = compilation.Emit(pe);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        var runner = loaded.Assembly.GetType("Runner")!;
        var run = runner.GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(6, run.Invoke(null, [false]));
        Assert.Equal(-7, run.Invoke(null, [true]));
        var parse = runner.GetMethod("TryParse", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(false, parse.Invoke(null, ["42"]));
        Assert.Equal(true, parse.Invoke(null, ["invalid"]));
    }

    [Fact]
    public void InferredEntryResultRecognizesFailure()
    {
        var compilation = Compilation.Create("FailureEntry",
            [SyntaxTree.ParseText("import System.*\nreturn .Failure(\"failure\")")],
            TestMetadataReferences.DefaultWithRavenCore, new CompilationOptions(OutputKind.ConsoleApplication));
        using var pe = new MemoryStream();
        var emitted = compilation.Emit(pe);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
    }
}
