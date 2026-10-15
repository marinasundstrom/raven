using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class AdhocUnionPropagationCodeGenTests
{
    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void ResultPropagation_InjectsEitherErrorAlternative(int failure)
    {
        const string code = """
            import System.*

            class IOError { }
            class ParseError { }
            class Harness {
                private static var steps: int = 0
                private static func ReadFile(failure: int) -> Result<string, IOError> {
                    steps = steps + 1
                    if failure == 1 { return .Error(IOError()) }
                    return .Ok("config")
                }
                private static func ParseConfig(text: string, failure: int) -> Result<int, ParseError> {
                    steps = steps + 1
                    if failure == 2 { return .Error(ParseError()) }
                    return .Ok(42)
                }
                private static func LoadConfig(failure: int) -> Result<int, IOError | ParseError> {
                    let text = ReadFile(failure)?
                    let config = ParseConfig(text, failure)?
                    steps = steps + 1
                    return .Ok(config)
                }
                public static func Check(failure: int) -> bool {
                    let result = LoadConfig(failure)
                    if result is .Error(let error) {
                        if failure == 1 { return steps == 1 && error is IOError }
                        return failure == 2 && steps == 2 && error is ParseError
                    }
                    return failure == 0 && steps == 3 && result is .Ok(42)
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("adhoc-union-propagation", [SyntaxTree.ParseText(code)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        Assert.Equal(true, loaded.Assembly.GetType("Harness")!.GetMethod("Check")!.Invoke(null, [failure]));
    }

    [Theory]
    [InlineData("IOError")]
    [InlineData("ParseError")]
    public void InlineUnionTypeArguments_ConstructAndInvoke(string errorType)
    {
        const string code = """
            import System.*
            class IOError { }
            class ParseError { }
            class Box<T> {
                val Value: T
                init(value: T) { Value = value }
            }
            class Harness {
                private static func Identity<T>(value: T) -> T => value
                public static func Check() -> bool {
                    let direct = Box<IOError | ParseError>(SelectedError())
                    let nested = Box<Result<int, IOError | ParseError>>(.Error(SelectedError()))
                    let invoked = Identity<IOError | ParseError>(SelectedError())
                    if nested.Value is .Error(let actual) {
                        return direct.Value is SelectedError && invoked is SelectedError && actual is SelectedError
                    }
                    return false
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("inline-union-type-arguments", [SyntaxTree.ParseText(code.Replace("SelectedError", errorType, StringComparison.Ordinal))], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        Assert.Equal(true, loaded.Assembly.GetType("Harness")!.GetMethod("Check")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void Widening_PreservesPayloadDefaultAndSingleEvaluation(int selected)
    {
        const string code = """
            import System.*
            class Cell {
                val Value: int = 42
            }
            class Harness {
                private static var calls: int = 0
                private static val cell = Cell()
                private static func Read(selected: int) -> int | Cell {
                    calls = calls + 1
                    if selected == 0 { return 42 }
                    return cell
                }
                private static func Widen<T>(value: int | T) -> bool | T | int {
                    return value
                }
                public static func EmptyCheck(value: int | Cell) -> bool {
                    let widened: bool | Cell | int = value
                    return !widened.HasValue
                }
                public static func Check(selected: int) -> bool {
                    let widened = Widen<Cell>(Read(selected))
                    let reordered: int | bool | Cell = widened
                    if calls != 1 { return false }
                    if selected == 0 { return reordered is 42 }
                    if selected == 1 {
                        if reordered is let actual: Cell {
                            return Object.ReferenceEquals(actual, cell) && actual.Value == 42
                        }
                        return false
                    }
                    return !reordered.HasValue
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("union-widening", [SyntaxTree.ParseText(code)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        var harness = loaded.Assembly.GetType("Harness")!;
        if (selected == 2)
        {
            // A .NET caller can supply an inactive default carrier even though
            // Raven diagnoses returning a known inactive carrier in source.
            var method = harness.GetMethod("EmptyCheck")!;
            Assert.Equal(true, method.Invoke(null, [Activator.CreateInstance(method.GetParameters()[0].ParameterType)]));
        }
        else
        {
            Assert.Equal(true, harness.GetMethod("Check")!.Invoke(null, [selected]));
        }
    }

    [Fact]
    public void ResultPropagation_RejectsErrorOutsideAlternatives()
    {
        const string code = """
            import System.*
            class IOError { }
            class ParseError { }
            class OtherError { }
            class Harness {
                static func Read() -> Result<int, OtherError> => .Error(OtherError())
                static func Load() -> Result<int, IOError | ParseError> {
                    let value = Read()?
                    return .Ok(value)
                }
            }
            """;
        var compilation = Compilation.Create("unrelated-error-propagation", [SyntaxTree.ParseText(code)],
            TestMetadataReferences.DefaultWithRavenCore, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1503" &&
            diagnostic.GetMessage().Contains("OtherError", StringComparison.Ordinal));
    }

}
