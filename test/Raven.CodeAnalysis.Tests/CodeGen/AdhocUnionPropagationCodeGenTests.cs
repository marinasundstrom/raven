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
