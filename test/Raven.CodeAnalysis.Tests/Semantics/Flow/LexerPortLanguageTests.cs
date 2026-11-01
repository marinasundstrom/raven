using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class LexerPortLanguageTests
{
    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("NestedRecordStruct", """
        class Outer {
            private field state: Frame
            private record struct Frame(Active: bool) {}
        }
        """)]
    [InlineData("NestedRecordClass", """
        class Outer {
            private field state: Snapshot? = null
            private record Snapshot(Active: bool) {}
        }
        """)]
    [InlineData("KeywordStringPatterns", """
        class Probe {
            private static func IsConditionalDirectiveKeyword(keyword: string) -> bool => keyword is "if" or "elif" or "else" or "endif"
            private func Following() -> bool => true
        }
        """)]
    [InlineData("RangeAccessIfValue", """
        class Probe {
            static func Trim(text: string, trim: bool) -> string => if trim { text[..^1] } else { text }
        }
        """)]
    [InlineData("UnsignedConstantOperands", """
        class Probe {
            static func Divide(value: ulong) -> ulong => value / 2
            static func Fits(value: ulong) -> bool => value <= int.MaxValue
        }
        """)]
    [InlineData("LeftOperandOutAssignment", """
        class Probe {
            private static func Read(out value: string) -> bool { value = "value"; return true }
            static func HasValue(out value: string) -> bool => Read(out value) && value.Length > 0
        }
        """)]
    public void DirectTranslationPreservesSharedSemantics(string scenario, string source)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(scenario, [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void TranslatedExpressionsPreserveRuntimeValues()
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                private record struct Frame(Value: int) {}
                private record Snapshot(Value: int) {}
                private static func Read(out value: string) -> bool { value = "42"; return true }
                static func Run() -> int {
                    let frame = Frame(20)
                    let snapshot = Snapshot(22)
                    let text = "42!"
                    let trimmed = if true { text[..^1] } else { text }
                    if trimmed != "42" { return -1 }
                    let wide: ulong = 84
                    if wide / 2 != 42 || wide > int.MaxValue { return -2 }
                    let small: uint = 41
                    let sum: uint = small + 1
                    if sum != 42 { return -3 }
                    if !(Read(out var value) && value == trimmed) { return -4 }
                    return frame.Value + snapshot.Value
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("LexerPortRuntime", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new System.IO.MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("value / -1")]
    [InlineData("value / signed")]
    public void UnsignedNonConvertibleOperandsRemainRejected(string expression)
    {
        var tree = SyntaxTree.ParseText($"class Probe {{ static func Run(value: ulong, signed: int) -> ulong => {expression} }}");
        var compilation = Compilation.Create("UnsignedOperand", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }
}
