using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// The native target owns process-result adaptation. Use ordinary bound Raven for
// Result patterns, boxing and console I/O, rather than reproducing union encoding.
internal static class NeoClrEntryPoint
{
    internal const string BridgeName = "Raven.Generated.ResultEntryBridge";

    internal static bool TryGetResult(ITypeSymbol type, out ITypeSymbol output, out ITypeSymbol error)
    {
        if (type is INamedTypeSymbol { SpecialType: SpecialType.System_Threading_Tasks_Task_T, TypeArguments.Length: 1 } task)
            type = task.TypeArguments[0];
        return EntryPointSignature.TryGetResultPayloadTypes(type, out output, out error, out _);
    }

    internal static Compilation Prepare(Compilation compilation)
    {
        if (compilation.Options.OutputKind != OutputKind.ConsoleApplication ||
            compilation.GetEntryPoint() is not { } entry ||
            !TryGetResult(entry.ReturnType, out _, out var errorType))
            return compilation;

        return compilation.AddSyntaxTrees(SyntaxTree.ParseText("""
            module Raven.Generated
            import System.*
            import System.Result.*

            internal class ResultEntryBridge {
                static func CompleteInt<E>(result: Result<int, E>) -> int {
                    if result is Ok(let value) { return value }
                    if result is Failure(let error) {
                        return Report(error)
                    }
                    Panic("Invalid entry point Result")
                }
                static func CompleteUnit<E>(result: Result<(), E>) -> int {
                    if result is Ok(_) { return 0 }
                    if result is Failure(let error) {
                        return Report(error)
                    }
                    Panic("Invalid entry point Result")
                }
                static func Report<E>(error: E) -> int {
                    let writer = Console.Error
                    let boxed: object? = error
                    var text = ""
                    if boxed is object value { text = value.ToString() }
                    _ = writer.WriteLine(text)
                    return 1
                }
            }
            """, path: "<native-result-entry>"));
    }
}
