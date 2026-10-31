using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NominalConstrainedCallChecks
{
    internal static void Run(MetadataReference[] references, CompilationOptions options,
        Func<string, NeoClrEmissionBackend> backend, string directory)
    {
        const string source = """
            public interface Counter { func Next() -> int }
            public struct ValueCounter : Counter {
                private var count: int
                public func Next() -> int { count = count + 1; return count }
            }
            public open class ReferenceCounter : Counter {
                private var count: int
                public virtual func Next() -> int { count = count + 1; return count }
            }
            public class DerivedCounter : ReferenceCounter {
                public override func Next() -> int => 10
            }
            public class Holder<T: Counter> {
                public static func Sum(value: T) -> int {
                    var local = value
                    return local.Next() + local.Next()
                }
            }
            func Sum<T: Counter>(value: T) -> int {
                var local = value
                return local.Next() + local.Next()
            }
            func Main() -> int {
                if Holder<ValueCounter>.Sum(ValueCounter()) != 3 { return 1 }
                if Holder<ReferenceCounter>.Sum(ReferenceCounter()) != 3 { return 2 }
                if Sum(ValueCounter()) != 3 { return 3 }
                if Sum(ReferenceCounter()) != 3 { return 4 }
                if Holder<ReferenceCounter>.Sum(DerivedCounter()) != 20 { return 5 }
                if Sum<ReferenceCounter>(DerivedCounter()) != 20 { return 6 }
                return 42
            }
            """;
        const string name = "NominalConstrainedCalls";
        var compilation = Compilation.Create(name, [SyntaxTree.ParseText(source)], references,
            options.WithOutputKind(OutputKind.ConsoleApplication));
        using var image = new MemoryStream();
        var result = compilation.Emit(image, null, new EmitOptions().WithBackend(backend(name)));
        if (!result.Success) throw new Exception(name + ": " + string.Join("\n", result.Diagnostics));
        File.WriteAllBytes(Path.Combine(directory, name + ".dll"), image.ToArray());
    }
}
