using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SyntaxPortScopeTests
{
    [Theory]
    [InlineData("""
        public class Probe {
            private static func Value() -> int => 42
            private class Nested { func Read() -> int => Value() }
            static func Run() -> int => Nested().Read()
        }
        """, 42)]
    [InlineData("""
        public class Probe {
            static func Run() -> int {
                func Read(index: int) -> int => index
                let index = 7
                return Read(42)
            }
        }
        """, 42)]
    [InlineData("""
        import System.Linq.*
        public class Probe {
            static func Run() -> int {
                let values = [1, 2]
                return values.SelectMany(value => [value, value]).Sum()
            }
        }
        """, 6)]
    [InlineData("""
        public class Probe {
            private static func Value() -> int => 42
            private class Nested {
                private static func Value() -> int => 21
                func Read() -> int => Value()
            }
            static func Run() -> int => Nested().Read()
        }
        """, 21)]
    [InlineData("""
        public class Probe {
            static func Copy<T>(value: T) -> T[] {
                let make: () -> T[] = () => [value]
                return make()
            }
            static func Run() -> int => Copy(42)[0]
        }
        """, 42)]
    [InlineData("""
        import System.Linq.*
        public class Probe {
            static func Run() -> int {
                let offset = 40
                let values = [1, 2].Select(value => (Value: value, Sum: value + offset)).ToArray()
                return values[1].Sum
            }
        }
        """, 42)]
    [InlineData("""
        import System.*
        public class Probe {
            static func Run() -> int {
                var result = 0
                for value in [1, 2, 3] {
                    try {
                        if value == 1 { continue }
                        if value == 2 { throw InvalidOperationException() }
                        break
                    } catch (InvalidOperationException error) {
                        continue
                    } finally {
                        result += value
                    }
                }
                return result
            }
        }
        """, 6)]
    [InlineData("""
        import System.*
        import System.Linq.*
        import System.Collections.Generic.*
        public class Probe {
            static func Read(input: int, out value: int) -> bool { value = input; return true }
            static func Values() -> IEnumerable<int> {
                for input in [20, 22] {
                    if Read(input, out let value) { yield value }
                }
            }
            static func Run() -> int => Values().Sum()
        }
        """, 42)]
    [InlineData("""
        import System.Linq.*
        import System.Collections.Generic.*
        public class Node {
            private field children: Node[]
            init(children: Node[]) { self.children = children }
            func Children() -> IEnumerable<Node> => children
            func Values() -> IEnumerable<int> {
                for child in Children() {
                    let node = child
                    for value in node.Values() { yield value }
                }
                yield 1
            }
        }
        public class Probe {
            static func Run() -> int => Node([Node([])]).Values().Sum()
        }
        """, 2)]
    [InlineData("""
        public class Probe {
            static func Count<T>(value: T, depth: int) -> int {
                if depth > 0 { return Count(value, depth - 1) }
                return 1
            }
            static func Run() -> int => Count(42, 2)
        }
        """, 1)]
    [InlineData("""
        import System.Linq.*
        import System.Collections.Generic.*
        public class Node {
            private field children: Node[]
            private field leaf: bool
            init(children: Node[], leaf: bool) { self.children = children; self.leaf = leaf }
            func Children() -> IEnumerable<Node> => children
            func Values() -> IEnumerable<int> {
                for child in Children() {
                    if child.leaf {
                        yield 1
                    } else {
                        let node = child
                        for value in node.Values() { yield value }
                    }
                }
            }
        }
        public class Probe {
            static func Run() -> int => Node([Node([], true), Node([Node([], true)], false)], false).Values().Sum()
        }
        """, 2)]
    [InlineData("""
        import System.Linq.*
        import System.Collections.Generic.*
        public class Probe {
            static func Values(input: object) -> IEnumerable<int> {
                yield 1
                if input is string text {
                    yield text.Length
                    yield text.Length
                }
            }
            static func Run() -> int => Values("abc").Sum()
        }
        """, 7)]
    public void SyntaxPortScopeAndInferenceBehaveLikeDotNet(string source, int expected)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("SyntaxPortScope", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
