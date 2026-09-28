using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests.Utilities;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

// Executable representation probes, not support for intersection source annotations.
public sealed class IntersectionReferenceErasureTests
{
    [Fact]
    public void InterfaceViews_PreserveIdentityAndSharedMutation()
    {
        AssertRunsTrue("""
            import System.*
            interface IRead { func Read() -> int }
            interface IWrite { func Write(value: int) -> unit }
            class Cell: IRead, IWrite {
                private field value: int = 0
                public func Read() -> int => value
                public func Write(next: int) -> unit { value = next }
            }
            public class Runner {
                public static func Run() -> bool {
                    let original = Cell()
                    let stored: object = original
                    let reader = (IRead)stored
                    let writer = (IWrite)stored
                    writer.Write(42)
                    return Object.ReferenceEquals(original, reader) &&
                        Object.ReferenceEquals(reader, writer) && reader.Read() == 42
                }
            }
            """);
    }

    [Fact]
    public void ClassAndInterfaceViews_PreserveIdentityAndVirtualDispatch()
    {
        AssertRunsTrue("""
            import System.*
            interface IExtra { func Extra() -> int }
            open class Base { public virtual func Read() -> int => 1 }
            class Derived: Base, IExtra {
                public override func Read() -> int => 40
                public func Extra() -> int => 2
            }
            public class Runner {
                public static func Run() -> bool {
                    let original = Derived()
                    let stored: object = original
                    let baseView = (Base)stored
                    let extraView = (IExtra)stored
                    return Object.ReferenceEquals(original, baseView) &&
                        Object.ReferenceEquals(baseView, extraView) &&
                        baseView.Read() + extraView.Extra() == 42
                }
            }
            """);
    }

    [Fact]
    public void ExplicitInterfaceViews_KeepConflictingImplementationsDistinct()
    {
        AssertRunsTrue("""
            import System.*
            interface ILeft { func Read() -> int }
            interface IRight { func Read() -> int }
            class Both: ILeft, IRight {
                func ILeft.Read() -> int => 40
                func IRight.Read() -> int => 2
            }
            public class Runner {
                public static func Run() -> bool {
                    let original = Both()
                    let stored: object = original
                    let left = (ILeft)stored
                    let right = (IRight)stored
                    return Object.ReferenceEquals(original, left) &&
                        Object.ReferenceEquals(left, right) &&
                        left.Read() == 40 && right.Read() == 2
                }
            }
            """);
    }

    [Fact]
    public void CheckedMembership_RequiresBothBoundsOnTheSameNonNullObject()
    {
        AssertRunsTrue("""
            interface ILeft {}
            interface IRight {}
            class Left: ILeft {}
            class Right: IRight {}
            class Both: ILeft, IRight {}
            public class Runner {
                private static func Accept(value: object?) -> bool {
                    return (value is ILeft) && (value is IRight)
                }
                public static func Run() -> bool {
                    return Accept(Both()) && !Accept(Left()) && !Accept(Right()) && !Accept(null)
                }
            }
            """);
    }

    private static void AssertRunsTrue(string source)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("intersection-reference-erasure",
            [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(true, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }
}
