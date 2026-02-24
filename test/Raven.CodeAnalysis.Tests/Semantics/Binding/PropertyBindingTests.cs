using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PropertyBindingTests : DiagnosticTestBase
{
    [Fact]
    public void StoredVarProperty_DeclarationBindsAsProperty()
    {
        const string testCode =
            """
            class Counter {
                public var Count: int = 0
            }

            val counter = Counter()
            counter.Count = 1
            val current = counter.Count
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitFieldDeclaration_WithFieldKeyword_BindsAsStorageMember()
    {
        const string testCode =
            """
            class Counter {
                private field _count: int = 0

                public func Increment() -> unit {
                    _count = _count + 1
                }
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void AccessingInstancePropertyAsStatic_ProducesDiagnostic()
    {
        string testCode =
            """
            class Foo {
                public Bar: int {
                    get => 0
                }
            }
            Foo.Bar
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.MemberDoesNotContainDefinition.Id).WithSpan(6, 5, 6, 8).WithArguments("Foo", "Bar")]);

        verifier.Verify();
    }

    [Fact]
    public void AccessingStaticPropertyAsInstance_ProducesDiagnostic()
    {
        string testCode =
            """
            class Foo {
                public static Bar: int {
                    get => 0
                }
            }
            val f = Foo()
            f.Bar;
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithSpan(7, 3, 7, 6).WithArguments("Bar")]);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyAutoProperty_AssignableInConstructor()
    {
        string testCode =
            """
            class Person {
                public Name: string { get; }
                init (name: string) {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyAutoProperty_AssignmentOutsideConstructor_ProducesDiagnostic()
    {
        string testCode =
            """
            class Person {
                public Name: string { get; }
                public func SetName(name: string) -> unit {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0200").WithSpan(4, 9, 4, 13).WithArguments("Name")]);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyPropertyWithAccessorBody_AssignmentInConstructor_ProducesDiagnostic()
    {
        string testCode =
            """
            class Person {
                public Name: string {
                    get => ""
                }
                init (name: string) {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0200").WithSpan(6, 9, 6, 13).WithArguments("Name")]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyAccessorFieldKeyword_BindsToSynthesizedBackingField()
    {
        string testCode =
            """
            class Box {
                public Value: int {
                    get => field
                    set {
                        field = value;
                    }
                }
            }

            val box = Box()
            box.Value = 42
            val current = box.Value
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ValProperty_WithSetAccessor_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                val Count: int {
                    get => field
                    set => field = value
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.ValPropertyCannotDeclareWritableAccessor.Id).WithSpan(4, 9, 4, 12).WithArguments("Count", "set")]);

        verifier.Verify();
    }

    [Fact]
    public void VarProperty_WithoutWritableShape_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                var Count: int {
                    get => field
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.VarPropertyRequiresWritableShape.Id).WithSpan(2, 5, 2, 8).WithArguments("Count")]);

        verifier.Verify();
    }

    [Fact]
    public void VarProperty_WithStorageInitializer_DoesNotProduceDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                var Count: int = 0
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void FieldKeyword_OutsidePropertyAccessor_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                func Read() -> int {
                    val value = field
                    return value
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.FieldKeywordOnlyValidInPropertyAccessor.Id).WithSpan(3, 21, 3, 26)]);

        verifier.Verify();
    }

    [Fact]
    public void FieldKeyword_InComputedProperty_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                val Value: int => field
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.FieldKeywordRequiresPropertyBackingStorage.Id).WithSpan(2, 23, 2, 28).WithArguments("Value")]);

        verifier.Verify();
    }

}
