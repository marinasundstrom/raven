using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ObjectCreationTests : DiagnosticTestBase
{
    [Fact]
    public void InvocationWithoutNewCreatesObject()
    {
        string testCode =
            """
            val foo = Foo();

            class Foo {
                init () {}
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void InvocationPrefersMethodOverConstructor()
    {
        string testCode =
            """
            val i = Foo(3);

            class Foo {
                init () {}
            }

            func Foo(x: int) -> int {
                return x;
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void InvocationFallsBackToConstructorWhenMethodNotApplicable()
    {
        string testCode =
            """
            val i = Foo(1);

            func Foo() -> unit {}

            class Foo {
                init () {}

                init (x: int) {} 
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void StaticFactoryMethodCreatesObject()
    {
        string testCode =
            """
            val p = Person.WithName("John");
            val n = p.GetName();

            class Person {
                var name: string;

                static func WithName(name: string) -> Person {
                    return Person(name);
                }

                init (name: string) {
                    self.name = name
                }

                func GetName() -> string => name;
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void StaticFactoryMethodWithLocalDeclarationCreatesObject()
    {
        string testCode =
            """
            val p = Person.WithName("John");
            val n = p.GetName();

            class Person {
                var name: string;

                static func WithName(name: string) -> Person {
                    val temp = name;
                    return Person(temp);
                }

                init (name: string) {
                    self.name = name;
                }

                func GetName() -> string => name;
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void DefaultConstructorSynthesizedWhenMissing()
    {
        string testCode =
            """
            val f = Foo();

            class Foo {
                var x: int;
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GenericTypeInvocationCreatesObject()
    {
        string testCode =
            """
            import System.Collections.Generic.List<>

            val list = List<int>()
            list.Add(1)
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GenericTypeInvocationWithNamespaceImport_ResolvesMembers()
    {
        string testCode =
            """
            import System.Collections.Generic.*

            val list = List<int>()
            list.Add(1)
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GenericTypeInvocation_InfersTypeArgumentsFromTargetType()
    {
        string testCode =
            """
            func Main() -> () {
                val box: Box<int> = Box()
            }

            class Box<T> {
                init() {}
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void NewObjectCreation_ReportsUnexpectedTokenDiagnostic()
    {
        string testCode =
            """
            func Main() -> () {
                val box: Box<int> = new Box()
            }

            class Box<T> {
                init() {}
            }
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult(CompilerDiagnostics.ExpressionExpected.Id).WithAnySpan(),
                new DiagnosticResult(CompilerDiagnostics.ConsecutiveStatementsMustBeSeparatedBySemicolon.Id).WithAnySpan()
            ]);

        verifier.Verify();
    }

    [Fact]
    public void GenericTypeInvocation_InfersTypeArgumentsFromReturnTargetType()
    {
        string testCode =
            """
            func Main() -> () {
                val box = Create()
            }

            func Create() -> Box<int> {
                return Box()
            }

            class Box<T> {
                init() {}
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GenericTypeInvocation_WithCompetingNonGenericType_DoesNotInferGenericFromTargetType()
    {
        string testCode =
            """
            func Main() -> () {
                val concrete = Test()
                val generic: Test<int> = Test()
            }

            class Test {
                init() {}
            }

            class Test<T> {
                init() {}
            }
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id)
                    .WithAnySpan()
                    .WithArguments("Test", "Test<int>")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void GenericUnionInvocation_WithoutTargetType_DoesNotInferUnusedTypeArguments()
    {
        string testCode =
            """
            import System.Collections.Generic.*

            func Main() {
                val test = MyResult(42)
            }

            union MyResult<T>(List<T> | int)
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult(CompilerDiagnostics.TypeRequiresTypeArguments.Id).WithSpan(4, 16, 4, 24).WithArguments("MyResult", 1)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructor_ValParameter_PromotesToInstanceProperty()
    {
        string testCode =
            """
            val person = Person("John")
            val name = person.GetName()

            class Person(val name: string)
            {
                func GetName() -> string => name
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructor_VarParameter_PromotesToMutableInstanceProperty()
    {
        string testCode =
            """
            val counter = Counter(1)
            counter.Increment()
            val value = counter.GetValue()

            class Counter(var value: int)
            {
                func Increment() {
                    value = value + 1
                }

                func GetValue() -> int => value
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void BareTypeInitializer_ReportsInvalidInvocationEvenWithParameterlessConstructor()
    {
        string testCode =
            """
            val foo = Foo

            class Foo {
                init() {}
            }
            """;

        var verifier = CreateVerifier(
            testCode,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.InvalidInvocation.Id).WithSpan(1, 11, 1, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void BareTypeInitializer_WithoutParameterlessConstructor_ReportsInvalidInvocation()
    {
        string testCode =
            """
            val foo = Foo

            class Foo(var Name: string)
            """;

        var verifier = CreateVerifier(
            testCode,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.InvalidInvocation.Id).WithSpan(1, 11, 1, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_WithoutParens_RemainsSupported()
    {
        string testCode =
            """
            val foo = Foo { }

            class Foo {
                init() {}
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_AssigningValProperty_ReportsReadOnlyDiagnostic()
    {
        const string testCode =
            """
            class Foo {
                val Name: string = ""
            }

            val foo = Foo {
                Name = "updated"
            }
            """;

        var verifier = CreateVerifier(
            testCode,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.PropertyOrIndexerCannotBeAssignedIsReadOnly.Id)
                    .WithAnySpan()
                    .WithArguments("Name")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_AssigningVarProperty_BindsWithoutDiagnostics()
    {
        const string testCode =
            """
            class Foo {
                var Name: string = ""
            }

            val foo = Foo {
                Name = "updated"
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_AssigningInitOnlyProperty_BindsWithoutDiagnostics()
    {
        const string testCode =
            """
            class Foo {
                val Name: string { init; }
            }

            val foo = Foo {
                Name = "updated"
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
