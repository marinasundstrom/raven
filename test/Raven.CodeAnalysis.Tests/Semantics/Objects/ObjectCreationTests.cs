using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ObjectCreationTests : DiagnosticTestBase
{
    [Fact]
    public void InvocationWithoutNewCreatesObject()
    {
        string testCode =
            """
            let foo = Foo();

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
            let i = Foo(3);

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
            let i = Foo(1);

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
            let p = Person.WithName("John");
            let n = p.GetName();

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
            let p = Person.WithName("John");
            let n = p.GetName();

            class Person {
                var name: string;

                static func WithName(name: string) -> Person {
                    let temp = name;
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
            let f = Foo();

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

            let list = List<int>()
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

            let list = List<int>()
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
                let box: Box<int> = Box()
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
                let box: Box<int> = new Box()
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
                let box = Create()
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
                let concrete = Test()
                let generic: Test<int> = Test()
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
                let test = MyResult(42)
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
            let person = Person("John")
            let name = person.GetName()

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
            let counter = Counter(1)
            counter.Increment()
            let value = counter.GetValue()

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
            let foo = Foo

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
            let foo = Foo

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
    public void ObjectInitializer_OnParameterlessConstructor_BindsWithoutDiagnostics()
    {
        string testCode =
            """
            let foo = Foo {
                Name = "Foo"
            }

            class Foo {
                init() {}

                var Name: string = ""
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_AfterConstructorArguments_BindsWithoutDiagnostics()
    {
        const string testCode =
            """
            let bar = Bar("Foo") {
                Age = 42
            }

            class Bar(var Name: string) {
                var Age: int = 0
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
                init() {}

                val Name: string { init; }
            }

            let foo = Foo {
                Name = "updated"
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_CompoundAssignment_BindsWithoutDiagnostics()
    {
        const string testCode =
            """
            class Foo {
                init() {}

                var Count: int = 1
            }

            let foo = Foo {
                Count += 41
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_SatisfiesRequiredMembers()
    {
        const string testCode =
            """
            class Person {
                init() {}

                required val Name: string { init; }
                required val Age: int { init; }
            }

            let person = Person {
                Name = "Anna"
                Age = 42
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_MissingRequiredMember_ReportsDiagnostic()
    {
        const string testCode =
            """
            class Person {
                init() {}

                required val Name: string { init; }
                required val Age: int { init; }
            }

            let person = Person {
                Name = "Anna"
            }
            """;

        var verifier = CreateVerifier(
            testCode,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.RequiredMemberMustBeSet.Id)
                    .WithAnySpan()
                    .WithArguments("Age")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ObjectInitializer_ContentEntries_BindThroughInitializerConventions()
    {
        const string testCode =
            """
            open class Control {
                init() {}
            }

            class Button : Control {
                init() {}

                var HorizontalAlignment: HorizontalAlignment = .Fill
            }

            class StackPanel {
                init() {}

                func Add(control: Control) -> unit {}
            }

            enum HorizontalAlignment {
                Fill
            }

            let panel = StackPanel {
                Button {
                    HorizontalAlignment = .Fill
                }
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
