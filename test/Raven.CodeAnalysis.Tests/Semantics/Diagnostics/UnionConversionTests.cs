using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class UnionConversionTests : DiagnosticTestBase
{
    [Fact]
    public void UnionAssignedToObject_ReturnsNoDiagnostic()
    {
        string code = """
import System.*
class Foo {}
class Bar {}

class Baz {
    func Test(flag: bool) -> Object {
        if flag {
            return Foo()
        } else {
            return Bar()
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void InstanceMembersFromObjectAreAccessibleOnUnion()
    {
        string code = """
import System.*

class Foo {}
class Bar {}

class Baz {
    func Test(flag: bool) {
        val value = if flag {
            Foo()
        } else {
            Bar()
        }

        value.ToString()
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void CommonBaseMembersAreAccessibleOnUnion()
    {
        string code = """
import System.*

open class Animal {
    func Speak() -> string {
        "sound"
    }
}

class Dog : Animal {}
class Cat : Animal {}

class Zoo {
    func Test(flag: bool) {
        val pet = if flag {
            Dog()
        } else {
            Cat()
        }

        pet.Speak()
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void UnionNotConvertibleToExplicitType_ProducesDiagnostic()
    {
        string code = """
class Foo {}
class Bar {}

class Baz {
    func Test(flag: bool) -> Foo {
        if flag {
            return Foo()
        } else {
            return Bar()
        }
    }
}
""";

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id).WithSpan(9, 20, 9, 25).WithArguments("Bar", "Foo"),
        ]);
        verifier.Verify();
    }

    [Fact]
    public void UnitType_UsesKeywordInDiagnostic()
    {
        var code = """
func test(flag: bool) {
    val value: int = if flag {
        ()
    } else {
        1
    }
}
""";

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id).WithSpan(2, 22, 6, 6).WithArguments("ValueType", "int"),
        ]);
        verifier.Verify();
    }

    [Fact]
    public void UnqualifiedCaseConstructor_WrongTypeArgs_ExplicitReturn_DiagnosticAtExpression()
    {
        // return Ok(1) where the return type is Result<(), string>: Ok is inferred as Ok<int>,
        // so the error should be at the Ok(1) expression, not at the function body.
        var code = """
func build() -> Result<(), string> {
    return Ok(1)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";
        // line 2: "    return Ok(1)" — Ok(1) at col 12..16 (end exclusive)
        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                .WithSpan(2, 12, 2, 17)
                .WithArguments("Result<int, E>", "Result<(), string>")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void UnqualifiedCaseConstructor_WrongTypeArgs_ImplicitReturn_DiagnosticAtExpression()
    {
        // Trailing Ok(1) (implicit return) in Result<(), string> body: error must point at
        // the expression itself, not at the entire function body.
        var code = """
func build() -> Result<(), string> {
    Ok(1)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";
        // line 2: "    Ok(1)" — Ok(1) at col 5..9 (end exclusive)
        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                .WithSpan(2, 5, 2, 10)
                .WithArguments("Result<int, E>", "Result<(), string>")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void IncompleteMemberAccessBeforeImplicitOkReturn_DoesNotReportResultConversionDiagnostic()
    {
        var code = """
import System.Console.*

func build() -> Result<(), AppError> {
    val result = [1, 2, 3]

    for no in result {
        WriteLine(no.)
    }

    Ok
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}

record AppError(Message: string)
""";
        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.IdentifierExpected.Id).WithAnySpan()
        ]);
        verifier.Verify();
    }
}

public class UnionCaseConversionClassificationTests : CompilationTestBase
{
    private static readonly string ResultUnionDecl = """
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

    [Fact]
    public void CaseToUnion_MatchingTypeArgs_IsImplicitlyConvertible()
    {
        // Ok<int> should be implicitly convertible to Result<int, string>.
        // func must precede union declaration (file-scope code before declarations).
        var source = """
func build(x: Ok<int>) -> Result<int, string> {
    return x
}
""" + ResultUnionDecl;

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(System.Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CaseToUnion_MismatchedTypeArgs_IsNotConvertible()
    {
        // Ok<int> must NOT be implicitly convertible to Result<(), string>.
        var source = """
func build(x: Ok<int>) -> Result<(), string> {
    return x
}
""" + ResultUnionDecl;

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
    }

    [Fact]
    public void CaseToUnion_GenericPayloadSubtype_IsImplicitlyConvertible()
    {
        var source = """
import System.*

func build() -> Result<string, Exception> {
    return Error(InvalidOperationException("x"))
}
""" + ResultUnionDecl;

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(System.Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CaseToUnion_GenericPayloadSupertypeToSubtype_IsNotImplicitlyConvertible()
    {
        var source = """
import System.*

func build() -> Result<string, InvalidOperationException> {
    return Error(Exception("x"))
}
""" + ResultUnionDecl;

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
    }

    [Fact]
    public void ImplicitReturn_TargetTypedOk_RemainsStableAfterPriorNonTargetBinding()
    {
        var source = """
func build() -> Result<(), string> {
    Ok
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var okIdentifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "Ok");

        var semanticModel = compilation.GetSemanticModel(tree);
        _ = semanticModel.GetBoundNode(okIdentifier);

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Id == CompilerDiagnostics.CannotConvertFromTypeToType.Id);
    }
}
