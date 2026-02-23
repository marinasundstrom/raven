using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

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
    Test(flag: bool) -> Object {
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
    Test(flag: bool) {
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
    public Speak() -> string {
        "sound"
    }
}

class Dog : Animal {}
class Cat : Animal {}

class Zoo {
    Test(flag: bool) {
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
    Test(flag: bool) -> Foo {
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
    public void BooleanLiteral_UsesKeywordInDiagnostic()
    {
        var code = "val x: \"true\" | 1 = true";
        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id).WithAnySpan().WithArguments("'true'", "\"true\" | 1")
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
            new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id).WithAnySpan().WithArguments("'() | int'", "'int'")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void NumericLiteralNotInUnion_ProducesDiagnostic()
    {
        var code = "val x: \"true\" | 1 = 2";
        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id).WithAnySpan().WithArguments("'2'", "\"true\" | 1")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void NullLiteral_AssignableToUnionParameter()
    {
        var code = """
func describe(input: string | int | null) {}

describe(null)
""";

        var verifier = CreateVerifier(code);
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
    Ok(value: T)
    Error(error: E)
}
""";
        // line 2: "    return Ok(1)" — Ok(1) at col 12..16 (end exclusive)
        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                .WithSpan(2, 12, 2, 17)
                .WithArguments("Ok<int>", "Result<(), string>")
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
    Ok(value: T)
    Error(error: E)
}
""";
        // line 2: "    Ok(1)" — Ok(1) at col 5..9 (end exclusive)
        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                .WithSpan(2, 5, 2, 10)
                .WithArguments("Ok<int>", "Result<(), string>")
        ]);
        verifier.Verify();
    }
}

public class DiscriminatedUnionCaseConversionClassificationTests : CompilationTestBase
{
    private static readonly string ResultUnionDecl = """
union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
}

public class UnionConversionClassificationTests : CompilationTestBase
{
    [Fact]
    public void NullLiteral_ClassifyConversionForUnionParameter_IsImplicit()
    {
        var source = """
func describe(input: string | int | null) -> string { "" }

describe(null)
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        var parameterType = symbol.Parameters[0].Type;
        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, parameterType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
    }
}
