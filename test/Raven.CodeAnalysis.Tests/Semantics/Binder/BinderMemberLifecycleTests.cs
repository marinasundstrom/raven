using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests.Binding;

public sealed class BinderMemberLifecycleTests : CompilationTestBase
{
    [Fact]
    public void FinallyDeclaration_DoesNotBecomeDuplicateDuringMemberBinding()
    {
        var (compilation, _) = CreateCompilation(
            """
class Customer {
    finally {
    }
}
""");

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic =>
            diagnostic.Descriptor == CompilerDiagnostics.TypeAlreadyDefinesMember);
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void DuplicateFinallyDeclarations_ReportDuplicateMember()
    {
        var (compilation, _) = CreateCompilation(
            """
class Customer {
    finally {
    }

    finally {
    }
}
""");

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic =>
            diagnostic.Descriptor == CompilerDiagnostics.TypeAlreadyDefinesMember);
    }

    [Fact]
    public void StaticFactoryMethod_UsesCanonicalSourceMethodForEmission()
    {
        var (compilation, _) = CreateCompilation(
            """
val user = Person.WithName("Ada")

class Person {
    static func WithName(name: string) -> Person {
        Person()
    }
}
""",
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        AssertEmitSucceeds(compilation);
    }

    [Fact]
    public void PrimaryConstructorRecordBase_CanBindDerivedConstructorArguments()
    {
        var (compilation, _) = CreateCompilation(
            """
sealed record Expr
record Lit(Value: int) : Expr
sealed record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
""");

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void AsyncGenericInvocation_PreservesTaskReturnType()
    {
        var (compilation, tree) = CreateCompilation(
            """
import System.*
import System.Threading.Tasks.*

val foo = Foo(42)
val result = await foo(true)

class Box<T>(Value: T)

class Foo(value: int) {
    async func Test(flag: bool) -> Task<Box<int>> {
        await Task.Delay(1)

        if flag {
            return Box<int>(value)
        }

        return Box<int>(0)
    }

    func self(flag: bool) -> Task<Box<int>> {
        return Test(flag)
    }
}
""",
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == "result");
        var resultSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));

        Assert.Equal("Box", resultSymbol.Type.Name);
    }

    private static void AssertEmitSucceeds(Compilation compilation)
    {
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(static diagnostic => diagnostic.ToString())));
    }
}
