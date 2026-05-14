using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MultipleSyntaxTreeTests
{
    [Fact]
    public void MultipleSyntaxTrees_ReferenceAcrossTrees_ProducesNoDiagnostics()
    {
        var tree1 = SyntaxTree.ParseText("""
class Helper {
    public init () {}

    public func GetValue() -> int => 42;
}
""");

        var tree2 = SyntaxTree.ParseText("""
class Program {
    static func Main() -> unit {
        val helper = Helper();
        val value = helper.GetValue();
        return;
    }
}
""");

        var compilation = Compilation.Create(
            "app",
            new[] { tree1, tree2 },
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var semanticModel = compilation.GetSemanticModel(tree2);
        var diagnostics = semanticModel.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void MultipleSyntaxTrees_ExtensionReturnsUnionCaseFromLaterTree_ProducesNoDiagnostics()
    {
        var useTree = SyntaxTree.ParseText("""
namespace System

import System.*

union Option<T> {
    case Some(value: T)
    case None
}

extension OptionExtensions<T> for Option<T> {
    func IsOkOr<E>(error: E) -> Result<T, E> {
        if self is .Some(val value) {
            return .Ok(value)
        }

        .Error(error)
    }
}
""");

        var resultTree = SyntaxTree.ParseText("""
namespace System

union Result<T, E> {
    case Ok(value: T)
    case Error(value: E)
}
""");

        var compilation = Compilation.Create(
            "app",
            new[] { useTree, resultTree },
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void MultipleSyntaxTrees_ExtensionConstraintFromLaterTree_ProducesNoDiagnostics()
    {
        var useTree = SyntaxTree.ParseText("""
namespace System

import System.*

interface IError {
    val Message: string
}

record ContextError<TError: IError>(
    val Message: string,
    val Cause: TError
) : IError

extension ErrorExtensions<TError: IError> for TError {
    func WithContext(message: string) -> ContextError<TError> {
        ContextError<TError>(message, self)
    }
}

extension ResultErrorContextExtensions<T, E: IError> for Result<T, E> {
    func WithContext(message: string) -> Result<T, ContextError<E>> {
        self.MapError(error => error.WithContext(message))
    }
}
""");

        var resultTree = SyntaxTree.ParseText("""
namespace System

import System.*

union Result<T, E> {
    case Ok(value: T)
    case Error(value: E)
}

extension ResultExtensions<T, E> for Result<T, E> {
    func MapError<E2>(mapper: E -> E2) -> Result<T, E2> {
        self match {
            .Ok(val value) => .Ok(value)
            .Error(val error) => .Error(mapper(error))
        }
    }
}
""");

        var compilation = Compilation.Create(
            "app",
            new[] { useTree, resultTree },
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }
}
