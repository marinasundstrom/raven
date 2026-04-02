using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MethodOverloadTests : CompilationTestBase
{
    [Fact]
    public void Overloads_DifferOnlyByNullableReferenceType_AreRejected()
    {
        var source = """
        class C {
            func f(x: string) -> int { 0 }
            func f(x: string?) -> int { 1 }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(methods[0]);
        _ = model.GetDeclaredSymbol(methods[1]);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.TypeAlreadyDefinesMember, diagnostic.Descriptor);
    }

    [Fact]
    public void Overloads_WithNullableValueType_AreAllowed()
    {
        var source = """
        class C {
            func f(x: int) -> int { 0 }
            func f(x: int?) -> int { 1 }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(methods[0]);
        _ = model.GetDeclaredSymbol(methods[1]);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void UnionArgument_UsesCommonDenominatorForOverloadResolution()
    {
        var source = """
        open class Base {}
        class D1 : Base {}
        class D2 : Base {}
        class C {
            static func m(x: Base) -> int { 0 }
            static func m(x: object) -> int { 1 }
            func test(flag: bool) -> int {
                val u = if flag { D1() } else { D2() }
                return m(u);
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax { Identifier.Text: "m" });
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;

        Assert.Equal("Base", symbol.Parameters[0].Type.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void LambdaArgument_CanBindToSystemDelegateParameter()
    {
        var source = """
        import System.*
        class C {
            static func takes(handler: Delegate) -> int { 1 }
            func run() -> int {
                return takes(() => 42)
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal(SpecialType.System_Delegate, symbol.Parameters[0].Type.SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1501");
    }

    [Fact]
    public void OverloadResolutionPriority_PrefersHigherPrioritySourceMethod()
    {
        var source = """
        import System.Runtime.CompilerServices.*

        class C {
            [OverloadResolutionPriority(1)]
            static func pick(value: object) -> int { 1 }

            static func pick(value: string) -> int { 2 }

            func run() -> int {
                return pick("ok")
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal(SpecialType.System_Object, symbol.Parameters[0].Type.SpecialType);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void OverloadResolutionPriority_PrefersHigherPriorityMetadataMethod()
    {
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
            import System.Runtime.CompilerServices.*

            class Library {
                [OverloadResolutionPriority(1)]
                public static func Pick(value: object) -> int { 1 }

                public static func Pick(value: string) -> int { 2 }
            }
            """,
            "OverloadResolutionPriorityFixture");

        var source = """
        class C {
            func run() -> int {
                return Library.Pick("ok")
            }
        }
        """;

        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.Equal(SpecialType.System_Object, boundInvocation.Method.Parameters[0].Type.SpecialType);
    }

    [Fact]
    public void LambdaArgument_PrefersTypedFuncOverSystemDelegateOverload()
    {
        var source = """
        import System.*
        class C {
            static func pick(handler: Delegate) -> int { 1 }
            static func pick(handler: Func<string>) -> int { 2 }

            func run() -> int {
                return pick(() => "ok")
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Func", symbol.Parameters[0].Type.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void LambdaArgument_CanBindToSystemMulticastDelegateParameter()
    {
        var source = """
        import System.*
        class C {
            static func takes(handler: MulticastDelegate) -> int { 1 }
            func run() -> int {
                return takes(() => 42)
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal(SpecialType.System_MulticastDelegate, symbol.Parameters[0].Type.SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1501");
    }

    [Fact]
    public void LambdaArgument_WithNamedLambdaArgument_InfersFromCompetingDelegateCandidates()
    {
        var source = """
        import System.*
        import System.Collections.Generic.*
        import System.Linq.Expressions.*

        class C {
            static func Pick(source: IEnumerable<int>, selector: Func<int, int>) -> int { 1 }
            static func Pick(source: IEnumerable<int>, selector: Expression<Func<int, string>>) -> int { 2 }

            func run() -> int {
                val values: IEnumerable<int> = [1, 2, 3]
                return Pick(values, selector: x => x + 1)
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax { Identifier.Text: "Pick" });

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.Equal("Func", boundInvocation.Method.Parameters[1].Type.Name);

        var lambda = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void LambdaArgument_OverloadsWithOptionalTail_DoNotPolluteInference()
    {
        var source = """
        import System.*

        class C {
            static func Transform(projector: Func<int, int>) -> int { 1 }
            static func Transform(projector: Func<string, string>, fallback: string = "") -> int { 2 }

            func run() -> int {
                return Transform(x => x + 1)
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax { Identifier.Text: "Transform" });

        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Transform", symbol.Name);
        var projectorType = Assert.IsAssignableFrom<INamedTypeSymbol>(symbol.Parameters[0].Type);
        Assert.Equal(SpecialType.System_Int32, projectorType.TypeArguments[0].SpecialType);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => ReferenceEquals(diagnostic.Descriptor, CompilerDiagnostics.LambdaParameterTypeCannotBeInferred));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void LambdaArgument_WithRequestDelegateAndSystemDelegateOverloads_PrefersSystemDelegateForSyncLambda()
    {
        var source = """
        import System.*
        import System.Threading.Tasks.*

        namespace Microsoft.AspNetCore.Http {
            public class HttpContext { }
        }

        class C {
            static func map(handler: Func<Microsoft.AspNetCore.Http.HttpContext, Task>) -> int { 1 }
            static func map(handler: Delegate) -> int { 2 }

            func run() -> int {
                return map((name: string) => "ok")
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax { Identifier.Text: "map" });

        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal(SpecialType.System_Delegate, symbol.Parameters[0].Type.SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1503");
    }

    [Fact]
    public void LambdaArgument_WithRequestDelegateLikeOverload_PrefersExplicitlyTypedLambdaMatch()
    {
        var source = """
        import System.*
        import System.Threading.Tasks.*

        namespace Microsoft.AspNetCore.Http {
            public class HttpContext { }
        }

        class C {
            static func map(handler: Func<Microsoft.AspNetCore.Http.HttpContext, Task>) -> int { 1 }
            static func map(handler: Func<string, string>) -> int { 2 }

            func run() -> int {
                return map((name: string) => "ok")
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax { Identifier.Text: "map" });

        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Func", symbol.Parameters[0].Type.Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1501");
    }

    [Fact]
    public void LambdaArgument_WithRequestDelegateLikeOverload_ReportsLambdaBodyDiagnostics()
    {
        var source = """
        import System.*
        import System.Threading.Tasks.*

        namespace Microsoft.AspNetCore.Http {
            public class HttpContext { }
        }

        class C {
            static func map(handler: Func<Microsoft.AspNetCore.Http.HttpContext, Task>) -> int { 1 }
            static func map(handler: Func<string, string>) -> int { 2 }

            func run() -> int {
                return map((name: string) => missingValue)
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic =>
            diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext &&
            diagnostic.GetMessage().Contains("missingValue"));
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV1501");
    }

    [Fact]
    public void LambdaArgument_WithRequestDelegateLikeOverload_WhenNoSignatureMatches_ReportsConversionDiagnostics()
    {
        var source = """
        import System.*
        import System.Threading.Tasks.*

        namespace Microsoft.AspNetCore.Http {
            public class HttpContext { }
        }

        class C {
            static func map(handler: Func<Microsoft.AspNetCore.Http.HttpContext, Task>) -> int { 1 }
            static func map(handler: Func<string, string>) -> int { 2 }

            func run() -> int {
                return map(() => "ok")
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType &&
                          diagnostic.GetMessage().Contains("() -> Task") &&
                          diagnostic.GetMessage().Contains("HttpContext -> Task"));
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType &&
                          diagnostic.GetMessage().Contains("'string'") &&
                          diagnostic.GetMessage().Contains("'Task'"));
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
    }
}
