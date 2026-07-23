using System.Collections.Generic;
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
    public void CollectionLiteralArgument_UsesArrayTargetWhenOverloadsDisagree()
    {
        const string source = """
        import System.*

        val t = typeof(string)
        val value: object? = "x"
        val created = Activator.CreateInstance(t, [value])
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
                memberAccess.Name.Identifier.ValueText == "CreateInstance");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("CreateInstance", boundInvocation.Method.Name);
        Assert.IsAssignableFrom<IArrayTypeSymbol>(boundInvocation.Method.Parameters[1].Type.GetPlainType());
        var argument = Assert.IsType<BoundCollectionExpression>(boundInvocation.Arguments.ElementAt(1));
        Assert.IsAssignableFrom<IArrayTypeSymbol>(argument.Type.GetPlainType());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void TrailingBlock_BindsAsFinalClosureArgument()
    {
        const string source = """
        func Use(action: () -> int) -> int {
            return action()
        }

        val result = Use {
            return 42
        }
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.TrailingBlock is not null);

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("Use", symbol.Name);
        Assert.Single(symbol.Parameters);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void TrailingBlock_BindsToFinalOptionalClosureAfterNamedArguments()
    {
        const string source = """
        func StackPanel(orientation: string = "vertical", spacing: int = 0, content: (() -> string)? = null) -> string {
            return orientation + spacing.ToString() + (content?() ?? "")
        }

        val result = StackPanel(spacing: 8) {
            return "child"
        }
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.TrailingBlock is not null);

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("StackPanel", symbol.Name);
        Assert.Equal(["orientation", "spacing", "content"], symbol.Parameters.Select(static parameter => parameter.Name).ToArray());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ParameterDefaultValue_AllowsTargetTypedExternalEnumMember()
    {
        const string source = """
        import System.*

        func Paint(color: ConsoleColor = .Green) -> ConsoleColor {
            return color
        }

        val result = Paint()
        """;

        var (compilation, _) = CreateCompilation(source);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void TrailingBlock_WithExplicitSingleParameterClosure_BindsParameter()
    {
        const string source = """
        func Apply(value: int, transform: int -> int) -> int {
            return transform(value)
        }

        val result = Apply(41) { (value: int) =>
            return value + 1
        }
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var trailingBlock = tree.GetRoot()
            .DescendantNodes()
            .OfType<TrailingBlockExpressionSyntax>()
            .Single();
        var bodyStart = trailingBlock.FatArrowToken.Span.End;
        var bodyEnd = trailingBlock.CloseBraceToken.SpanStart;
        var identifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Span.Start > bodyStart &&
                identifier.Span.End < bodyEnd &&
                identifier.Identifier.ValueText == "value");

        var symbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetSymbolInfo(identifier).Symbol);
        Assert.Equal("value", symbol.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void TrailingBlock_WithExplicitMultipleParameterClosure_BindsParameters()
    {
        const string source = """
        func Combine(left: int, right: int, transform: (int, int) -> int) -> int {
            return transform(left, right)
        }

        val result = Combine(20, 22) { (left: int, right: int) =>
            return left + right
        }
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var trailingBlock = tree.GetRoot()
            .DescendantNodes()
            .OfType<TrailingBlockExpressionSyntax>()
            .Single();
        var bodyStart = trailingBlock.FatArrowToken.Span.End;
        var bodyEnd = trailingBlock.CloseBraceToken.SpanStart;
        var identifiers = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(identifier => identifier.Span.Start > bodyStart &&
                identifier.Span.End < bodyEnd &&
                identifier.Identifier.ValueText is "left" or "right")
            .ToArray();

        Assert.Equal(["left", "right"], identifiers.Select(static identifier => identifier.Identifier.ValueText).ToArray());
        foreach (var identifier in identifiers)
        {
            var symbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetSymbolInfo(identifier).Symbol);
            Assert.Equal(identifier.Identifier.ValueText, symbol.Name);
        }

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void FunctionExpression_ItIsNotImplicitParameterAlias()
    {
        const string source = """
        val transform: int -> int = x => it + 1
        """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());

        Assert.Equal(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, diagnostic.Descriptor);
        Assert.Equal("'it' is not in scope.", diagnostic.GetMessage());
    }

    [Fact]
    public void TrailingBlock_BindsAsConstructorClosureArgument()
    {
        const string source = """
        class Window {
            init(content: () -> string) {
                Title = content()
            }

            var Title: string = ""
        }

        val window = Window {
            return "Main"
        }
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.TrailingBlock is not null);

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal(MethodKind.Constructor, symbol.MethodKind);
        Assert.Single(symbol.Parameters);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void TrailingBlock_WithoutClosureParameter_IsRejectedByOverloadResolution()
    {
        const string source = """
        val foo = Foo {
            return 42
        }

        class Foo {
            init() {}
        }
        """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());

        Assert.Equal(CompilerDiagnostics.NoOverloadForMethod, diagnostic.Descriptor);
        Assert.Contains("constructor for type 'Foo' takes 1 arguments", diagnostic.ToString(), StringComparison.Ordinal);
    }

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

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TypeAlreadyDefinesMember);
        Assert.All(diagnostics, diagnostic => Assert.Equal(CompilerDiagnostics.TypeAlreadyDefinesMember, diagnostic.Descriptor));
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
    public void ConcreteReceiver_DoesNotResolveImplementedInterfaceMethodOverConcreteMethod()
    {
        var source = """
        import System.Collections.Immutable.*

        record Person(val Name: string)

        func Test() -> ImmutableList<Person> {
            val people = [Person("Alice")]
            return people.Add(Person("Test"))
        }
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static i => i.Expression is MemberAccessExpressionSyntax
            {
                Name.Identifier.Text: "Add"
            });

        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetTypeInfo(invocation).Type);
        Assert.Equal("ImmutableList", type.Name);
        var typeArgument = Assert.Single(type.TypeArguments);
        Assert.Equal("Person", typeArgument.Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1014");
    }

    [Fact]
    public void ConcreteReceiver_CollectionLiteralArgument_IsRejectedForScalarAddParameter()
    {
        var source = """
        import System.Collections.Immutable.*

        record Person(val Name: string)

        func Test() -> ImmutableList<Person> {
            val people = [Person("Alice")]
            return people.Add([Person("Test")])
        }
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static i => i.Expression is MemberAccessExpressionSyntax
            {
                Name.Identifier.Text: "Add"
            });

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
        Assert.Contains(diagnostics, diagnostic => diagnostic.ToString().Contains("Cannot convert from 'ImmutableList<Person>' to 'Person'", StringComparison.Ordinal));
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.ToString().Contains("Cannot convert from 'collection expression' to 'Person'", StringComparison.Ordinal));
    }

    [Fact]
    public void InterfaceTypedReceiver_CanResolveExplicitInterfaceMember()
    {
        var source = """
        import System.Collections.*
        import System.Collections.Immutable.*

        record Person(val Name: string)

        func Test() -> int {
            val people = [Person("Alice")]
            val values: IList = people
            return values.Add(Person("Test"))
        }
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static i => i.Expression is MemberAccessExpressionSyntax
            {
                Name.Identifier.Text: "Add"
            });

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("IList", symbol.ContainingType?.Name);
        Assert.Equal("object?", symbol.Parameters[0].Type.Name);
        Assert.Equal(SpecialType.System_Int32, symbol.ReturnType.SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1014");
    }

    [Fact]
    public void ConcreteReceiver_DoesNotSeeSourceDefinedExplicitInterfaceMember()
    {
        var source = """
        interface ILogger {
            func Log(message: string) -> string
        }

        class QuietLogger : ILogger {
            func ILogger.Log(message: string) -> string {
                return "[quiet]"
            }
        }

        func Test() -> string {
            val logger = QuietLogger()
            return logger.Log("hi")
        }
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static i => i.Expression is MemberAccessExpressionSyntax
            {
                Name.Identifier.Text: "Log"
            });

        var info = model.GetSymbolInfo(invocation);
        Assert.Null(info.Symbol);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0117");
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
    public void PipelineSelect_OnOrderedQueryable_PrefersQueryableOverEnumerable()
    {
        var source = """
        import System.*
        import System.Linq.*
        import System.Collections.Generic.*
        import System.Linq.Expressions.*

        class C {
            func Project(source: IOrderedQueryable<int>) -> IQueryable<string> {
                return source |> Select(x => x.ToString())
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
        Assert.Empty(diagnostics);
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
