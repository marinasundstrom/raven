using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class GenericMethodTests : CompilationTestBase
{
    [Fact]
    public void GenericMethod_ExposesTypeParametersAndArguments()
    {
        var source = """
            class Container
            {
                static func identity<T>(value: T) -> T
                {
                    return value;
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var methodDeclaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodDeclaration)!;

        Assert.True(methodSymbol.IsGenericMethod);
        Assert.Single(methodSymbol.TypeParameters);
        Assert.Equal("T", methodSymbol.TypeParameters[0].Name);
        Assert.Equal(TypeParameterOwnerKind.Method, methodSymbol.TypeParameters[0].OwnerKind);
        Assert.Same(methodSymbol, methodSymbol.TypeParameters[0].DeclaringMethodParameterOwner);
        Assert.Null(methodSymbol.TypeParameters[0].DeclaringTypeParameterOwner);
        Assert.Same(methodSymbol.TypeParameters[0], methodSymbol.TypeArguments[0]);
        Assert.Same(methodSymbol.TypeParameters[0], methodSymbol.ReturnType);
        Assert.Same(methodSymbol.TypeParameters[0], methodSymbol.Parameters[0].Type);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericMethodInvocation_WithExplicitTypeArguments_BindsConstructedMethod()
    {
        var source = """
            class Container
            {
                static func identity<T>(value: T) -> T
                {
                    return value;
                }

                static func call() -> int
                {
                    return identity<int>(1);
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        Assert.True(method.IsGenericMethod);
        Assert.Equal("identity", method.Name);
        Assert.Single(method.TypeArguments);
        Assert.Same(intType, method.TypeArguments[0]);
        Assert.Same(intType, method.ReturnType);
        Assert.Same(intType, method.Parameters[0].Type);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericMethodInvocation_NullArgument_DoesNotInferTypeArgument()
    {
        var source = """
            class C
            {
                static func f<T>(value: T) -> ()
                {
                }

                static func test() -> ()
                {
                    f(null);
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
    }

    [Fact]
    public void GenericMethodInvocation_GenericMethodGroup_InfersBothMethods()
    {
        const string source = """
            import System.*

            let result = Apply(21, Identity)

            func Apply<TInput, TResult>(value: TInput, transform: Func<TInput, TResult>) -> TResult {
                transform(value)
            }

            func Identity<T>(value: T) -> T {
                value
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(System.Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var invocations = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().ToArray();
        var applyInvocation = Assert.Single(invocations, invocation => invocation.Expression.ToString() == "Apply");
        var applyMethod = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(applyInvocation)).Method;

        Assert.Equal(new[] { SpecialType.System_Int32, SpecialType.System_Int32 },
            applyMethod.TypeArguments.Select(static type => type.SpecialType));

        var applySymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(applyInvocation).Symbol);
        Assert.Equal(new[] { SpecialType.System_Int32, SpecialType.System_Int32 },
            applySymbol.TypeArguments.Select(static type => type.SpecialType));

        var identityReference = tree.GetRoot().DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "Identity");
        var identityMethod = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(identityReference).Symbol);

        Assert.Equal(SpecialType.System_Int32, Assert.Single(identityMethod.TypeArguments).SpecialType);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GenericMethodGroup_WithSatisfiedConstraint_IsQueryOrderIndependent(bool diagnosticsFirst)
    {
        const string source = """
            import System.*

            let result = Apply(21, Stringify)

            func Apply<TInput, TResult>(value: TInput, transform: Func<TInput, TResult>) -> TResult {
                transform(value)
            }

            func Stringify<T: struct>(value: T) -> string {
                ""
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var applyInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression.ToString() == "Apply");
        var stringifyReference = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "Stringify");

        var apply = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(applyInvocation).Symbol);
        var stringify = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(stringifyReference).Symbol);

        Assert.Equal(
            [SpecialType.System_Int32, SpecialType.System_String],
            apply.TypeArguments.Select(static type => type.SpecialType));
        Assert.Equal(SpecialType.System_Int32, Assert.Single(stringify.TypeArguments).SpecialType);
        Assert.True((Assert.Single(stringify.TypeParameters).ConstraintKind & TypeParameterConstraintKind.ValueType) != 0);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericMethodGroup_WithUnsatisfiedConstraint_ReportsConstraintFailure()
    {
        const string source = """
            import System.*

            let result = Apply("value", Stringify)

            func Apply<TInput, TResult>(value: TInput, transform: Func<TInput, TResult>) -> TResult {
                transform(value)
            }

            func Stringify<T: struct>(value: T) -> string {
                ""
            }
            """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
        Assert.Contains("Stringify", diagnostic.GetMessage());
        Assert.Contains("struct", diagnostic.GetMessage());
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
    }

    [Fact]
    public void EditingMethodGroupArgument_RecomputesInferredConstraintsAndSymbolInfo()
    {
        const string validSource = """
            import System.*

            let result = Apply(21, Stringify)

            func Apply<TInput, TResult>(value: TInput, transform: Func<TInput, TResult>) -> TResult {
                transform(value)
            }

            func Stringify<T: struct>(value: T) -> string {
                ""
            }
            """;
        var invalidSource = validSource.Replace("Apply(21", "Apply(\"value\"", System.StringComparison.Ordinal);
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "generic-method-group-edit",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "method-group.rav",
            SourceText.From(validSource),
            "/tmp/generic-method-group-edit.rav").Project;
        workspace.TryApplyChanges(project.Solution);

        AssertValidSnapshot();

        var documentId = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single().Id;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(documentId, SourceText.From(invalidSource)));
        AssertInvalidSnapshot();

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(documentId, SourceText.From(validSource)));
        AssertValidSnapshot();

        void AssertValidSnapshot()
        {
            var compilation = workspace.GetCompilation(projectId);
            var tree = compilation.SyntaxTrees.Single();
            var model = compilation.GetSemanticModel(tree);
            var stringify = tree.GetRoot()
                .DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Single(identifier => identifier.Identifier.ValueText == "Stringify");
            var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(stringify).Symbol);

            var typeArgument = Assert.Single(method.TypeArguments);
            Assert.True(
                typeArgument.SpecialType == SpecialType.System_Int32,
                $"Expected int, got {typeArgument.ToDisplayString()} ({typeArgument.GetType().Name}) from {method.ToDisplayString()}");
            Assert.Empty(compilation.GetDiagnostics());
        }

        void AssertInvalidSnapshot()
        {
            var compilation = workspace.GetCompilation(projectId);
            var tree = compilation.SyntaxTrees.Single();
            var model = compilation.GetSemanticModel(tree);
            var stringify = tree.GetRoot()
                .DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Single(identifier => identifier.Identifier.ValueText == "Stringify");
            var symbolInfo = model.GetSymbolInfo(stringify);
            var diagnostic = Assert.Single(
                compilation.GetDiagnostics(),
                diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);

            var method = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
            Assert.IsAssignableFrom<ITypeParameterSymbol>(Assert.Single(method.TypeArguments));
            Assert.Contains("Stringify", diagnostic.GetMessage());
        }
    }

    [Fact]
    public void GenericMethodInvocation_ConstraintFailure_DoesNotReportNameMissing()
    {
        var source = """
            import System.*

            f2<bool?>(null)

            func f2<T>(t: T) -> ()
                where T: notnull {
                Console.WriteLine(t)
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
    }

    [Fact]
    public void GenericMethodInvocation_WhereClauseSelfConstraint_AllowsExplicitTypeArgument()
    {
        var source = """
            import System.*

            func Parse<T>(text: string) -> T
                where T: IParsable<T>
                => T.Parse(text, null)

            func Main() -> int
            {
                return Parse<int>("42");
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
        Assert.Empty(diagnostics);
    }

    [Fact]
    public void MetadataGenericMethod_ConstraintUsingConstructedContainingType_IsSatisfied()
    {
        const string source = """
            import Raven.MetadataFixtures.Generics.*

            let value = GenericContainer<object>.Coerce<string>("value")
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Empty(compilation.GetDiagnostics());

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(
            compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);
        var constraint = Assert.Single(Assert.Single(method.TypeParameters).ConstraintTypes);

        Assert.Equal(SpecialType.System_Object, constraint.SpecialType);
    }

    [Fact]
    public void MetadataGenericMethod_ConstraintUsingConstructedContainingType_IsRejected()
    {
        const string source = """
            import Raven.MetadataFixtures.Generics.*

            let value = GenericContainer<string>.Coerce<object>("value")
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbolInfo = compilation.GetSemanticModel(tree).GetSymbolInfo(invocation);

        Assert.Null(symbolInfo.Symbol);
        Assert.Equal(CandidateReason.OverloadResolutionFailure, symbolInfo.CandidateReason);
        var candidate = Assert.Single(symbolInfo.CandidateSymbols.OfType<IMethodSymbol>());
        Assert.Equal("Coerce", candidate.Name);
    }

    [Fact]
    public void GenericConstraintFailure_DoesNotCascadeToAmbiguousOverload()
    {
        var source = """
            import System.*
            import System.Console.*

            func Main() -> ()
            {
                let r = Parse<bool?>("42")
                WriteLine(r)
            }

            func Parse<T>(text: string) -> T
                where T: notnull
                => throw InvalidOperationException()
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
    }

    [Fact]
    public void ImplementedGenericInterface_ConstraintFailure_ReportsDiagnostic()
    {
        var source = """
            union class Response<T> {
                case Success(value: T)
                case Failure(message: string)
            }

            interface IRequestHandler<TRequest, TReturn>
                where TReturn : new() {
            }

            class SubmitOrderHandler : IRequestHandler<int, Response<decimal>> {
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
    }

    [Fact]
    public void InvocationThroughConstructedGenericInterface_AwaitsConstructedReturnType()
    {
        var source = """
import System.Threading.Tasks.*

let handler: IHandler<Request, Response<int>> = Handler()
let response = await handler.Handle(Request())

interface IHandler<TRequest, TResponse> {
    func Handle(request: TRequest) -> Task<TResponse>;
}

class Handler : IHandler<Request, Response<int>> {
    async func Handle(request: Request) -> Task<Response<int>> {
        await Task.CompletedTask
        return .Success(1)
    }
}

record class Request()

union Response<T> {
    case Success(value: T)
    case Failure(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression.ToString() == "handler.Handle");

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("Handle", symbol.Name);
        Assert.Equal("IHandler", symbol.ContainingType?.Name);
        Assert.Equal("Task<Response<int>>", symbol.ReturnType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var awaitExpression = tree.GetRoot()
            .DescendantNodes()
            .OfType<PrefixOperatorExpressionSyntax>()
            .Single(awaitExpression => awaitExpression.Expression == invocation);

        Assert.Equal("Response<int>", model.GetTypeInfo(awaitExpression).Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }
}
