using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NullableTypeTests : CompilationTestBase
{
    [Fact]
    public void NullableReferenceAndValueTypes_AreBound()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableInt = intType.WithNullableAnnotation(NullableAnnotation.Annotated);
        var nullableString = stringType.WithNullableAnnotation(NullableAnnotation.Annotated);
        Assert.Equal(TypeKind.Nullable, nullableInt.TypeKind);
        Assert.Equal(TypeKind.Nullable, nullableString.TypeKind);
        Assert.Equal(SpecialType.System_Int32, ((NullableTypeSymbol)nullableInt).UnderlyingType.SpecialType);
        Assert.Equal(SpecialType.System_String, ((NullableTypeSymbol)nullableString).UnderlyingType.SpecialType);
    }

    [Fact]
    public void MetadataNullableDefinition_ReportsNullableSpecialType()
    {
        var compilation = CreateCompilation();
        var nullableDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Nullable`1"));

        Assert.Equal(SpecialType.System_Nullable_T, nullableDefinition.SpecialType);
    }

    [Fact]
    public void ReferencedLibrary_NullabilityAnnotations_AreRead()
    {
        var compilation = CreateCompilation();

        compilation.EnsureSetup();
        var consoleType = (INamedTypeSymbol)compilation.GetType(typeof(Console))!;
        var readLine = consoleType.GetMembers("ReadLine").OfType<IMethodSymbol>().First(m => m.Parameters.Length == 0);

        Assert.IsType<NullableTypeSymbol>(readLine.ReturnType);
        var underlying = ((NullableTypeSymbol)readLine.ReturnType).UnderlyingType;
        Assert.Equal(SpecialType.System_String, underlying.SpecialType);
    }

    [Fact]
    public void ReferencedNullableReturnType_CannotImplicitlyConvertToNonNullableReference()
    {
        var source = """
        import System.*

        func Main() -> unit {
            let s: string = Console.ReadLine()
        }
        """;

        var (compilation, _) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor == CompilerDiagnostics.CannotAssignFromTypeToType));

        Assert.Equal("Cannot assign 'string?' to 'string'", diagnostic.GetMessage());
    }

    [Fact]
    public void NullableSyntax_BindsToNullableTypeSymbol()
    {
        var source = """
        let s: string? = null
        let i: int? = null
        """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();

        var sType = model.GetTypeInfo(declarators[0].TypeAnnotation!.Type).Type;
        var iType = model.GetTypeInfo(declarators[1].TypeAnnotation!.Type).Type;

        var nullableString = Assert.IsType<NullableTypeSymbol>(sType);
        Assert.Equal(SpecialType.System_String, nullableString.UnderlyingType.SpecialType);

        var nullableInt = Assert.IsType<NullableTypeSymbol>(iType);
        Assert.Equal(SpecialType.System_Int32, nullableInt.UnderlyingType.SpecialType);
    }

    [Fact]
    public void GetTypeInfo_ReportsFlowNarrowingInsideStrictNullCheck()
    {
        const string source = """
            func Length(value: string?) -> int {
                if value is not null {
                    return value.Length
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var checkedValue = tree.GetRoot()
            .DescendantNodes()
            .OfType<IsPatternExpressionSyntax>()
            .Single()
            .Expression;
        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        var checkedTypeInfo = model.GetTypeInfo(checkedValue);
        var typeInfo = model.GetTypeInfo(receiver);

        Assert.True(checkedTypeInfo.Type?.IsNullable);
        Assert.Equal(NullableAnnotation.Annotated, checkedTypeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.MaybeNull, checkedTypeInfo.Nullability.FlowState);
        Assert.True(typeInfo.Type?.IsNullable);
        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Equal(NullableAnnotation.Annotated, typeInfo.ConvertedNullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.ConvertedNullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void EditingNullableStandardUnionAlternativeInvalidatesFlowAndConvertedType()
    {
        const string source = """
            func Unwrap(value: (int | string)?) -> (int | string) {
                if value is null {
                    throw System.Exception()
                }

                return value
            }
            """;

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "incremental-nullable-standard-union",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.DefaultWithRavenCore)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "flow.rav",
            SourceText.From(source),
            "/tmp/nullable-standard-union-flow.rav").Project;
        workspace.TryApplyChanges(project.Solution);

        AssertSnapshot("int | string");

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single();
        var updatedSource = source.Replace("string", "bool", System.StringComparison.Ordinal);
        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(updatedSource)));

        AssertSnapshot("int | bool");

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(source)));

        AssertSnapshot("int | string");

        void AssertSnapshot(string expectedUnionDisplay)
        {
            var compilation = workspace.GetCompilation(projectId);
            var tree = compilation.SyntaxTrees.Single();
            var returnedValue = tree.GetRoot()
                .DescendantNodes()
                .OfType<ReturnStatementSyntax>()
                .Single()
                .Expression!;
            var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(returnedValue);

            Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
            Assert.Equal(expectedUnionDisplay, typeInfo.ConvertedType?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
            Assert.Empty(compilation.GetDiagnostics());
        }
    }

    [Fact]
    public void GetTypeInfo_ReportsFlowNarrowingAfterNullGuard_RegardlessOfQueryOrder()
    {
        const string source = """
            func Length(value: string?) -> int {
                if value is null {
                    return 0
                }

                return value.Length
            }
            """;

        TypeInfo GetReceiverTypeInfo(string source, bool collectDiagnosticsFirst)
        {
            var (compilation, tree) = CreateCompilation(source);
            if (collectDiagnosticsFirst)
                Assert.Empty(compilation.GetDiagnostics());

            var receiver = tree.GetRoot()
                .DescendantNodes()
                .OfType<MemberAccessExpressionSyntax>()
                .Single()
                .Expression;

            return compilation.GetSemanticModel(tree).GetTypeInfo(receiver);
        }

        var coldTypeInfo = GetReceiverTypeInfo(source, collectDiagnosticsFirst: false);
        var diagnosticsFirstTypeInfo = GetReceiverTypeInfo(source, collectDiagnosticsFirst: true);

        Assert.All([coldTypeInfo, diagnosticsFirstTypeInfo], typeInfo =>
        {
            Assert.True(typeInfo.Type?.IsNullable);
            Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
            Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        });
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullAfterBothBranchesGuardNull(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: string?, chooseFirst: bool) -> int {
                if chooseFirst {
                    if value is null {
                        return 0
                    }
                } else {
                    if value is null {
                        return 0
                    }
                }

                return value.Length
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_RemainsMaybeNullWhenOnlyOneBranchGuardsNull(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: string?, guardValue: bool) -> int {
                if guardValue {
                    if value is null {
                        return 0
                    }
                } else {
                    let ignored = 0
                }

                return value.Length
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_NarrowsAfterNullGuardWhoseBlockEndsAbruptly(bool diagnosticsFirst)
    {
        const string source = """
            import System.Console.*

            func Length(value: string?) -> int {
                if value is null {
                    WriteLine("missing")
                    return 0
                }

                return value.Length
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_NarrowsAfterNullGuardWhoseNestedBranchesExit(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: string?, alternate: bool) -> int {
                if value is null {
                    if alternate {
                        return 0
                    } else {
                        throw System.Exception()
                    }
                }

                return value.Length
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(memberAccess => memberAccess.Expression.ToString() == "value")
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullInsideWhileNullCheck(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: string?) -> int {
                while value is not null {
                    return value.Length
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullAfterWhileNullGuardWithoutEarlyLoopExit(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: string?) -> int {
                while value is null {
                    return 0
                }

                return value.Length
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_RemainsMaybeNullAfterWhileNullGuardWithBreak(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: string?) -> int {
                while value is null {
                    break
                }

                return value.Length
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_NestedLoopBreakDoesNotSuppressOuterWhileExitNarrowing(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: string?) -> int {
                while value is null {
                    loop {
                        break
                    }

                    return 0
                }

                return value.Length
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void EditingWhileNullCheckInvalidatesBodyFlowState()
    {
        const string source = """
            func Length(value: string?) -> int {
                while value is not null {
                    return value.Length
                }

                return 0
            }
            """;

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "incremental-while-nullability",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "flow.rav",
            SourceText.From(source),
            "/tmp/flow.rav").Project;
        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single();
        var initialReceiver = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.NotNull,
            initialCompilation.GetSemanticModel(initialTree).GetTypeInfo(initialReceiver).Nullability.FlowState);
        Assert.Empty(initialCompilation.GetDiagnostics());

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single();
        var updatedSource = source.Replace("is not null", "is null", System.StringComparison.Ordinal);
        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(updatedSource)));

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single();
        var updatedReceiver = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.MaybeNull,
            updatedCompilation.GetSemanticModel(updatedTree).GetTypeInfo(updatedReceiver).Nullability.FlowState);
        Assert.Contains(
            updatedCompilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(source)));

        var restoredCompilation = workspace.GetCompilation(projectId);
        var restoredTree = restoredCompilation.SyntaxTrees.Single();
        var restoredReceiver = restoredTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.NotNull,
            restoredCompilation.GetSemanticModel(restoredTree).GetTypeInfo(restoredReceiver).Nullability.FlowState);
        Assert.DoesNotContain(
            restoredCompilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullForScrutineeInsideWhileTypedPattern(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: object?) -> int {
                while let text: string = value {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .First()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullForScrutineeInsideIfTypedPattern(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: object?) -> int {
                if value is string text {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .First()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullAfterNegatedTypedPatternGuard(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is not string text {
                    return 0
                }

                return value.GetHashCode()
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_RemainsMaybeNullInsideNegatedTypedPattern(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is not string text {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Contains(
                compilation.GetDiagnostics(),
                diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullInsideDoubleNegatedTypedPattern(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is not not string text {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullInsidePropertyPattern(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is { } {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullAfterNegatedPropertyPatternGuard(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is not { } {
                    return 0
                }

                return value.GetHashCode()
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullInsideDeconstructionPattern(bool diagnosticsFirst)
    {
        const string source = """
            record class Person(Name: string, Age: int)

            func Hash(value: object?) -> int {
                if value is Person(let name, let age) {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullAfterNegatedDeconstructionPatternGuard(bool diagnosticsFirst)
    {
        const string source = """
            record class Person(Name: string, Age: int)

            func Hash(value: object?) -> int {
                if value is not Person(let name, let age) {
                    return 0
                }

                return value.GetHashCode()
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_PreservesKnownNotNullAfterTypedPatternMismatch(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is null {
                    return 0
                }

                if value is string text {
                    return 1
                }
                else {
                    return value.GetHashCode()
                }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_RemainsMaybeNullInsideNegatedRuntimeValuePattern(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?, other: object?) -> int {
                if value is not other {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Contains(
                compilation.GetDiagnostics(),
                diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullInsideConjunctivePattern(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is not null and string text {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullInsideDisjunctiveNonNullPattern(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is string text or { } {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_RemainsMaybeNullInsideMixedDisjunctivePattern(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is null or string text {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Contains(
                compilation.GetDiagnostics(),
                diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullInsideSequencePattern(bool diagnosticsFirst)
    {
        const string source = """
            func Hash(value: int[]?) -> int {
                if value is [let first, ...] {
                    return value.Length
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNotNullInsideDictionaryPattern(bool diagnosticsFirst)
    {
        const string source = """
            import System.Collections.Generic.*

            func Count(value: Dictionary<string, int>?) -> int {
                if value is ["answer": let answer] {
                    return value.Count
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void EditingTypedPatternNegationInvalidatesBodyFlowState()
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is string text {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "incremental-typed-pattern-nullability",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "flow.rav",
            SourceText.From(source),
            "/tmp/flow.rav").Project;
        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single();
        var initialReceiver = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.NotNull,
            initialCompilation.GetSemanticModel(initialTree).GetTypeInfo(initialReceiver).Nullability.FlowState);
        Assert.Empty(initialCompilation.GetDiagnostics());

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single();
        var updatedSource = source.Replace("is string", "is not string", System.StringComparison.Ordinal);
        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(updatedSource)));

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single();
        var updatedReceiver = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.MaybeNull,
            updatedCompilation.GetSemanticModel(updatedTree).GetTypeInfo(updatedReceiver).Nullability.FlowState);
        Assert.Contains(
            updatedCompilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(source)));

        var restoredCompilation = workspace.GetCompilation(projectId);
        var restoredTree = restoredCompilation.SyntaxTrees.Single();
        var restoredReceiver = restoredTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.NotNull,
            restoredCompilation.GetSemanticModel(restoredTree).GetTypeInfo(restoredReceiver).Nullability.FlowState);
        Assert.Empty(restoredCompilation.GetDiagnostics());
    }

    [Fact]
    public void EditingCompoundPatternInvalidatesAndRestoresFlowState()
    {
        const string source = """
            func Hash(value: object?) -> int {
                if value is not null and string text {
                    return value.GetHashCode()
                }

                return 0
            }
            """;

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "incremental-compound-pattern-nullability",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "flow.rav",
            SourceText.From(source),
            "/tmp/flow.rav").Project;
        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single();
        var initialReceiver = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.NotNull,
            initialCompilation.GetSemanticModel(initialTree).GetTypeInfo(initialReceiver).Nullability.FlowState);
        Assert.Empty(initialCompilation.GetDiagnostics());

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single();
        var updatedSource = source.Replace(
            "not null and string",
            "null or string",
            System.StringComparison.Ordinal);
        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(updatedSource)));

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single();
        var updatedReceiver = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.MaybeNull,
            updatedCompilation.GetSemanticModel(updatedTree).GetTypeInfo(updatedReceiver).Nullability.FlowState);
        Assert.Contains(
            updatedCompilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(source)));

        var restoredCompilation = workspace.GetCompilation(projectId);
        var restoredTree = restoredCompilation.SyntaxTrees.Single();
        var restoredReceiver = restoredTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.Equal(
            NullableFlowState.NotNull,
            restoredCompilation.GetSemanticModel(restoredTree).GetTypeInfo(restoredReceiver).Nullability.FlowState);
        Assert.Empty(restoredCompilation.GetDiagnostics());
    }

    [Fact]
    public void GetTypeInfo_ReportsNullableValueTypeFlowNarrowing()
    {
        const string source = """
            func Value(value: int?) -> int {
                if value is not null {
                    return value
                }

                return 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var returnedValue = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .First()
            .Expression!;

        var typeInfo = model.GetTypeInfo(returnedValue);

        Assert.True(typeInfo.Type?.IsNullable);
        Assert.Equal(SpecialType.System_Int32, typeInfo.Type.GetNonNullableType().SpecialType);
        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData("string", "\"\"", SpecialType.System_String, false)]
    [InlineData("string", "\"\"", SpecialType.System_String, true)]
    [InlineData("int", "0", SpecialType.System_Int32, false)]
    [InlineData("int", "0", SpecialType.System_Int32, true)]
    public void GetTypeInfo_PreservesDeclaredNullabilityAndReportsReturnConversionForReferenceAndValueTypes(
        string typeName,
        string fallbackValue,
        SpecialType expectedUnderlyingType,
        bool diagnosticsFirst)
    {
        var source = $$"""
            func Value(value: {{typeName}}?) -> {{typeName}} {
                if value is not null {
                    return value
                }

                return {{fallbackValue}}
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var returnedValue = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .First()
            .Expression!;

        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var semanticModel = compilation.GetSemanticModel(tree);
        var typeInfo = semanticModel.GetTypeInfo(returnedValue);

        Assert.Equal(TypeKind.Nullable, typeInfo.Type?.TypeKind);
        Assert.Equal(expectedUnderlyingType, typeInfo.Type?.GetNonNullableType().SpecialType);
        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Equal(expectedUnderlyingType, typeInfo.ConvertedType?.SpecialType);
        Assert.False(typeInfo.ConvertedType?.IsNullable);
        Assert.Equal(NullableAnnotation.NotAnnotated, typeInfo.ConvertedNullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.ConvertedNullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());

        var repeatedTypeInfo = semanticModel.GetTypeInfo(returnedValue);
        Assert.Equal(typeInfo.Type, repeatedTypeInfo.Type, SymbolEqualityComparer.Default);
        Assert.Equal(typeInfo.Nullability, repeatedTypeInfo.Nullability);
        Assert.Equal(typeInfo.ConvertedNullability, repeatedTypeInfo.ConvertedNullability);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsNullableStandardUnionFlowNarrowing(bool diagnosticsFirst)
    {
        const string source = """
            func Unwrap(value: (int | string)?) -> (int | string) {
                if value is null {
                    throw System.Exception()
                }

                return value
            }
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithRavenCore);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var returnedValue = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .First()
            .Expression!;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(returnedValue);

        Assert.True(typeInfo.Type?.IsNullable);
        Assert.False(typeInfo.Type?.GetNonNullableType().IsNullable);
        Assert.NotEqual(TypeKind.Error, typeInfo.Type?.GetNonNullableType().TypeKind);
        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ExplicitNullableGenericSyntax_BindsToNamedNullableType()
    {
        var source = """
        import System.*

        let i: Nullable<int> = null
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var nullableInt = Assert.IsAssignableFrom<INamedTypeSymbol>(type);
        Assert.Equal(SpecialType.System_Nullable_T, nullableInt.SpecialType);
        Assert.Equal(SpecialType.System_Int32, nullableInt.TypeArguments.Single().SpecialType);
    }

    [Fact]
    public void NullableShorthandAndExplicitNullableGeneric_AreInteroperable()
    {
        var source = """
        import System.*

        let a: int? = 1
        let b: Nullable<int> = a
        let c: int? = b
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();

        var aSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(x => x.Identifier.ValueText == "a")));
        var bSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(x => x.Identifier.ValueText == "b")));
        var cSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(x => x.Identifier.ValueText == "c")));

        Assert.False(SymbolEqualityComparer.Default.Equals(aSymbol.Type, bSymbol.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(aSymbol.Type, cSymbol.Type));
    }

    [Fact]
    public void ExplicitNullableGeneric_AllowsNullableMembers()
    {
        var source = """
        import System.*

        let n: Nullable<int> = 1
        let hasValue = n.HasValue
        let value = n.GetValueOrDefault()
        """;

        var (compilation, _) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData("class Box<T : struct> { val value: T? = null }", TypeParameterConstraintKind.ValueType)]
    [InlineData("class Box<T : class> { val value: T? = null }", TypeParameterConstraintKind.ReferenceType)]
    [InlineData("class Box<T> { val value: T? = null }", TypeParameterConstraintKind.None)]
    public void NullableTypeSyntax_WrapsTypeParameters_WithConstraints(string source, TypeParameterConstraintKind expectedConstraint)
    {
        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();

        var type = model.GetTypeInfo(declarator.Type.Type).Type;

        var nullable = Assert.IsType<NullableTypeSymbol>(type);
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(nullable.UnderlyingType);
        Assert.Equal(expectedConstraint, typeParameter.ConstraintKind);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NullableTypeSyntax_NotNullTypeParameter_IsAllowed()
    {
        var source = "class Box<T : notnull> { val value: T? = null }";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);
    }

    [Fact]
    public void NullableTypeSymbol_LookupType_DoesNotThrow()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullable = intType.WithNullableAnnotation(NullableAnnotation.Annotated);

        var exception = Record.Exception(() => nullable.LookupType("DoesNotExist"));
        Assert.Null(exception);

        Assert.Null(nullable.LookupType("DoesNotExist"));
        Assert.False(nullable.IsMemberDefined("DoesNotExist", out _));
    }

    [Fact]
    public void NullableDelegateInvocation_ReportsError()
    {
        var source = """
import System.*

class Foo {
    func Run() -> unit {
        let f: Action<int>? = null
        f(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess &&
                          diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void NullableDelegateConditionalInvocation_SuppressesWarning()
    {
        var source = """
import System.*

class Foo {
    func Run() -> unit {
        let f: Action<int>? = null
        f?(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableFunctionTypeConditionalInvocation_DoesNotReportInvalidInvocation()
    {
        var source = """
class Foo {
    func Run() -> unit {
        let f: (() -> ())? = null
        f?()
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.InvalidInvocation);
    }

    [Fact]
    public void NullableFunctionTypeFieldConditionalInvocation_DoesNotReportInvalidInvocation()
    {
        var source = """
class Foo {
    private val f: (() -> ())?

    init(f: (() -> ())?) {
        self.f = f
    }

    func Run() -> unit {
        f?()
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.InvalidInvocation);
    }

    [Fact]
    public void NullableFunctionTypeFieldConditionalInvocationInsideLambda_DoesNotReportInvalidInvocation()
    {
        var source = """
class Foo {
    private val f: (() -> ())?

    init(f: (() -> ())?) {
        self.f = f
    }

    func Use(callback: () -> ()) -> unit {
        callback()
    }

    func Run() -> unit {
        Use(func () => {
            f?()
        })
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.InvalidInvocation);
    }

    [Fact]
    public void NullableFunctionTypeConditionalInvocation_SemanticQueriesDoNotPoisonDiagnostics()
    {
        var source = """
class Foo {
    private val f: (() -> ())?

    init(f: (() -> ())?) {
        self.f = f
    }

    func Use(callback: () -> ()) -> unit {
        callback()
    }

    func Run() -> unit {
        f?()
        Use(func () => {
            f?()
        })
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        foreach (var receiverBinding in root.DescendantNodes().OfType<ReceiverBindingExpressionSyntax>())
        {
            _ = model.GetSymbolInfo(receiverBinding);
            _ = model.GetOperation(receiverBinding);
        }

        var diagnostics = model.GetDocumentDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.InvalidInvocation);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterNullCheck_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    func Run() -> unit {
        let f: Action<int>? = null
        if f is not null {
            f(2)
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterIsNotNull_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    func Run() -> unit {
        let f: Action<int>? = null
        if f is not null {
            f(2)
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterIsNotNull_WithParens_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    func Run() -> unit {
        let f: Action<int>? = null
        if (f is not null) {
            f.Invoke(2)
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterGuardReturn_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    func Run() -> unit {
        let f: Action<int>? = null
        if f is null {
            return
        }
        f(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterIsNullGuard_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    func Run() -> unit {
        let f: Action<int>? = null
        if f is null {
            return
        }
        f(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableArgument_AfterIsNullGuard_ConvertsToNonNullableParameter()
    {
        var source = """
import System.*

class Foo {
    func Accept(value: string) -> unit {
    }

    func Run(candidate: string?) -> unit {
        if candidate is null {
            return
        }

        Accept(candidate)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
    }

    [Fact]
    public void AsCastArgument_AfterIsNullGuard_ConvertsToNonNullableParameter()
    {
        var source = """
import System.*

class Foo {
    func Accept(value: string) -> unit {
    }

    func Run(input: object) -> unit {
        let candidate = input as string
        if candidate is null {
            return
        }

        Accept(candidate)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterNotEqualsNullGuard_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    func Run() -> unit {
        let f: Action<int>? = null
        if f != null {
            f(2)
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

    [Fact]
    public void NonNullable_To_Nullable_Conversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableInt = intType.WithNullableAnnotation(NullableAnnotation.Annotated);
        var nullableString = stringType.WithNullableAnnotation(NullableAnnotation.Annotated);

        var intConv = compilation.ClassifyConversion(intType, nullableInt);
        Assert.True(intConv.IsImplicit);
        Assert.False(intConv.IsIdentity);

        var stringConv = compilation.ClassifyConversion(stringType, nullableString);
        Assert.True(stringConv.IsImplicit);
        Assert.True(stringConv.IsIdentity);
    }

    [Fact]
    public void NullableReference_To_NonNullableReference_Conversion_IsNotImplicit()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableString = stringType.WithNullableAnnotation(NullableAnnotation.Annotated);
        var reverse = compilation.ClassifyConversion(nullableString, stringType);

        Assert.False(reverse.Exists && reverse.IsImplicit);
    }

    [Fact]
    public void ExplicitNullableGeneric_AndNullableSyntax_UseSameConversionRules()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableSyntaxType = intType.WithNullableAnnotation(NullableAnnotation.Annotated);
        var nullableDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Nullable`1"));
        var explicitNullableType = nullableDefinition.Construct([intType]);

        var fromSyntaxToExplicit = compilation.ClassifyConversion(nullableSyntaxType, explicitNullableType);
        Assert.True(fromSyntaxToExplicit.Exists);
        Assert.True(fromSyntaxToExplicit.IsImplicit);

        var fromExplicitToSyntax = compilation.ClassifyConversion(explicitNullableType, nullableSyntaxType);
        Assert.True(fromExplicitToSyntax.Exists);
        Assert.True(fromExplicitToSyntax.IsImplicit);

        var fromValueToExplicit = compilation.ClassifyConversion(intType, explicitNullableType);
        Assert.True(fromValueToExplicit.Exists);
        Assert.True(fromValueToExplicit.IsImplicit);

        var fromExplicitToValue = compilation.ClassifyConversion(explicitNullableType, intType);
        Assert.True(fromExplicitToValue.Exists);
        Assert.False(fromExplicitToValue.IsImplicit);
    }

    [Fact]
    public void NullLiteral_To_Object_Conversion_IsNotImplicit()
    {
        var compilation = CreateCompilation();
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var nullType = compilation.NullTypeSymbol;

        var conversion = compilation.ClassifyConversion(nullType, objectType);

        Assert.False(conversion.IsImplicit);
    }

    [Fact]
    public void NullLiteral_To_NullableReference_Conversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableString = stringType.WithNullableAnnotation(NullableAnnotation.Annotated);

        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, nullableString);

        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.Exists);
    }

    [Fact]
    public void NullLiteral_To_NullableValue_Conversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = intType.WithNullableAnnotation(NullableAnnotation.Annotated);

        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, nullableInt);

        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.Exists);
    }

    [Fact]
    public void ObjectVariable_AssignedNull_RequiresNullable()
    {
        var (compilation, _) = CreateCompilation(
            "let x: object = null",
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(x => x.Descriptor == CompilerDiagnostics.CannotAssignNullToType));
        Assert.Equal("Cannot assign null to 'object'", diagnostic.GetMessage());
    }

    [Fact]
    public void PropertyInitializer_AssignedNullToNonNullableReference_ReportsDiagnostic()
    {
        const string source = """
class Foo {
    var Name: string = null
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void PropertyInitializer_AssignedNullToNullableReference_IsAllowed()
    {
        const string source = """
class Foo {
    var Name: string? = null
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void OverloadResolution_Prefers_NonNullable_WhenAvailable()
    {
        var source = """
        func f(x: string) -> int { 0 }
        func f2(x: string?) -> int { 1 }
        let s: string = ""
        let n: string? = null
        let a = f(s)
        let b = f2(n)
        let c = f2(null)
        """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocations = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().ToArray();

        var aSymbol = (IMethodSymbol)model.GetSymbolInfo(invocations[0]).Symbol!;
        Assert.Equal(SpecialType.System_String, aSymbol.Parameters[0].Type.SpecialType);

        var bSymbol = (IMethodSymbol)model.GetSymbolInfo(invocations[1]).Symbol!;
        Assert.IsType<NullableTypeSymbol>(bSymbol.Parameters[0].Type);

        var cSymbol = (IMethodSymbol)model.GetSymbolInfo(invocations[2]).Symbol!;
        Assert.IsType<NullableTypeSymbol>(cSymbol.Parameters[0].Type);
    }

    [Fact]
    public void ConsoleWriteLine_WithStringLiteral_Chooses_StringOverload()
    {
        var (compilation, tree) = CreateCompilation(
            "System.Console.WriteLine(\"Foo\")",
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;

        var param = Assert.IsType<NullableTypeSymbol>(symbol.Parameters[0].Type);
        Assert.Equal(SpecialType.System_String, param.UnderlyingType.SpecialType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ConsoleWriteLine_WithNullLiteral_IsAmbiguous()
    {
        var (compilation, tree) = CreateCompilation(
            "System.Console.WriteLine(null)",
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbolInfo = model.GetSymbolInfo(invocation);

        Assert.Null(symbolInfo.Symbol);
        Assert.Equal(CandidateReason.Ambiguous, symbolInfo.CandidateReason);
        Assert.True(symbolInfo.CandidateSymbols.Length > 1);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
    }

    [Fact]
    public void ConsoleWriteLine_WithNullableLocal_Chooses_StringOverload()
    {
        const string source = """
            let value: string? = null
            System.Console.WriteLine(value)
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;

        var param = Assert.IsType<NullableTypeSymbol>(symbol.Parameters[0].Type);
        Assert.Equal(SpecialType.System_String, param.UnderlyingType.SpecialType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NullableString_EqualityComparisonWithStringLiteral_IsAllowed()
    {
        const string source = """
            let x: string? = null

            if x == "" { }
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var binary = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        var typeInfo = model.GetTypeInfo(binary);

        Assert.Equal(SpecialType.System_Boolean, typeInfo.Type?.SpecialType);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

    [Fact]
    public void NullableInt_EqualityComparisonWithIntLiteral_IsAllowed()
    {
        const string source = """
            let x: int? = null

            if x == 1 { }
            if x != 1 { }
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var binaries = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().ToArray();

        Assert.Equal(2, binaries.Length);
        Assert.All(binaries, binary => Assert.Equal(SpecialType.System_Boolean, model.GetTypeInfo(binary).Type?.SpecialType));
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

    [Fact]
    public void NullableUserType_EqualityComparisonWithValue_IsAllowed()
    {
        const string source = """
            class Foo {}

            let x: Foo? = null
            let foo = Foo()

            if x == foo { }
            if x != foo { }
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var binaries = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().ToArray();

        Assert.Equal(2, binaries.Length);
        Assert.All(binaries, binary => Assert.Equal(SpecialType.System_Boolean, model.GetTypeInfo(binary).Type?.SpecialType));
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

}
