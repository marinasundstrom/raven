using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests.Scripting;

public sealed class ScriptCompilationTests : CompilationTestBase
{
    [Fact]
    public void CreateScriptCompilation_RecordsSubmissionChain()
    {
        var (first, _) = CreateSubmission("let value = 40", "submission0");
        var (second, _) = CreateSubmission("value + 2", "submission1", first);

        Assert.True(first.IsSubmission);
        Assert.Null(first.ScriptCompilationInfo!.PreviousScriptCompilation);
        Assert.Same(first, second.ScriptCompilationInfo!.PreviousScriptCompilation);
        Assert.Contains(
            second.References.OfType<CompilationReference>(),
            reference => ReferenceEquals(reference.Compilation, first));
    }

    [Fact]
    public void CreateScriptCompilation_RecordsEmittedPreviousSubmissionReference()
    {
        var (first, _) = CreateSubmission("let value = 40", "submission0");
        var emittedReference = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        var (second, _) = CreateSubmission(
            "value + 2",
            "submission1",
            first,
            emittedReference);

        Assert.Same(emittedReference, second.ScriptCompilationInfo!.PreviousScriptCompilationReference);
        Assert.Contains(emittedReference, second.References);
    }

    [Fact]
    public void CreateScriptCompilation_RejectsEmittedReferenceWithoutPreviousSubmission()
    {
        var tree = ParseSubmission("let value = 40");
        var emittedReference = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);

        var exception = Assert.Throws<ArgumentException>(() => Compilation.CreateScriptCompilation(
            "submission",
            tree,
            GetMetadataReferences(),
            new CompilationOptions(OutputKind.ConsoleApplication),
            previousScriptCompilationReference: emittedReference));

        Assert.Contains("previous script compilation", exception.Message, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void CreateScriptCompilation_RequiresSubmissionSyntaxTree()
    {
        var tree = SyntaxTree.ParseText("let value = 40");

        var exception = Assert.Throws<ArgumentException>(() => Compilation.CreateScriptCompilation(
            "submission",
            tree,
            GetMetadataReferences()));

        Assert.Contains("script or interactive", exception.Message, StringComparison.Ordinal);
    }

    [Fact]
    public void CreateScriptCompilation_RequiresPreviousScriptCompilation()
    {
        var regularTree = SyntaxTree.ParseText("let value = 40");
        var regularCompilation = CreateCompilation(regularTree, new CompilationOptions(OutputKind.ConsoleApplication));
        var scriptTree = ParseSubmission("value + 2");

        var exception = Assert.Throws<ArgumentException>(() => Compilation.CreateScriptCompilation(
            "submission",
            scriptTree,
            GetMetadataReferences(),
            new CompilationOptions(OutputKind.ConsoleApplication),
            regularCompilation));

        Assert.Contains("previous compilation", exception.Message, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void PreviousSubmissionVariable_BindsInNextSubmission()
    {
        var (first, firstTree) = CreateSubmission("let value = 40", "submission0");
        var (second, secondTree) = CreateSubmission("value + 2", "submission1", first);

        AssertNoErrors(first);
        AssertNoErrors(second);

        var declaredValue = Assert.IsAssignableFrom<ILocalSymbol>(first.GetSemanticModel(firstTree)
            .GetDeclaredSymbol(firstTree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single()));
        var valueReference = secondTree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "value");
        var referencedValue = second.GetSemanticModel(secondTree).GetSymbolInfo(valueReference).Symbol;

        var submissionVariable = Assert.IsType<SubmissionVariableSymbol>(referencedValue);
        Assert.Same(declaredValue, submissionVariable.OriginalVariable);
        Assert.Equal(SpecialType.System_Int32, submissionVariable.Type.SpecialType);
    }

    [Fact]
    public void PreviousSubmissionVariables_StayVisibleAcrossChain()
    {
        var (first, _) = CreateSubmission("let first = 40", "submission0");
        var (second, _) = CreateSubmission("let second = first + 1", "submission1", first);
        var (third, thirdTree) = CreateSubmission("first + second", "submission2", second);

        AssertNoErrors(third);

        var model = third.GetSemanticModel(thirdTree);
        var references = thirdTree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .ToDictionary(identifier => identifier.Identifier.ValueText);

        Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(references["first"]).Symbol);
        Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(references["second"]).Symbol);
    }

    [Fact]
    public void PreviousSubmissionFunction_BindsInNextSubmission()
    {
        var (first, firstTree) = CreateSubmission(
            "func twice(value: int) -> int => value * 2",
            "submission0");
        var (second, secondTree) = CreateSubmission("twice(21)", "submission1", first);

        AssertNoErrors(first);
        AssertNoErrors(second);

        var declaredFunction = Assert.IsAssignableFrom<IMethodSymbol>(first.GetSemanticModel(firstTree)
            .GetDeclaredSymbol(firstTree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single()));
        var invocation = secondTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var referencedFunction = second.GetSemanticModel(secondTree).GetSymbolInfo(invocation).Symbol;

        Assert.Same(declaredFunction, referencedFunction);
    }

    [Fact]
    public void PreviousSubmissionVariable_BindsInsideTopLevelFunction()
    {
        var (first, _) = CreateSubmission("let value = 42", "submission0");
        var (second, _) = CreateSubmission(
            "func getValue() -> int => value",
            "submission1",
            first);

        AssertNoErrors(second);
    }

    [Fact]
    public void PreviousSubmissionType_BindsThroughCompilationReference()
    {
        var (first, _) = CreateSubmission("class Widget {}", "submission0");
        var (second, secondTree) = CreateSubmission("let widget = Widget()", "submission1", first);

        AssertNoErrors(first);
        AssertNoErrors(second);

        var creation = secondTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var type = second.GetSemanticModel(secondTree).GetTypeInfo(creation).Type;

        Assert.Equal("Widget", Assert.IsAssignableFrom<INamedTypeSymbol>(type).Name);
        Assert.Same(first.Assembly, type.ContainingAssembly);
    }

    [Fact]
    public void PreviousSubmissionType_StaysVisibleAcrossChain()
    {
        var (first, _) = CreateSubmission("class Widget {}", "submission0");
        var (second, _) = CreateSubmission("let answer = 42", "submission1", first);
        var (third, thirdTree) = CreateSubmission("let widget = Widget()", "submission2", second);

        AssertNoErrors(third);

        var creation = thirdTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var type = third.GetSemanticModel(thirdTree).GetTypeInfo(creation).Type;

        Assert.Equal("Widget", Assert.IsAssignableFrom<INamedTypeSymbol>(type).Name);
        Assert.Contains(
            third.References.OfType<CompilationReference>(),
            reference => ReferenceEquals(reference.Compilation, first));
    }

    private (Compilation Compilation, SyntaxTree Tree) CreateSubmission(
        string source,
        string assemblyName,
        Compilation? previous = null,
        MetadataReference? previousReference = null)
    {
        var tree = ParseSubmission(source);
        var compilation = Compilation.CreateScriptCompilation(
            assemblyName,
            tree,
            GetMetadataReferences(),
            new CompilationOptions(OutputKind.ConsoleApplication),
            previous,
            previousReference);
        return (compilation, tree);
    }

    private static SyntaxTree ParseSubmission(string source)
        => SyntaxTree.ParseText(source, new ParseOptions { Kind = SourceCodeKind.Script });

    private static void AssertNoErrors(Compilation compilation)
        => Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
}
