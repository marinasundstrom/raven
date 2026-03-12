using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ParamsParameterSemanticTests : CompilationTestBase
{
    [Fact]
    public void Parser_RecognizesDotDotDotInParameterAndArgument()
    {
        const string source = """
func Collect(items: int[] ...) -> int {
    return 0
}

val xs = [1, 2, 3]
val count = Collect(...xs)
""";

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var function = root.DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var parameter = Assert.Single(function.ParameterList.Parameters);
        Assert.Equal(SyntaxKind.DotDotDotToken, parameter.DotDotDotToken.Kind);

        var invocation = root.DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var argument = Assert.Single(invocation.ArgumentList.Arguments);
        Assert.Equal(SyntaxKind.DotDotDotToken, argument.DotDotDotToken.Kind);

    }

    [Fact]
    public void Parser_VarArgsMarkerAppearsAfterTypeAnnotation()
    {
        const string source = """
func Collect<T>(items: T ...) -> int {
    return 0
}
""";

        var tree = SyntaxTree.ParseText(source);
        var parameter = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .ParameterList
            .Parameters
            .Single();

        Assert.Equal(SyntaxKind.DotDotDotToken, parameter.DotDotDotToken.Kind);
        Assert.NotNull(parameter.TypeAnnotation);
        Assert.True(parameter.DotDotDotToken.SpanStart > parameter.Identifier.Span.End);
        Assert.True(parameter.DotDotDotToken.SpanStart > parameter.TypeAnnotation!.Span.End);
    }

    [Fact]
    public void Invocation_WithParamsCollector_PacksExtraPositionalArguments()
    {
        const string source = """
func Collect(items: int ...) -> int {
    return 0
}

val count = Collect(1, 2, 3)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();
        var collection = Assert.IsType<BoundCollectionExpression>(Assert.Single(arguments));
        var elements = collection.Elements.ToArray();

        Assert.Equal(3, elements.Length);
        Assert.All(elements, element => Assert.IsType<BoundLiteralExpression>(element));
        Assert.True(boundInvocation.Method.Parameters[0].IsVarParams);
        Assert.Equal("IList`1", ((INamedTypeSymbol)boundInvocation.Method.Parameters[0].Type).MetadataName);
    }

    [Fact]
    public void Invocation_WithSpreadArgument_ExpandsIntoParamsCollector()
    {
        const string source = """
func Collect(items: int ...) -> int {
    return 0
}

val xs = [1, 2, 3]
val count = Collect(...xs)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var argument = Assert.IsType<BoundCollectionExpression>(Assert.Single(boundInvocation.Arguments));
        var element = Assert.Single(argument.Elements);

        Assert.IsType<BoundSpreadElement>(element);
        Assert.True(boundInvocation.Method.Parameters[0].IsVarParams);
    }

    [Fact]
    public void OverloadResolution_PrefersFixedArityOverVarArgs()
    {
        const string source = """
class C {
    static func Pick(value: int) -> int {
        return 1
    }

    static func Pick(values: int ...) -> int {
        return 2
    }

    static func Use() -> int {
        return Pick(42)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        Assert.True(compilation.GetDiagnostics().IsEmpty);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is IdentifierNameSyntax { Identifier.Text: "Pick" });
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.False(boundInvocation.Method.Parameters[0].IsVarParams);
        Assert.Equal("Pick", boundInvocation.Method.Name);
    }

    [Fact]
    public void Parser_RecognizesParamsKeywordInParameter()
    {
        const string source = """
func Collect(params items: int[]) -> int {
    return 0
}
""";

        var tree = SyntaxTree.ParseText(source);
        var function = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(f => f.Identifier.ValueText == "Collect");
        var parameter = Assert.Single(function.ParameterList.Parameters);

        Assert.Equal(SyntaxKind.ParamsKeyword, parameter.ParamsKeyword.Kind);
        Assert.Equal(SyntaxKind.None, parameter.DotDotDotToken.Kind);
    }

    [Fact]
    public void Params_WithDotDotDot_ReportsDiagnostic()
    {
        const string source = """
func Collect(params items: int[] ...) -> int {
    return 0
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0923");
    }

    [Fact]
    public void Params_Syntax_CanKeepExplicitArrayType()
    {
        const string source = """
func Collect(params items: int[]) -> int {
    val count = items.Length
    return count
}

func Main() -> int {
    return Collect(1, 2, 3)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var function = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(f => f.Identifier.ValueText == "Collect");
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(function));
        var parameter = Assert.Single(symbol.Parameters);
        Assert.True(parameter.IsVarParams);
        Assert.IsAssignableFrom<IArrayTypeSymbol>(parameter.Type);
    }

    [Fact]
    public void VarArgs_SingleArrayArgument_BindsAsNormalForm()
    {
        const string source = """
func Collect(items: int[] ...) -> int {
    return 0
}

val xs = [1, 2, 3]
val count = Collect(xs)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        Assert.True(compilation.GetDiagnostics().IsEmpty);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        // Normal form binds the single array argument directly (not as expanded elements).
        var argument = Assert.Single(boundInvocation.Arguments);
        Assert.IsNotType<BoundCollectionExpression>(argument);
        Assert.True(boundInvocation.Method.Parameters[0].IsVarParams);
    }

    [Fact]
    public void GenericVarArgs_SingleArrayArgument_InfersTypeArgumentFromNormalForm()
    {
        const string source = """
func Collect<T>(items: T ...) -> int {
    return 0
}

val xs: int[] = [1, 2, 3]
val count = Collect(xs)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        Assert.True(compilation.GetDiagnostics().IsEmpty);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("Collect", boundInvocation.Method.Name);
        Assert.Single(boundInvocation.Method.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, boundInvocation.Method.TypeArguments[0].SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1501");
    }

    [Fact]
    public void GenericVarArgs_ExpandedArguments_InfersTypeArgumentFromElements()
    {
        const string source = """
func Collect<T>(items: T ...) -> int {
    return 0
}

val count = Collect(1, 2, 3)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        Assert.True(compilation.GetDiagnostics().IsEmpty);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("Collect", boundInvocation.Method.Name);
        Assert.Single(boundInvocation.Method.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, boundInvocation.Method.TypeArguments[0].SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1501");
    }
}
