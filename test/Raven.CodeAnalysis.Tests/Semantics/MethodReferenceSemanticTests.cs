using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MethodReferenceSemanticTests : CompilationTestBase
{
    [Fact]
    public void MethodGroupWithoutTargetContext_ReportsCandidates()
    {
        const string source = """
class Logger {
    static Log(value: string) -> unit {}
    static Log(value: int) -> unit {}
}

Logger.Log
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Logger.Log");

        var bound = model.GetBoundNode(memberAccess);
        Assert.IsType<BoundMethodGroupExpression>(bound);

        var methodGroup = (BoundMethodGroupExpression)bound;
        Assert.Null(methodGroup.SelectedMethod);
        Assert.Equal(2, methodGroup.Methods.Length);

        var info = model.GetSymbolInfo(memberAccess);

        Assert.Null(info.Symbol);
        Assert.Equal(CandidateReason.OverloadResolutionFailure, info.CandidateReason);

        var candidates = info.CandidateSymbols
            .OfType<IMethodSymbol>()
            .Select(FormatSignature)
            .OrderBy(value => value)
            .ToArray();

        Assert.Equal(new[] { "Logger.Log(Int32)", "Logger.Log(String)" }, candidates);
    }

    [Fact]
    public void MethodGroupWithExplicitDelegate_ReturnsSelectedMethod()
    {
        const string source = """
import System.*

class Logger {
    static Log(value: string) -> unit {}
    static Log(value: int) -> unit {}
}

val callback: System.Action<string> = Logger.Log
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Logger.Log");

        var bound = model.GetBoundNode(memberAccess);
        Assert.IsType<BoundMethodGroupExpression>(bound);

        var methodGroup = (BoundMethodGroupExpression)bound;
        Assert.Equal(2, methodGroup.Methods.Length);
        Assert.Null(methodGroup.SelectedMethod);

        var info = model.GetSymbolInfo(memberAccess);

        Assert.Equal(CandidateReason.MemberGroup, info.CandidateReason);
        Assert.Null(info.Symbol);

        var candidates = info.CandidateSymbols
            .OfType<IMethodSymbol>()
            .Select(FormatSignature)
            .OrderBy(value => value)
            .ToArray();

        Assert.Equal(new[] { "Logger.Log(Int32)", "Logger.Log(String)" }, candidates);
    }

    [Fact]
    public void MethodGroupWithSingleCandidate_InferredDelegateReturnsMethod()
    {
        const string source = """
class Calculator {
    static Add(x: int, y: int) -> int { x + y }
}

val add = Calculator.Add
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Calculator.Add");

        var bound = model.GetBoundNode(memberAccess);
        Assert.IsType<BoundDelegateCreationExpression>(bound);

        var delegateCreation = (BoundDelegateCreationExpression)bound;
        Assert.NotNull(delegateCreation.Method);
        Assert.Equal("Calculator.Add(Int32, Int32)", FormatSignature(delegateCreation.Method!));

        var info = model.GetSymbolInfo(memberAccess);

        Assert.Equal(CandidateReason.None, info.CandidateReason);
        Assert.NotNull(info.Symbol);
        Assert.Equal("Calculator.Add(Int32, Int32)", FormatSignature((IMethodSymbol)info.Symbol!));

        var candidate = Assert.Single(info.CandidateSymbols.OfType<IMethodSymbol>());
        Assert.Equal("Calculator.Add(Int32, Int32)", FormatSignature(candidate));
    }

    private static string FormatSignature(IMethodSymbol method)
    {
        var containingType = method.ContainingType?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? method.Name;
        var parameters = string.Join(", ", method.Parameters.Select(parameter => parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)));
        return parameters.Length == 0
            ? $"{containingType}.{method.Name}()"
            : $"{containingType}.{method.Name}({parameters})";
    }

    private static MemberAccessExpressionSyntax GetMemberAccess(SyntaxTree tree, string text)
    {
        return tree
            .GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.ToString() == text);
    }
}
