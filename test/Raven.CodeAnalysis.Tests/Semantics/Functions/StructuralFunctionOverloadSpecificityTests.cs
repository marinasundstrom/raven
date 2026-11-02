using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class StructuralFunctionOverloadSpecificityTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "DotNet")]
    public void DotNet_LambdaResultPrefersIdentityOverNumericConversion(bool reverse)
        => AssertNumericCallbackSelection(TargetPlatform.DotNet, reverse);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_LambdaResultPrefersIdentityOverNumericConversion(bool reverse)
        => AssertNumericCallbackSelection(TargetPlatform.NeoCLR, reverse);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "DotNet")]
    public void DotNet_LambdaValueResultIsPreferredOverDiscard(bool reverse)
        => AssertNumericCallbackSelection(TargetPlatform.DotNet, reverse, "unit");

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_LambdaValueResultIsPreferredOverDiscard(bool reverse)
        => AssertNumericCallbackSelection(TargetPlatform.NeoCLR, reverse, "unit");

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNet_UnrelatedNumericCallbackResultsRemainAmbiguous()
        => AssertUnrelatedNumericCallbacks(TargetPlatform.DotNet);

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_UnrelatedNumericCallbackResultsRemainAmbiguous()
        => AssertUnrelatedNumericCallbacks(TargetPlatform.NeoCLR);

    private static void AssertUnrelatedNumericCallbacks(TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("""
            class Scheduler {
                static func Choose(callback: () -> double) -> int => 1
                static func Choose(callback: () -> decimal) -> int => 2
            }
            func Test() -> int => Scheduler.Choose(() => 1)
            """);
        var compilation = Compilation.Create("AmbiguousNumericCallback", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var errors = compilation.GetSemanticModel(tree).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error);
        Assert.Equal("RAV0121", Assert.Single(errors).Id);
    }

    private static void AssertNumericCallbackSelection(TargetPlatform target, bool reverse, string alternative = "long")
    {
        const string narrow = "static func Choose(callback: () -> int) -> int => 1";
        var wide = $"static func Choose(callback: () -> {alternative}) -> int => 2";
        var tree = SyntaxTree.ParseText($$"""
            class Scheduler {
                {{(reverse ? wide : narrow)}}
                {{(reverse ? narrow : wide)}}
            }
            func Test() -> int => Scheduler.Choose(() => 1)
            """);
        var compilation = Compilation.Create("NumericCallback", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        Assert.DoesNotContain(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var chosen = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);
        var callback = Assert.IsAssignableFrom<INamedTypeSymbol>(chosen.Parameters[0].Type);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(callback.GetMembers("Invoke").OfType<IMethodSymbol>()).ReturnType.SpecialType);
    }

    [Theory]
    [InlineData(TargetPlatform.DotNet, false)]
    [InlineData(TargetPlatform.DotNet, true)]
    [InlineData(TargetPlatform.NeoCLR, false)]
    [InlineData(TargetPlatform.NeoCLR, true)]
    public void ConstructedCallbackResultIsMoreSpecificThanBareTypeParameter(TargetPlatform target, bool reverse)
    {
        var general = "static func Choose<T>(callback: () -> T) -> int => 1";
        var specific = "static func Choose<T>(callback: () -> Box<T>) -> int => 2";
        var tree = SyntaxTree.ParseText($$"""
            class Box<T> { }
            class Scheduler {
                {{(reverse ? specific : general)}}
                {{(reverse ? general : specific)}}
            }
            func Test() -> int {
                return Scheduler.Choose(() => Box<int>())
            }
            """);
        var compilation = Compilation.Create("CallbackSpecificity", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = compilation.GetSemanticModel(tree);
        Assert.Empty(model.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression.ToString() == "Scheduler.Choose");
        var chosen = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(chosen.TypeArguments).SpecialType);
    }

    [Theory]
    [InlineData(TargetPlatform.DotNet)]
    [InlineData(TargetPlatform.NeoCLR)]
    public void ConflictingCallbackSpecificityRemainsAmbiguous(TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("""
            class Box<T> { }
            class Scheduler {
                static func Choose<T, U>(first: () -> Box<T>, second: () -> U) -> int => 1
                static func Choose<T, U>(first: () -> T, second: () -> Box<U>) -> int => 2
            }
            func Test() -> int {
                return Scheduler.Choose(() => Box<int>(), () => Box<int>())
            }
            """);
        var compilation = Compilation.Create("ConflictingSpecificity", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var errors = compilation.GetSemanticModel(tree).GetDiagnostics()
            .Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Equal("RAV0121", Assert.Single(errors).Id);
    }

}
