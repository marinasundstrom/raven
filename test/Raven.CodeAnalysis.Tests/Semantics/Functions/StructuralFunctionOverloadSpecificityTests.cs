using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class StructuralFunctionOverloadSpecificityTests
{
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
