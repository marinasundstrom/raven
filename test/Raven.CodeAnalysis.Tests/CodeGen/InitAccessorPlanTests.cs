using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class InitAccessorPlanTests : CompilationTestBase
{
    private static EmissionCapabilities Capabilities(bool init) => new(
        Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
        Enum.GetValues<EmissionDeclarationKind>().Where(kind => init || kind != EmissionDeclarationKind.InitAccessor),
        Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
        allowsRootClassSignatures: true, allowsRootClassLocals: true);

    [Theory]
    [InlineData("val Value: int { init; } = 0")]
    [InlineData("private var stored: int = 0\nval Value: int { get => stored; init { stored = value } }")]
    public void InitAccessorRequiresCapabilityAndLowersThroughSharedPlan(string members)
    {
        var (compilation, _) = CreateCompilation("class Settings {\n" + members + "\n}",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        var owner = (INamedTypeSymbol)model.GetDeclaredSymbol(syntax)!;
        var setter = owner.GetMembers("Value").OfType<IPropertySymbol>().Single().SetMethod!;
        Assert.Equal(MethodKind.InitOnly, setter.MethodKind);
        Assert.False(SourceCallablePlan.TryCreate(setter, out _, Capabilities(false)));
        Assert.True(SourceCallablePlan.TryCreate(setter, out var plan, Capabilities(true)));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
    }
}
