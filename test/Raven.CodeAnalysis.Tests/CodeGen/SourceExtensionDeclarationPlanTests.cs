using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests;

public sealed class SourceExtensionDeclarationPlanTests : CompilationTestBase
{
    [Fact]
    public void ExtensionContainerUsesStaticStorageOnlyWithExplicitCapability()
    {
        var (compilation, _) = CreateCompilation("""
            public extension Values<T> for T {
                func Identity() -> T => self
            }
            """, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<ExtensionDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax));
        EmissionCapabilities Capabilities(bool extensions) => new(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>(), Enum.GetValues<EmissionDeclarationKind>(),
            Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsGenericStaticOwners: true, allowsGenericMethods: true, allowsLoweredExtensionCalls: extensions);
        Assert.False(SourceTypePlan.TryCreate(symbol, out _, Capabilities(false)));
        Assert.True(SourceTypePlan.TryCreate(symbol, out var plan, Capabilities(true)));
        Assert.True(plan!.IsStatic);
        Assert.True(plan.IsExtensionContainer);
        var method = symbol.GetMembers("Identity").OfType<IMethodSymbol>().Single();
        Assert.True(CallableSignature.TryCreate(method, out var signature, Capabilities(true)));
        Assert.Equal(0, signature.DeclaringTypeArity);
        Assert.True(signature.DeclaringTypeIsStatic);
        Assert.Single(signature.GenericParameterNames);
        Assert.Equal(EmissionDeclarationKind.StaticType, plan.DeclarationKind);
    }
}
