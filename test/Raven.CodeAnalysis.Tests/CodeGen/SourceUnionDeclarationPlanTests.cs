using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests;

public sealed class SourceUnionDeclarationPlanTests : CompilationTestBase
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void CompleteGraphPreservesPhysicalCaseOwnersAndUnusedContracts(bool generic)
    {
        var source = generic
            ? "union Choice<T> { case Some(value: T) case None }"
            : "union Choice { case Some(value: int) case None }";
        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var union = Assert.IsType<SourceUnionSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        var plan = SourceUnionDeclarationPlan.Create(union);
        Assert.Same(union, plan.Types[0].Symbol);
        Assert.Equal(generic ? 4 : 3, plan.Types.Length);
        var carrier = plan.Types[0];
        Assert.Contains(carrier.Fields, field => field.MetadataName == UnionFieldUtilities.TagFieldName && field.Type.SpecialType == SpecialType.System_Byte);
        Assert.All(union.DeclaredCaseTypes, @case => Assert.Contains(carrier.Fields, field => field.MetadataName == UnionFieldUtilities.GetPayloadFieldName(@case.Name)));
        Assert.Contains(carrier.Methods, m => m.Name == "TryGetValue");
        Assert.Contains(carrier.Methods, m => m.Name == "ToString");
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), [],
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public, Accessibility.Internal],
            [Accessibility.Public, Accessibility.Internal, Accessibility.Private],
            allowsRootClassSignatures: true, allowsManagedReferences: true,
            allowsGenericStaticOwners: true, allowsGenericClassOwners: true);
        foreach (var @case in union.DeclaredCaseTypes)
        {
            var plannedCase = Assert.Single(plan.Types.Where(t => SymbolEqualityComparer.Default.Equals(t.Symbol, @case)));
            Assert.Same(((SourceUnionCaseTypeSymbol)@case).MetadataContainingType, plannedCase.MetadataOwner);
            Assert.True(plan.Types.IndexOf(plan.Types.Single(t => SymbolEqualityComparer.Default.Equals(t.Symbol, plannedCase.MetadataOwner))) < plan.Types.IndexOf(plannedCase));
            Assert.Contains(plannedCase.Methods, m => m.MethodKind == MethodKind.Constructor);
            Assert.True(SourceTypePlan.TryCreate(@case, out var typePlan, capabilities));
            Assert.Same(plannedCase.MetadataOwner, typePlan!.MetadataOwner);
            if (@case.Arity == 1)
            {
                var constructed = (INamedTypeSymbol)@case.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
                Assert.True(SourceTypePlan.TryCreate(constructed, out var constructedPlan, capabilities));
                Assert.Same(plannedCase.MetadataOwner, constructedPlan!.MetadataOwner);
            }
        }
        var helper = carrier.Methods.First(m => m.Name == "TryGetValue");
        Assert.False(SourceCallablePlan.TryCreate(helper, out _, capabilities));
        Assert.True(SourceCallablePlan.TryCreate(helper, out var callable, capabilities, synthesizedAnchor: declaration));
        Assert.Same(declaration, callable!.Body);
        Assert.True(compilation.TryGetSynthesizedMethodBody(helper, BoundTreeView.Lowered, out var body));
        Assert.NotNull(body);
        foreach (var type in plan.Types)
        {
            Assert.Equal(type.Methods.Length, type.Methods.Distinct<IMethodSymbol>(SymbolEqualityComparer.Default).Count());
            Assert.All(type.Properties, property =>
            {
                if (property.GetMethod is { } get) Assert.Contains(type.Methods, method => SymbolEqualityComparer.Default.Equals(method, get));
                if (property.SetMethod is { } set) Assert.Contains(type.Methods, method => SymbolEqualityComparer.Default.Equals(method, set));
            });
        }
    }
}
