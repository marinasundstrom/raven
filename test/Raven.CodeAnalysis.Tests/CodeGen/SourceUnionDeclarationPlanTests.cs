using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests;

public sealed class SourceUnionDeclarationPlanTests : CompilationTestBase
{
    [Fact]
    public void CaseTestsLowerInConditionalBranchesAndBooleanReturns()
    {
        var (compilation, _) = CreateCompilation("""
            union Choice<T> {
                case Some(value: T)
                case None
                func TryRead(out output: T) -> bool {
                    output = default
                    if self is Some(let value) {
                        output = value
                        return true
                    }
                    return false
                }
                func IsEmpty() -> bool => self is None
            }
            """, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var union = (SourceUnionSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration)!;
        EmissionCapabilities Capabilities(bool patterns) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsRootClassLocals: true, allowsManagedReferences: true,
            allowsConstructedFieldReferences: true, allowsGenericInstanceMethods: true,
            allowsGenericStaticOwners: true, allowsGenericClassOwners: true, allowsCasePatterns: patterns);
        foreach (var method in union.GetMembers().OfType<IMethodSymbol>().Where(m => m.Name is "TryRead" or "IsEmpty"))
        {
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true), declaration));
            Assert.False(plan!.TryLowerBody(compilation, _ => false, out _, out _, Capabilities(false)));
            Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        }
    }

    [Theory]
    [InlineData("class Display { override func ToString() -> string? => \"display\" }")]
    [InlineData("struct Display { override func GetHashCode() -> int => 42 }")]
    public void OverridesRequireExplicitEmissionCapabilities(string source)
    {
        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var declaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration)!;
        Assert.False(SourceCallablePlan.TryCreate(method, out _, ReflectionEmitCapabilities.Shared));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void CompleteGraphPreservesPhysicalCaseOwnersAndUnusedContracts(bool generic)
    {
        var source = generic
            ? "union Choice<T> { case Some(value: T) case None func Read() -> int => 42 }"
            : "union Choice { case Some(value: int) case None func Read() -> int => 42 }";
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
        var capabilities = Capabilities(true);
        EmissionCapabilities Capabilities(bool overrides) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>().Where(kind => overrides || kind != EmissionDeclarationKind.ValueObjectOverride), [Accessibility.Public, Accessibility.Internal],
            [Accessibility.Public, Accessibility.Internal, Accessibility.Private],
            allowsRootClassSignatures: true, allowsRootClassLocals: true, allowsManagedReferences: true,
            allowsConstructedFieldReferences: true, allowsGenericInstanceMethods: true,
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
        foreach (var display in plan.Types.SelectMany(t => t.Methods).Where(m => m.Name == "ToString"))
        {
            Assert.True(SourceCallablePlan.TryCreate(display, out var displayPlan, capabilities, declaration), display.ToDisplayString());
            Assert.Equal(EmissionOverrideKind.ObjectToString, displayPlan!.Override);
            Assert.Equal(EmissionDeclarationKind.ValueObjectOverride, displayPlan.DeclarationKind);
            Assert.False(SourceCallablePlan.TryCreate(display, out _, Capabilities(false), declaration));
        }
        var helper = carrier.Methods.First(m => m.Name == "TryGetValue");
        Assert.False(SourceCallablePlan.TryCreate(helper, out _, capabilities));
        Assert.True(SourceCallablePlan.TryCreate(helper, out var callable, capabilities, synthesizedAnchor: declaration));
        Assert.Same(declaration, callable!.Body);
        Assert.True(compilation.TryGetSynthesizedMethodBody(helper, BoundTreeView.Lowered, out var body));
        Assert.NotNull(body);
        Assert.True(callable.TryLowerBody(compilation, _ => false, out var lowered, out var failure, capabilities), failure?.Detail);
        Assert.NotNull(lowered);
        var wrongAnchor = SyntaxTree.ParseText("union Other { case None }").GetRoot()
            .DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        Assert.False(SourceCallablePlan.TryCreate(helper, out _, capabilities, wrongAnchor));
        var authored = carrier.Methods.Single(m => m.Name == "Read");
        Assert.True(SourceCallablePlan.TryCreate(authored, out var authoredPlan, capabilities, declaration));
        Assert.IsType<ArrowExpressionClauseSyntax>(authoredPlan!.Body);
        var rejected = new List<string>();
        foreach (var type in plan.Types)
        {
            foreach (var method in type.Methods.Where(m => m.MethodKind == MethodKind.Constructor || m.Name is "TryGetValue" or "Deconstruct" || m.MethodKind == MethodKind.PropertyGet && type.Symbol is IUnionCaseTypeSymbol))
            {
                if (!SourceCallablePlan.TryCreate(method, out var core, capabilities, declaration))
                    rejected.Add(method.ToDisplayString() + ": declaration " + string.Join(",", method.DeclaringSyntaxReferences.Select(r => r.GetSyntax().GetType().Name)));
                else if (!core!.TryLowerBody(compilation, _ => false, out _, out var rejectedBody, capabilities))
                    rejected.Add(method.ToDisplayString() + ": " + rejectedBody!.Detail);
            }
            Assert.Equal(type.Methods.Length, type.Methods.Distinct<IMethodSymbol>(SymbolEqualityComparer.Default).Count());
            Assert.All(type.Properties, property =>
            {
                if (property.GetMethod is { } get) Assert.Contains(type.Methods, method => SymbolEqualityComparer.Default.Equals(method, get));
                if (property.SetMethod is { } set) Assert.Contains(type.Methods, method => SymbolEqualityComparer.Default.Equals(method, set));
            });
        }
        Assert.True(rejected.Count == 0, string.Join("\n", rejected));
    }
}
