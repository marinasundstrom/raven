using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class DeclarationCapabilityTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void CallableCategoriesDistinguishLogicalOwnership(bool allowFunction)
    {
        var compilation = Create();
        var tree = compilation.SyntaxTrees[0]; var model = compilation.GetSemanticModel(tree);
        var function = (IMethodSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single())!;
        var method = (IMethodSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single())!;
        var admitted = allowFunction ? EmissionDeclarationKind.AssemblyFunction : EmissionDeclarationKind.StaticMethod;
        var capabilities = new EmissionCapabilities([EmissionPrimitiveType.Int32], Enum.GetValues<LinearInstructionKind>(), [admitted], methodVisibilities: [Accessibility.Public], functionVisibilities: [Accessibility.Public, Accessibility.Internal]);
        Assert.Equal(allowFunction, SourceCallablePlan.TryCreate(function, out var functionPlan, capabilities));
        Assert.Equal(!allowFunction, SourceCallablePlan.TryCreate(method, out var methodPlan, capabilities));
        var accepted = allowFunction ? functionPlan : methodPlan;
        Assert.Equal(admitted, accepted!.DeclarationKind);
        Assert.Equal(allowFunction, accepted.TypeOwner is null);
        Assert.Null(allowFunction ? methodPlan : functionPlan);
        // A plan collected without target admission cannot bypass the body boundary.
        Assert.True(SourceCallablePlan.TryCreate(allowFunction ? method : function, out var unadmitted));
        Assert.False(unadmitted!.TryLowerBody(compilation, _ => false, out var body, out var failure, capabilities));
        Assert.Null(body);
        Assert.Contains("target does not support declaration", failure!.Detail);
        Assert.Same(unadmitted.Syntax.SyntaxTree, failure.Syntax.SyntaxTree);
    }

    [Theory]
    [InlineData("public", Accessibility.Public)]
    [InlineData("internal", Accessibility.Internal)]
    public void AssemblyFunctionAccessRequiresIndependentAdmission(string modifier, Accessibility visibility)
    {
        var compilation = Compilation.Create("FunctionAccess", [SyntaxTree.ParseText($"{modifier} func Value() -> int => 42")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var syntax = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var denied = new EmissionCapabilities([EmissionPrimitiveType.Int32], Enum.GetValues<LinearInstructionKind>(),
            [EmissionDeclarationKind.AssemblyFunction], methodVisibilities: [visibility]);
        Assert.False(SourceCallablePlan.TryCreate(symbol, out _, denied));
        var allowed = new[] { visibility };
        var profile = new EmissionCapabilities([EmissionPrimitiveType.Int32], Enum.GetValues<LinearInstructionKind>(),
            [EmissionDeclarationKind.AssemblyFunction], functionVisibilities: allowed);
        allowed[0] = Accessibility.Private;
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, profile));
        Assert.Equal(visibility, plan!.Visibility);
        Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var failure, profile), failure?.Detail);
        Assert.False(plan.TryLowerBody(compilation, _ => false, out _, out _, denied));
    }

    [Fact]
    public void TypeCategoryIsIndependentAndProfilesOwnTheirInput()
    {
        var compilation = Create();
        var tree = compilation.SyntaxTrees[0]; var model = compilation.GetSemanticModel(tree);
        var symbol = (INamedTypeSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single())!;
        var declarations = new[] { EmissionDeclarationKind.StaticType };
        var capabilities = new EmissionCapabilities([], [], declarations, [Accessibility.Public]);
        declarations[0] = EmissionDeclarationKind.StaticMethod;
        Assert.True(SourceStaticTypePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.Equal("Helpers", plan!.Name);
        Assert.False(capabilities.Allows(EmissionDeclarationKind.StaticMethod));
        Assert.False(SourceStaticTypePlan.TryCreate(symbol, out var rejected, new([], [])));
        Assert.Null(rejected);
        Assert.True(SourceStaticTypePlan.TryCreate(symbol, out _, ReflectionEmitCapabilities.Shared));
    }

    [Fact]
    public void InternalTypeVisibilityRequiresExplicitAdmission()
    {
        var compilation = Compilation.Create("InternalDeclarations", [SyntaxTree.ParseText("internal static class Hidden { }")], TestMetadataReferences.Default);
        var tree = compilation.SyntaxTrees[0];
        var symbol = (INamedTypeSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single())!;
        var visibilities = new[] { Accessibility.Internal };
        var profile = new EmissionCapabilities([], [], [EmissionDeclarationKind.StaticType], visibilities);
        visibilities[0] = Accessibility.Public;
        Assert.True(SourceStaticTypePlan.TryCreate(symbol, out var plan, profile));
        Assert.Equal(Accessibility.Internal, plan!.Visibility);
        Assert.False(SourceStaticTypePlan.TryCreate(symbol, out _, new([], [], [EmissionDeclarationKind.StaticType], [Accessibility.Public])));
        Assert.False(SourceStaticTypePlan.TryCreate(symbol, out _, new([], [], [EmissionDeclarationKind.StaticType])));
        Assert.True(SourceStaticTypePlan.TryCreate(symbol, out _, ReflectionEmitCapabilities.Shared));
    }

    [Theory]
    [InlineData("internal", Accessibility.Internal)]
    [InlineData("private", Accessibility.Private)]
    public void MethodVisibilityNeedsExplicitAdmission(string keyword, Accessibility visibility)
    {
        var compilation = Compilation.Create("VisibilityPlans", [SyntaxTree.ParseText($"public static class C {{ {keyword} static func Value() -> int {{ 42 }} }}")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var tree = compilation.SyntaxTrees[0];
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single())!;
        var allowed = new[] { visibility };
        var profile = new EmissionCapabilities([EmissionPrimitiveType.Int32], Enum.GetValues<LinearInstructionKind>(),
            [EmissionDeclarationKind.StaticMethod], methodVisibilities: allowed);
        allowed[0] = Accessibility.Public;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, profile));
        Assert.Equal(visibility, plan!.Visibility);
        var publicOnly = new EmissionCapabilities([EmissionPrimitiveType.Int32], Enum.GetValues<LinearInstructionKind>(),
            [EmissionDeclarationKind.StaticMethod], methodVisibilities: [Accessibility.Public]);
        Assert.False(SourceCallablePlan.TryCreate(method, out _, publicOnly));
        Assert.False(plan.TryLowerBody(compilation, _ => false, out _, out _, publicOnly));
        Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var failure, profile), failure?.Detail);
    }

    private static Compilation Create() => Compilation.Create("Declarations", [SyntaxTree.ParseText("""
        func Main() -> int { Helpers.Value() }
        public static class Helpers {
            public static func Value() -> int { 42 }
        }
        """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
}
