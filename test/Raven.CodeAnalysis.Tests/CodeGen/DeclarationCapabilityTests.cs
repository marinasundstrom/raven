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
        var capabilities = new EmissionCapabilities([EmissionPrimitiveType.Int32], Enum.GetValues<LinearInstructionKind>(), [admitted]);
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

    [Fact]
    public void TypeCategoryIsIndependentAndProfilesOwnTheirInput()
    {
        var compilation = Create();
        var tree = compilation.SyntaxTrees[0]; var model = compilation.GetSemanticModel(tree);
        var symbol = (INamedTypeSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single())!;
        var declarations = new[] { EmissionDeclarationKind.StaticType };
        var capabilities = new EmissionCapabilities([], [], declarations);
        declarations[0] = EmissionDeclarationKind.StaticMethod;
        Assert.True(SourceStaticTypePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.Equal("Helpers", plan!.Name);
        Assert.False(capabilities.Allows(EmissionDeclarationKind.StaticMethod));
        Assert.False(SourceStaticTypePlan.TryCreate(symbol, out var rejected, new([], [])));
        Assert.Null(rejected);
        Assert.True(SourceStaticTypePlan.TryCreate(symbol, out _, ReflectionEmitCapabilities.Shared));
    }

    private static Compilation Create() => Compilation.Create("Declarations", [SyntaxTree.ParseText("""
        func Main() -> int { Helpers.Value() }
        public static class Helpers {
            public static func Value() -> int { 42 }
        }
        """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
}
