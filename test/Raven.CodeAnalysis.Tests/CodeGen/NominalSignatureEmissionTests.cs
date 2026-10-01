using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class NominalSignatureEmissionTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void NominalSignaturesPreserveIdentityAndMutation(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Item {
                var Number: int = 41
                func Same() -> Item => self
            }
            class Copied {
                private var number: int
                init(item: Item) { number = item.Number }
                val Number: int => number
            }
            func Create() -> Item => Item()
            func Identity(item: Item) -> Item => item
            func Update(item: Item) { item.Number = 42 }
            func Main() -> int {
                let original = Create()
                let alias = Identity(original.Same())
                Identity(alias)
                Update(alias)
                return Copied(original).Number
            }
            """);
        var compilation = Compilation.Create("NominalSignatures", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().Where(n => n is FunctionStatementSyntax or MethodDeclarationSyntax))
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
            if (method.Name == "Identity")
            {
                var denied = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                    Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public, Accessibility.Internal],
                    [Accessibility.Public], [Accessibility.Internal], allowsRootClassLocals: true);
                Assert.False(plan.IsSupportedBy(denied));
            }
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var itemType = assembly.GetType("Item")!;
        Assert.Equal(itemType, itemType.GetMethod("Same")!.ReturnType);
        Assert.Equal(itemType, assembly.GetType("Copied")!.GetConstructors().Single().GetParameters().Single().ParameterType);
    }
}
