using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.Tests;

public sealed class ImplicitStructConstructorTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("struct", false)]
    [InlineData("record struct", false)]
    [InlineData("struct", true)]
    [InlineData("record struct", true)]
    public void DelegationToImplicitConstructorZerosUnassignedStorage(string kind, bool hasInitializer)
    {
        var compilation = Create(kind, TargetPlatform.DotNet, hasInitializer);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(hasInitializer ? 49 : 42, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
        Assert.DoesNotContain(loaded.Assembly.GetType("Item")!.GetConstructors(), constructor => constructor.GetParameters().Length == 0);
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("struct")]
    [InlineData("record struct")]
    public void NeoClrSemanticBindingFindsImplicitConstructor(string kind)
    {
        var compilation = Create(kind, TargetPlatform.NeoCLR);
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees.Single());
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var type = compilation.GetTypeByMetadataName("Item")!;
        var constructor = Assert.Single(type.Constructors, c => !c.IsStatic && c.Parameters.IsEmpty);
        Assert.True(constructor.IsImplicitlyDeclared);
        Assert.Equal(Accessibility.Public, constructor.DeclaredAccessibility);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void PortableStructConstructorPlanSupportsImplicitDelegation()
    {
        var compilation = Create("struct", TargetPlatform.DotNet, hasInitializer: true);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var constructor = compilation.GetTypeByMetadataName("Item")!.Constructors.Single(c => c.Parameters.Length == 1 && c.Parameters[0].Type.SpecialType == SpecialType.System_Int32);
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsManagedReferences: true,
            allowsExternalInterfaceDeclarations: true, allowsConstructedInterfaceImplementations: true);
        Assert.True(SourceCallablePlan.TryCreate(constructor, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
    }

    private static Compilation Create(string kind, TargetPlatform target, bool hasInitializer = false)
        => Compilation.Create("ImplicitStruct" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText($$"""
            {{kind}} Item {
                val Value: int { get; }
                val Other: int { get; } {{(hasInitializer ? "= 7" : "")}}
                init(value: int) : self() { Value = value }
            }
            class Probe {
                static func Run() -> int {
                    let item = Item(42)
                    return item.Value + item.Other + Item().Value + Item().Other
                }
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
}
