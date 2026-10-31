using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class VirtualClassDeclarationTests
{
    [Theory]
    [InlineData(OptimizationLevel.Debug)]
    [InlineData(OptimizationLevel.Release)]
    public void VirtualHierarchyRequiresExplicitPortableCapabilityAndPreservesClrDispatch(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            public interface Reader {
                func Read() -> int
            }
            public abstract class Base : Reader {
                private var value: int
                init(value: int) { self.value = value }
                public abstract func Read() -> int
                public virtual func Set(value: int) { self.value = value }
                public func Get() -> int => value
            }
            public class Derived : Base {
                init(value: int) : base(value) { }
                public override func Read() -> int => Get()
                public override func Set(value: int) { base.Set(value) }
            }
            public static class Entry {
                public static func Run() -> int {
                    let item = Derived(7)
                    let parent: Base = item
                    let reader: Reader = parent
                    if reader.Read() != 7 { return 1 }
                    parent.Set(42)
                    return reader.Read()
                }
            }
            """);
        var compilation = Compilation.Create("VirtualHierarchy" + Guid.NewGuid().ToString("N"), [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public], [Accessibility.Public],
            allowsRootClassSignatures: true, allowsRootClassLocals: true, allowsInterfaceSignatures: true,
            allowsInterfaceDispatch: true, allowsLocalClassInheritance: true, allowsClassVirtualSlots: true);
        var model = compilation.GetSemanticModel(tree);
        var parent = (INamedTypeSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First())!;
        Assert.False(SourceTypePlan.TryCreate(parent, out _, ReflectionEmitCapabilities.Shared));
        Assert.True(SourceTypePlan.TryCreate(parent, out _, capabilities));
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Where(m => m.Identifier.Text is "Read" or "Set"))
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            if (method.ContainingType.TypeKind == TypeKind.Interface) continue;
            Assert.False(SourceCallablePlan.TryCreate(method, out _, ReflectionEmitCapabilities.Shared));
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, capabilities));
            if (!method.IsAbstract)
                Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).GetType("Entry")!.GetMethod("Run")!.Invoke(null, null));
    }
}
