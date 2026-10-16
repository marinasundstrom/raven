using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableExternalInheritanceTests
{
    [Fact]
    public void ExternalClassSlotsAndBaseCallsRequireExplicitCapability()
    {
        var provider = TestMetadataFactory.CreateFileReferenceFromSource("""
            public abstract class Parent {
                private var value: int = 40
                protected init() {}
                public virtual func Read() -> int => value
            }
            """, "ExternalBaseProvider");
        var tree = SyntaxTree.ParseText("""
            public class Child : Parent {
                public init() : base() {}
                public override func Read() -> int => base.Read() + 2
            }
            """);
        var compilation = Compilation.Create("ExternalBaseConsumer", [tree], TestMetadataReferences.Default.Append(provider).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        foreach (var admit in new[] { false, true })
        {
            var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
                allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true,
                allowsExternalInstanceCalls: true, allowsExternalConstructors: true, allowsClassVirtualSlots: true,
                allowsLocalClassInheritance: true, allowsProtectedConstructors: true, allowsExternalClassInheritance: admit);
            Assert.Equal(admit, SourceTypePlan.TryCreate(method.ContainingType!, out _, capabilities));
            if (!admit) continue;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, capabilities));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        }
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Child")!;
        Assert.Equal(42, type.GetMethod("Read")!.Invoke(Activator.CreateInstance(type), null));
    }
    [Fact]
    public void GenericLocalDerivedClassRequiresExplicitCapability()
    {
        var tree = SyntaxTree.ParseText("""
            public open class Parent {
                public field Number: int
                public init(number: int) { Number = number }
            }
            public class Child<T> : Parent {
                private var value: T
                public init(number: int, value: T) : base(number + 1) { self.value = value }
                public func Get() -> T => value
            }
            func Main() -> int {
                let child = Child<int>(1, 40)
                let parent: Parent = child
                return parent.Number + child.Get()
            }
            """);
        var compilation = Compilation.Create("GenericLocalDerived", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        foreach (var admit in new[] { false, true })
        {
            var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
                allowsGenericClassOwners: true, allowsLocalClassInheritance: true, allowsGenericLocalClassInheritance: admit);
            Assert.Equal(admit, SourceTypePlan.TryCreate(compilation.GetTypeByMetadataName("Child`1")!, out _, capabilities));
        }
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.EntryPoint!.Invoke(null, null));
    }
    [Fact]
    public void ConstructedLocalBaseRequiresExplicitCapability()
    {
        var tree = SyntaxTree.ParseText("""
            public class Leaf : Middle<bool, int> {
                public init(value: int) : base(value) { }
            }
            public open class Middle<Unused, T> : Parent<T> {
                public init(value: T) : base(value) { }
            }
            public open class Parent<T> {
                private var value: T
                public init(value: T) { self.value = value }
                public func Get() -> T => value
            }
            func Main() -> int {
                let leaf = Leaf(42)
                let parent: Parent<int> = leaf
                return parent.Get()
            }
            """);
        var compilation = Compilation.Create("ConstructedLocalBase", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        foreach (var admit in new[] { false, true })
        {
            var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
                allowsGenericClassOwners: true, allowsLocalClassInheritance: true, allowsGenericLocalClassInheritance: true,
                allowsConstructedClassBases: admit);
            Assert.Equal(admit, SourceTypePlan.TryCreate(compilation.GetTypeByMetadataName("Leaf")!, out _, capabilities));
            Assert.Equal(admit, SourceTypePlan.TryCreate(compilation.GetTypeByMetadataName("Middle`2")!, out _, capabilities));
        }
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.EntryPoint!.Invoke(null, null));
        Assert.Equal(typeof(int), loaded.Assembly.GetType("Leaf")!.BaseType!.BaseType!.GenericTypeArguments.Single());
    }
}
