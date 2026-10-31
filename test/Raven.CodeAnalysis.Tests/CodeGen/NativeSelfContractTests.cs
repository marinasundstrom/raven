using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Mono.Cecil;

namespace Raven.CodeAnalysis.Tests;

public class NativeSelfContractTests
{
    const string Source = """
        public interface Cloneable { func Clone() -> Self; val Current: Self { get; } }
        public interface Comparable<T> { func Compare(other: T) -> int }
        public interface Number : Comparable<Self> {
            static val Zero: Self { get; }
            static func +(left: Self, right: Self) -> Self
            static func Add(left: Self, right: Self) -> Self
        }
        public struct Count : Number, Cloneable {
            public func Clone() -> Self => self
            public val Current: Self => self
            public var Value: int
            public func Compare(other: Self) -> int => Value - other.Value
            static val Zero: Self => Count()
            static func +(left: Self, right: Self) -> Self => Add(left, right)
            public static func Add(left: Self, right: Self) -> Self {
                var result = Count()
                result.Value = left.Value + right.Value
                return result
            }
        }
        public class Consumer {
            public static func Copy<T>(value: T) -> T where T: Cloneable => value.Clone()
            public static func Sum<T>(left: T, right: T) -> T where T: Number => T.Add(left, right) + T.Zero
        }
        """;

    private static CompilationOptions SelfOptions() => CompilationOptions.NeoCLR
        .WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
        .WithRuntimeTypeOfContract(null)
        .WithRuntimeSelfTypeContract(new("NativeSelfReference", "NativeSelfMarker"));

    private static MetadataReference[] References()
    {
        var marker = Compilation.Create("NativeSelfReference", [SyntaxTree.ParseText("public sealed class NativeSelfMarker {}")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        Assert.True(marker.Emit(image).Success);
        // A synthetic CLI core keeps these compiler/metadata tests independent of
        // external neoCLR bundles. Native execution is verified in the runtime repo.
        var corePath = TestMetadataReferences.Default.OfType<PortableExecutableReference>()
            .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
        using var core = AssemblyDefinition.ReadAssembly(corePath);
        core.Name.Name = "NeoCLR.CoreProbe";
        core.Name.PublicKey = [];
        using var coreImage = new MemoryStream();
        core.Write(coreImage);
        return [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(coreImage.ToArray()),
            MetadataReference.CreateFromImage(image.ToArray())];
    }

    const string InheritanceSource = """
        public interface Cloneable { func Clone() -> Self }
        public open class Base : Cloneable {
            public virtual func Clone() -> Self => self
        }
        public class Derived : Base {}
        public class Consumer {
            public static func Copy<T>(value: T) -> T where T: Cloneable => value.Clone()
            public static func Check(value: Derived) -> Base => Copy<Base>(value)
        }
        """;

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void SelfInheritancePreservesDeclarationResult(int variant)
    {
        var source = variant switch
        {
            1 => InheritanceSource.Replace("class Derived : Base {}", "class Derived : Base { public override func Clone() -> Base => self }"),
            2 => InheritanceSource.Replace("class Derived : Base {}", "class Derived : Base, Cloneable { func Cloneable.Clone() -> Self => self }")
                .Replace("Check(value: Derived) -> Base => Copy<Base>(value)", "Check(value: Derived) -> Derived => Copy<Derived>(value)"),
            _ => InheritanceSource
        };
        var options = SelfOptions();
        var compilation = Compilation.Create("SelfInheritance", [SyntaxTree.ParseText(source)], References(), options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("\n", diagnostics));
        var baseType = compilation.GetTypeByMetadataName("Base")!;
        Assert.Same(baseType, baseType.GetMembers("Clone").OfType<IMethodSymbol>().Single().ReturnType);
        var derived = compilation.GetTypeByMetadataName("Derived")!;
        if (variant == 1)
            Assert.Same(baseType, derived.GetMembers("Clone").OfType<IMethodSymbol>().Single().ReturnType);
        if (variant == 2)
            Assert.Same(derived, derived.GetMembers().OfType<IMethodSymbol>()
                .Single(method => !method.ExplicitInterfaceImplementations.IsEmpty).ReturnType);
        AssertRequiresNativeBackend(compilation);
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void InheritedSelfDoesNotPromiseDerivedResult(bool inferred)
    {
        var source = InheritanceSource.Replace("Copy<Base>(value)", inferred ? "Copy(value)" : "Copy<Derived>(value)");
        var options = SelfOptions();
        var compilation = Compilation.Create("SelfInheritance", [SyntaxTree.ParseText(source)], References(), options);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void RedeclaredSelfRejectsInheritedBaseResult(bool wrongExplicitResult)
    {
        var source = InheritanceSource.Replace("class Derived : Base {}", wrongExplicitResult
            ? "class Derived : Base, Cloneable { func Cloneable.Clone() -> Base => self }"
            : "class Derived : Base, Cloneable {}");
        var options = SelfOptions();
        var compilation = Compilation.Create("SelfInheritance", [SyntaxTree.ParseText(source)], References(), options);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void SelfPropertyRedeclarationRequiresDerivedResult(bool redeclared)
    {
        var source = """
            interface CurrentValue { val Current: Self { get; } }
            open class Base : CurrentValue { val Current: Self => self }
            class Derived : Base {}
            """;
        if (redeclared)
            source = source.Replace("Derived : Base", "Derived : Base, CurrentValue");
        var options = SelfOptions();
        var compilation = Compilation.Create("SelfPropertyInheritance", [SyntaxTree.ParseText(source)], References(), options);
        Assert.Equal(redeclared, compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NativeSelfKeepsInterfaceArityAndGenericResult()
    {
        var contract = new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker");
        var options = SelfOptions()
            .WithAllowNullableValueTypes(false)
            .WithRuntimeSelfTypeContract(contract)
            .WithAsyncCancellationPropagation(true);
        Assert.False(options.AllowNullableValueTypes);
        Assert.Same(contract, options.WithAllowNullableValueTypes(true).RuntimeSelfTypeContract);
        var compilation = Compilation.Create("NativeSelfProbe", [SyntaxTree.ParseText(Source)], References(), options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("\n", diagnostics));
        var number = compilation.GetTypeByMetadataName("Number")!;
        Assert.Empty(number.TypeParameters);
        var count = compilation.GetTypeByMetadataName("Count")!;
        var comparable = Assert.Single(number.Interfaces);
        Assert.Equal("Comparable", comparable.Name);
        Assert.Equal("NativeSelfMarker", Assert.Single(comparable.TypeArguments).Name);
        Assert.Same(count, count.GetMembers("Compare").OfType<IMethodSymbol>().Single().Parameters.Single().Type);
        Assert.Same(count, count.GetMembers("Add").OfType<IMethodSymbol>().Single().ReturnType);
        var consumer = compilation.GetTypeByMetadataName("Consumer")!;
        Assert.Single(consumer.GetMembers("Sum").OfType<IMethodSymbol>().Single().TypeParameters);
        var copy = consumer.GetMembers("Copy").OfType<IMethodSymbol>().Single();
        Assert.Same(Assert.Single(copy.TypeParameters), copy.ReturnType);
        Assert.Equal("Cloneable", copy.TypeParameters[0].ConstraintTypes.Single().Name);
        AssertRequiresNativeBackend(compilation);
    }

    private static void AssertRequiresNativeBackend(Compilation compilation)
    {
        // Native signature emission is covered by NeoClrMetadataProbe's source-core gate.
        using var output = new MemoryStream();
        output.Write([1, 2, 3]);
        var result = compilation.Emit(output);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, diagnostic => diagnostic.Id == "RAVT003");
        Assert.Equal(new byte[] { 1, 2, 3 }, output.ToArray());
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void ErasedSelfCallAndWrongImplementationAreRejected()
    {
        foreach (var source in new[] {
            "interface Cloneable { func Clone() -> Self } func Bad(value: Cloneable) { value.Clone() }",
            Source.Replace("public static func Add(left: Self, right: Self) -> Self", "public static func Add(left: Self, right: Self) -> string").Replace("return result", "return \"wrong\"")
        })
        {
            var options = SelfOptions()
            .WithAsyncCancellationPropagation(true);
            var compilation = Compilation.Create("NativeSelfProbe", [SyntaxTree.ParseText(source)], References(), options);
            Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        }
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetRejectsConfiguredSelfBeforeLoadingReferencesOrWritingOutput()
    {
        var options = CompilationOptions.DotNet.WithRuntimeSelfTypeContract(new("Missing", "Self"));
        var compilation = Compilation.Create("InvalidSelf", [], [], options);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal("RAVT003", diagnostic.Id);
        Assert.Contains("native Self requires TargetPlatform.NeoCLR", diagnostic.GetMessage());
        using var output = new MemoryStream();
        output.Write([1, 2, 3]);
        Assert.False(compilation.Emit(output).Success);
        Assert.Equal(new byte[] { 1, 2, 3 }, output.ToArray());
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetSemanticQueriesKeepUserDefinedSelfOrdinaryEvenWithInvalidContract()
    {
        var tree = SyntaxTree.ParseText("class Self {}\nclass Example { val Value: Self { get; } }");
        var compilation = Compilation.Create("OrdinarySelf", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithRuntimeSelfTypeContract(new("Missing", "Marker")));
        var identifier = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "Self");
        var type = compilation.GetSemanticModel(tree).GetTypeInfo(identifier).Type;
        Assert.Equal("Self", type?.Name);
        Assert.Equal("OrdinarySelf", type?.ContainingAssembly?.Name);
        Assert.False(compilation.HasNativeSelfContract);
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClrWithoutSelfMappingKeepsUserDefinedSelfOrdinary(bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText("class Self {}\nclass Example { val Value: Self { get; } }");
        var compilation = Compilation.Create("OrdinaryNeoSelf", [tree], References(),
            SelfOptions().WithRuntimeSelfTypeContract(null));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var identifier = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "Self");
        var type = compilation.GetSemanticModel(tree).GetTypeInfo(identifier).Type;
        Assert.Equal("Self", type?.Name);
        Assert.Equal("OrdinaryNeoSelf", type?.ContainingAssembly?.Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void OrdinaryClrCompilationDoesNotEnableNativeSelf()
    {
        var compilation = Compilation.Create("Ordinary", [SyntaxTree.ParseText(Source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
