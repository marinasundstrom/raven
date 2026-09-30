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

    private static MetadataReference[] References()
    {
        var marker = Compilation.Create("NativeSelfReference", [SyntaxTree.ParseText("public sealed class NativeSelfMarker {}")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        Assert.True(marker.Emit(image).Success);
        return TestMetadataReferences.Default.Append(MetadataReference.CreateFromImage(image.ToArray())).ToArray();
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
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeSelfTypeContract(new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker"));
        var compilation = Compilation.Create("SelfInheritance", [SyntaxTree.ParseText(source)], References(), options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("\n", diagnostics));
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        stream.Position = 0;
        using var module = ModuleDefinition.ReadModule(stream);
        Assert.Equal("Base", module.GetType("Base").Methods.Single(m => m.Name == "Clone").ReturnType.FullName);
        if (variant == 2)
            Assert.Equal("Derived", module.GetType("Derived").Methods.Single(m => m.HasOverrides).ReturnType.FullName);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void InheritedSelfDoesNotPromiseDerivedResult(bool inferred)
    {
        var source = InheritanceSource.Replace("Copy<Base>(value)", inferred ? "Copy(value)" : "Copy<Derived>(value)");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeSelfTypeContract(new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker"));
        var compilation = Compilation.Create("SelfInheritance", [SyntaxTree.ParseText(source)], References(), options);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RedeclaredSelfRejectsInheritedBaseResult(bool wrongExplicitResult)
    {
        var source = InheritanceSource.Replace("class Derived : Base {}", wrongExplicitResult
            ? "class Derived : Base, Cloneable { func Cloneable.Clone() -> Base => self }"
            : "class Derived : Base, Cloneable {}");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeSelfTypeContract(new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker"));
        var compilation = Compilation.Create("SelfInheritance", [SyntaxTree.ParseText(source)], References(), options);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
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
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeSelfTypeContract(new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker"));
        var compilation = Compilation.Create("SelfPropertyInheritance", [SyntaxTree.ParseText(source)], References(), options);
        Assert.Equal(redeclared, compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    public void NativeSelfKeepsInterfaceArityAndEmitsMarkerAndConstrainedCall()
    {
        var contract = new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithAllowNullableValueTypes(false)
            .WithRuntimeSelfTypeContract(contract)
            .WithAsyncCancellationPropagation(true);
        Assert.False(options.AllowNullableValueTypes);
        Assert.Same(contract, options.WithAllowNullableValueTypes(true).RuntimeSelfTypeContract);
        var compilation = Compilation.Create("NativeSelfProbe", [SyntaxTree.ParseText(Source)], References(), options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("\n", diagnostics));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        stream.Position = 0;
        using var module = ModuleDefinition.ReadModule(stream);
        var number = module.GetType("Number");
        Assert.Empty(number.GenericParameters);
        var count = module.GetType("Count");
        Assert.Contains(count.Interfaces, i => i.InterfaceType.FullName == "Comparable`1<Count>");
        Assert.True(count.Methods.Single(m => m.Name == "Compare").IsVirtual);
        Assert.Equal("NativeSelfMarker", number.Methods.Single(m => m.Name == "Add").ReturnType.FullName);
        Assert.Equal("Count", module.GetType("Count").Methods.Single(m => m.Name == "Add").ReturnType.FullName);
        Assert.Single(module.GetType("Consumer").Methods.Single(m => m.Name == "Sum").GenericParameters);
        var copy = module.GetType("Consumer").Methods.Single(m => m.Name == "Copy");
        Assert.Single(copy.GenericParameters);
        Assert.IsType<GenericParameter>(copy.ReturnType);
        Assert.Equal("Cloneable", copy.GenericParameters[0].Constraints.Single().ConstraintType.FullName);
    }

    [Fact]
    public void ErasedSelfCallAndWrongImplementationAreRejected()
    {
        foreach (var source in new[] {
            "interface Cloneable { func Clone() -> Self } func Bad(value: Cloneable) { value.Clone() }",
            Source.Replace("public static func Add(left: Self, right: Self) -> Self", "public static func Add(left: Self, right: Self) -> string").Replace("return result", "return \"wrong\"")
        })
        {
            var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeSelfTypeContract(new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker"))
            .WithAsyncCancellationPropagation(true);
            var compilation = Compilation.Create("NativeSelfProbe", [SyntaxTree.ParseText(source)], References(), options);
            Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        }
    }

    [Fact]
    public void OrdinaryClrCompilationDoesNotEnableNativeSelf()
    {
        var compilation = Compilation.Create("Ordinary", [SyntaxTree.ParseText(Source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
