using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class VirtualMemberTests : CompilationTestBase
{
    [Fact]
    public void OverrideWithStrongerReferenceReturnNullability_IsAccepted()
    {
        const string source = """
record ItemId(Value: int) {
    override func ToString() -> string => Value.ToString()
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var type = compilation.GetTypeByMetadataName("ItemId")!;
        var method = Assert.IsType<SourceMethodSymbol>(Assert.Single(type.GetMembers("ToString").OfType<IMethodSymbol>().Where(m => !m.IsImplicitlyDeclared)));
        Assert.NotNull(method.OverriddenMethod);
        Assert.True(method.OverriddenMethod!.ReturnType.IsNullable);
        Assert.False(method.ReturnType.IsNullable);
    }

    [Fact]
    public void OverrideWithWeakerReferenceReturnNullability_IsRejected()
    {
        const string source = """
open class Base {
    virtual func Value() -> string => "base"
}
class Derived : Base {
    override func Value() -> string? => null
}
""";
        var compilation = CreateCompilation(SyntaxTree.ParseText(source), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.OverrideMemberNotFound);
    }

    [Fact]
    public void OverrideOfNullableUnconstrainedGenericReturningValueType_UsesUnderlyingAbiType()
    {
        const string source = """
open class Base<T> {
    virtual func GetValue() -> T? => default(T)
}

record struct Payload(Value: int)

class Derived : Base<Payload> {
    override func GetValue() -> Payload => Payload(42)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var derived = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Derived"));
        var method = Assert.IsType<SourceMethodSymbol>(Assert.Single(derived.GetMembers("GetValue").OfType<IMethodSymbol>()));
        Assert.NotNull(method.OverriddenMethod);
        Assert.Equal(NullableAbiProjection.AnnotatedUnderlyingType, method.OverriddenMethod!.ReturnType.GetNullableAbiProjection());
        Assert.Equal(NullableAbiProjection.None, method.ReturnType.GetNullableAbiProjection());
    }

    [Fact]
    public void OverrideOfNullableUnconstrainedGenericReturningNullableValueType_ProducesDiagnostic()
    {
        const string source = """
open class Base<T> {
    virtual func GetValue() -> T? => default(T)
}

record struct Payload(Value: int)

class Derived : Base<Payload> {
    override func GetValue() -> Payload? => Payload(42)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OverrideMemberNotFound);
    }

    [Fact]
    public void VirtualMethodOnSealedType_ProducesDiagnostic()
    {
        const string source = """
class C {
    virtual func M() -> unit {
        return
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal(CompilerDiagnostics.VirtualMemberInClosedType.Id, diagnostic.Descriptor.Id);
    }

    [Fact]
    public void SealedModifierWithoutOverride_ProducesDiagnostic()
    {
        const string source = """
class C {
    sealed func M() -> unit {
        return
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV0309", diagnostic.Descriptor.Id);
    }

    [Fact]
    public void OverrideSealedMethod_ProducesDiagnostic()
    {
        const string source = """
open class Animal {
    virtual func Speak() -> unit {
        return
    }
}

open class Dog : Animal {
    sealed override func Speak() -> unit {
        return
    }
}

class Puppy : Dog {
    override func Speak() -> unit {
        return
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor.Id == "RAV0310");
    }

    [Fact]
    public void StaticOverride_ProducesDiagnostic()
    {
        const string source = """
open class Animal {
    virtual func Speak() -> unit {
        return
    }
}

class Dog : Animal {
    static override func Speak() -> unit {
        return
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV0311", diagnostic.Descriptor.Id);
    }

    [Fact]
    public void StaticVirtual_ProducesDiagnostic()
    {
        const string source = """
class C {
    static virtual func M() -> unit {
        return
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV0311", diagnostic.Descriptor.Id);
    }

    [Fact]
    public void SealedPropertyOverride_ProducesDiagnostic()
    {
        const string source = """
open class Animal {
    virtual val Name: string {
        get { return "animal" }
    }
}

open class Dog : Animal {
    sealed override val Name: string {
        get { return "dog" }
    }
}

class Puppy : Dog {
    override val Name: string {
        get { return "puppy" }
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor.Id == "RAV0310");
    }

    [Fact]
    public void SealedPropertyWithoutOverride_ProducesDiagnostic()
    {
        const string source = """
class C {
    sealed val Value: int {
        get { return 0 }
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV0309", diagnostic.Descriptor.Id);
    }
}
