using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class VirtualMemberTests : CompilationTestBase
{
    [Fact]
    public void VirtualMethodOnSealedType_ProducesDiagnostic()
    {
        const string source = """
class C {
    public virtual M() -> unit {
        return
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal(CompilerDiagnostics.VirtualMemberInSealedType.Id, diagnostic.Descriptor.Id);
    }

    [Fact]
    public void SealedModifierWithoutOverride_ProducesDiagnostic()
    {
        const string source = """
class C {
    public sealed M() -> unit {
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
    public virtual Speak() -> unit {
        return
    }
}

open class Dog : Animal {
    public sealed override Speak() -> unit {
        return
    }
}

class Puppy : Dog {
    public override Speak() -> unit {
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
    public virtual Speak() -> unit {
        return
    }
}

class Dog : Animal {
    public static override Speak() -> unit {
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
    public static virtual M() -> unit {
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
    public virtual Name: string { get => "animal" }
}

open class Dog : Animal {
    public sealed override Name: string { get => "dog" }
}

class Puppy : Dog {
    public override Name: string { get => "puppy" }
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
    public sealed Value: int { get => 0 }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV0309", diagnostic.Descriptor.Id);
    }
}
