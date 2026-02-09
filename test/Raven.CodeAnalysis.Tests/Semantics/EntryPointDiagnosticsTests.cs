using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class EntryPointDiagnosticsTests
{
    [Fact(Skip = "Requires reference assemblies in this environment")]
    public void ConsoleApp_WithoutMain_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText("");
        var compilation = Compilation.Create("app", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);
    }

    [Fact]
    public void ConsoleApp_WithMultipleMainMethods_ProducesAmbiguousDiagnostic()
    {
        var code = """
class Program {
    static Main() -> unit {
        return;
    }
}

class Helper {
    static Main() -> unit {
        return;
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("app", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.EntryPointIsAmbiguous);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);
    }

    [Fact]
    public void TopLevelStatements_WithUserDefinedMain_ProducesAmbiguousDiagnostic()
    {
        var topLevel = SyntaxTree.ParseText("val x = 0");
        var mainClass = SyntaxTree.ParseText("""
class App {
    static Main() -> unit {
        return;
    }
}
""");

        var compilation = Compilation.Create("app", new[] { topLevel, mainClass }, TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.EntryPointIsAmbiguous);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);
    }

    [Fact]
    public void ConsoleApp_WithValueTaskMain_ProducesInvalidSignatureDiagnostic()
    {
        var code = """
import System.Threading.Tasks.*

class Program {
    static Main() -> ValueTask {
        return default(ValueTask);
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.EntryPointHasInvalidSignature);

        var entryPoint = compilation.GetEntryPoint();
        Assert.Null(entryPoint);
    }

    [Fact]
    public void ConsoleApp_WithTaskMain_SynthesizesBridge()
    {
        var code = """
import System.Threading.Tasks.*

class Program {
    static Main() -> Task {
        return Task.CompletedTask;
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetEntryPointDiagnostics();
        Assert.True(diagnostics.IsDefaultOrEmpty, string.Join(Environment.NewLine, diagnostics));

        var allDiagnostics = compilation.GetDiagnostics();
        Assert.True(allDiagnostics.IsDefaultOrEmpty, string.Join(Environment.NewLine, allDiagnostics));

        Assert.NotEmpty(compilation.SourceGlobalNamespace.GetMembers());

        var methods = compilation.SourceGlobalNamespace
            .GetAllMembersRecursive()
            .OfType<IMethodSymbol>()
            .ToArray();
        var main = Assert.Single(methods, m => m.Name == EntryPointSignature.EntryPointName);

        Assert.True(EntryPointSignature.HasValidReturnType(main.ReturnType, compilation));
        Assert.True(EntryPointSignature.HasValidParameters(main.Parameters, compilation));
        Assert.True(main.IsStatic);
        Assert.False(main.IsGenericMethod);
        Assert.True(main.TypeParameters.IsDefaultOrEmpty);

        var candidates = methods.Where(compilation.IsEntryPointCandidate).ToArray();
        Assert.Single(candidates);

        var entryPoint = compilation.GetEntryPoint();

        var bridge = Assert.IsType<SynthesizedEntryPointBridgeMethodSymbol>(entryPoint);
        Assert.Equal(SpecialType.System_Unit, bridge.ReturnType.SpecialType);
        Assert.Equal(SymbolKind.Method, bridge.AsyncImplementation.Kind);
    }

    [Fact]
    public void WindowsApp_WithTaskOfIntMain_SynthesizesBridge()
    {
        var code = """
import System.Threading.Tasks.*

class Program {
    static Main() -> Task<int> {
        return Task.FromResult(0);
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.WindowsApplication));

        var diagnostics = compilation.GetEntryPointDiagnostics();
        Assert.True(diagnostics.IsDefaultOrEmpty, string.Join(Environment.NewLine, diagnostics));

        var allDiagnostics = compilation.GetDiagnostics();
        Assert.True(allDiagnostics.IsDefaultOrEmpty, string.Join(Environment.NewLine, allDiagnostics));

        Assert.NotEmpty(compilation.SourceGlobalNamespace.GetMembers());

        var methods = compilation.SourceGlobalNamespace
            .GetAllMembersRecursive()
            .OfType<IMethodSymbol>()
            .ToArray();
        Assert.Contains(methods, m => m.Name == EntryPointSignature.EntryPointName);

        var entryPoint = compilation.GetEntryPoint();

        var bridge = Assert.IsType<SynthesizedEntryPointBridgeMethodSymbol>(entryPoint);
        Assert.Equal(SpecialType.System_Int32, bridge.ReturnType.SpecialType);
        Assert.Equal(SymbolKind.Method, bridge.AsyncImplementation.Kind);
    }

    [Fact]
    public void ClassLibrary_DoesNotSynthesizeTopLevelProgram()
    {
        var tree = SyntaxTree.ParseText("");
        var compilation = Compilation.Create(
            "lib",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);

        var global = compilation.SourceGlobalNamespace;
        Assert.NotNull(global);

        var program = global.LookupType("Program");
        Assert.Null(program);
    }

    [Fact]
    public void TopLevelStatements_SynthesizeImplicitEntryPoint()
    {
        var tree = SyntaxTree.ParseText("val x = 0");
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var entryPoint = compilation.GetEntryPoint();

        var method = Assert.IsAssignableFrom<IMethodSymbol>(entryPoint);
        Assert.True(method.IsImplicitlyDeclared);
        Assert.True(method.CanBeReferencedByName);
    }
}
