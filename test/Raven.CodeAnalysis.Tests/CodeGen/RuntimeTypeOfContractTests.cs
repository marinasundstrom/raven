using System;
using System.IO;
using System.Linq;
using System.Runtime.Loader;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class RuntimeTypeOfContractTests
{
    private static readonly RuntimeTypeOfContract Contract = new("test", "Contracts.Info", "Contracts.Context");

    private static Compilation Create(string operand, RuntimeTypeOfContract contract)
    {
        var tree = SyntaxTree.ParseText($$"""
namespace Contracts
import System.*

public interface Info {
    val Name: string { get; }
}

internal class Descriptor : Info {
    private val type: Type
    init(type: Type) {
        self.type = type
    }
    val Name: string { get => type.Name }
}

public class Context {
    static val Current: Context { get => Context() }
    func GetTypeInfoFromHandle(handle: RuntimeTypeHandle) -> Info {
        return Descriptor(Type.GetTypeFromHandle(handle)!)
    }
}

public class Sample {
    func Run() -> Info {
        return typeof({{operand}})
    }
}
""");
        return Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeTypeOfContract(contract))
            .AddSyntaxTrees(tree).AddReferences(TestMetadataReferences.Default);
    }

    [Theory]
    [InlineData("int", "Int32")]
    [InlineData("string[]", "String[]")]
    [InlineData("System.Collections.Generic.List<>", "List`1")]
    public void TypeOf_UsesContextAndReturnsInterface(string operand, string expected)
    {
        var compilation = Create(operand, Contract);
        var tree = compilation.SyntaxTrees.Single();
        var expression = tree.GetRoot().DescendantNodes().OfType<TypeOfExpressionSyntax>().Single();
        Assert.Equal("Info", compilation.GetSemanticModel(tree).GetTypeInfo(expression).Type!.Name);
        Assert.Equal("Info", compilation.GetSemanticModel(tree).GetOperation(expression)!.Type!.Name);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        var sample = loaded.Assembly.GetType("Contracts.Sample", true)!;
        var method = sample.GetMethod("Run")!;
        Assert.True(method.ReturnType.IsInterface);
        var value = method.Invoke(Activator.CreateInstance(sample), null)!;
        Assert.False(value.GetType().IsPublic);
        Assert.Equal(expected, method.ReturnType.GetProperty("Name")!.GetValue(value));
    }

    [Theory]
    [InlineData("missing", "Contracts.Info", "Contracts.Context")]
    [InlineData("test", "Contracts.Missing", "Contracts.Context")]
    [InlineData("test", "Contracts.Info", "Contracts.Sample")]
    public void InvalidContract_ReportsDiagnosticWithoutFallback(string assembly, string info, string context)
    {
        var compilation = Create("int", new(assembly, info, context));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT003");
        using var stream = new MemoryStream();
        Assert.False(compilation.Emit(stream).Success);
    }

    [Fact]
    public void ReferencedProvider_ExecutesWithoutExposingImplementation()
    {
        using var provider = new MemoryStream();
        var providerResult = Create("int", Contract).Emit(provider);
        Assert.True(providerResult.Success, string.Join(Environment.NewLine, providerResult.Diagnostics));
        var client = Compilation.Create("client", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeTypeOfContract(Contract))
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(MetadataReference.CreateFromImage(provider.ToArray()))
            .AddSyntaxTrees(SyntaxTree.ParseText("""
public class Client {
    func Run() -> Contracts.Info {
        return typeof(int)
    }
}
"""));
        using var output = new MemoryStream();
        var result = client.Emit(output);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var context = new AssemblyLoadContext("typeof-contract", isCollectible: true);
        try
        {
            provider.Position = 0;
            context.LoadFromStream(provider);
            output.Position = 0;
            var assembly = context.LoadFromStream(output);
            var type = assembly.GetType("Client", true)!;
            var method = type.GetMethod("Run")!;
            var value = method.Invoke(Activator.CreateInstance(type), null)!;
            Assert.True(method.ReturnType.IsInterface);
            Assert.False(value.GetType().IsPublic);
            Assert.Equal("Int32", method.ReturnType.GetProperty("Name")!.GetValue(value));
        }
        finally
        {
            context.Unload();
        }
    }

    [Fact]
    public void OptionsCopies_PreserveContract()
    {
        var options = new CompilationOptions().WithRuntimeTypeOfContract(Contract)
            .WithOptimizationLevel(OptimizationLevel.Release).WithAllowArrayCovariance(false)
            .WithRuntimeUnitContract(null).WithTargetCoreAssemblyName(null);
        Assert.Equal(Contract, options.RuntimeTypeOfContract);
        Assert.Null(options.WithRuntimeTypeOfContract(null).RuntimeTypeOfContract);
    }

    [Theory]
    [InlineData("static val Current", "val Current")]
    [InlineData("func GetTypeInfoFromHandle", "static func GetTypeInfoFromHandle")]
    [InlineData("handle: RuntimeTypeHandle", "handle: int")]
    [InlineData("public interface Info", "internal interface Info")]
    public void MalformedProvider_ReportsConfigurationDiagnostic(string before, string after)
    {
        var original = Create("int", Contract);
        var tree = SyntaxTree.ParseText(original.SyntaxTrees.Single().GetText().ToString().Replace(before, after));
        var compilation = Compilation.Create("test", original.Options)
            .AddSyntaxTrees(tree).AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT003");
    }
}
