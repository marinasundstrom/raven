using System;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public class TypeMetadataNameTests
{
    [Fact]
    public void ToFullyQualifiedMetadataName_IncludesGenericArity()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var actionDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Action`1")!;

        Assert.Equal("System.Action`1", actionDefinition.ToFullyQualifiedMetadataName());
    }

    [Fact]
    public void GetClrType_ResolvesConstructedGenericFromMetadata()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var actionDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Action`1")!;
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructed = compilation.ConstructGenericType(actionDefinition, new ITypeSymbol[] { stringType });

        var clrType = constructed.GetClrType(compilation);

        Assert.Equal(typeof(Action<string>), clrType);
    }

    [Fact]
    public void ResolveMetadataType_UsesMetadataLoadContext()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var metadataType = compilation.ResolveMetadataType("System.String");

        Assert.NotNull(metadataType);
        Assert.NotSame(compilation.RuntimeCoreAssembly, metadataType!.Assembly);
        Assert.Equal("System.String", metadataType.FullName);
    }

    [Fact]
    public void ResolveType_UsesMetadataAssemblyForRuntimeTypes()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var runtimeTypeSymbol = compilation.GetType(typeof(string));
        var metadataSymbol = compilation.GetTypeByMetadataName("System.String");

        Assert.NotNull(runtimeTypeSymbol);
        Assert.NotNull(metadataSymbol);
        Assert.Same(metadataSymbol!.ContainingAssembly, runtimeTypeSymbol!.ContainingAssembly);
    }
}
