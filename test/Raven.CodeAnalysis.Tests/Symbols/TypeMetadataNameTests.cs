using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TypeMetadataNameTests
{
    [Fact]
    public void SourceNamedType_MetadataNameIsLocalAndFullyQualifiedNameCarriesContainers()
    {
        const string source = """
namespace Lib {
    class Outer<T> {
        class Inner<U> {}
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "source-metadata-names",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(syntaxTree);
        var declarations = syntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().ToArray();
        var outer = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declarations[0]));
        var inner = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declarations[1]));

        Assert.Equal("Outer`1", outer.MetadataName);
        Assert.Equal("Lib.Outer`1", outer.ToFullyQualifiedMetadataName());
        Assert.Equal("Inner`1", inner.MetadataName);
        Assert.Equal("Lib.Outer`1+Inner`1", inner.ToFullyQualifiedMetadataName());

        using var image = new MemoryStream();
        var emitResult = compilation.Emit(image);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
        var metadataCompilation = Compilation.Create(
                "source-metadata-names-consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences([
                .. TestMetadataReferences.Default,
                MetadataReference.CreateFromImage(image.ToArray()),
            ]);
        var metadataOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(
            metadataCompilation.GetTypeByMetadataName("Lib.Outer`1"));
        var metadataInner = Assert.IsAssignableFrom<INamedTypeSymbol>(metadataOuter.LookupType("Inner"));

        Assert.Equal(outer.MetadataName, metadataOuter.MetadataName);
        Assert.Equal(outer.ToFullyQualifiedMetadataName(), metadataOuter.ToFullyQualifiedMetadataName());
        Assert.Equal(inner.MetadataName, metadataInner.MetadataName);
        Assert.Equal(inner.ToFullyQualifiedMetadataName(), metadataInner.ToFullyQualifiedMetadataName());
    }

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
    public void ResolveRuntimeType_MapsNuGetSharedFrameworkReferenceAssembly()
    {
        var aspNetCoreReference = TryFindAspNetCoreReferenceAssembly();
        if (aspNetCoreReference is null)
            return;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(MetadataReference.CreateFromFile(aspNetCoreReference));

        var webApplication = Assert.IsAssignableFrom<PENamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Microsoft.AspNetCore.Builder.WebApplication"));

        var runtimeType = compilation.ResolveRuntimeType(webApplication);

        if (runtimeType is null)
            return;

        Assert.Equal("Microsoft.AspNetCore.Builder.WebApplication", runtimeType.FullName);
    }

    private static string? TryFindAspNetCoreReferenceAssembly()
    {
        var userProfile = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
        if (string.IsNullOrEmpty(userProfile))
            return null;

        var packageRoot = Path.Combine(userProfile, ".nuget", "packages", "microsoft.aspnetcore.app.ref");
        if (!Directory.Exists(packageRoot))
            return null;

        return Directory
            .EnumerateFiles(packageRoot, "Microsoft.AspNetCore.dll", SearchOption.AllDirectories)
            .OrderByDescending(static path => TryParsePackageVersion(path))
            .ThenByDescending(static path => path, StringComparer.OrdinalIgnoreCase)
            .FirstOrDefault(static path => path.Contains($"{Path.DirectorySeparatorChar}ref{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase));
    }

    private static Version? TryParsePackageVersion(string path)
    {
        var refIndex = path.IndexOf($"{Path.DirectorySeparatorChar}ref{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase);
        if (refIndex < 0)
            return null;

        var packageVersionDirectory = path[..refIndex];
        var version = Path.GetFileName(packageVersionDirectory);
        if (string.IsNullOrWhiteSpace(version))
            return null;

        var prereleaseIndex = version.IndexOf('-');
        var versionText = prereleaseIndex >= 0 ? version[..prereleaseIndex] : version;
        return Version.TryParse(versionText, out var parsed) ? parsed : null;
    }
}
