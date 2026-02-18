using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

internal static class TestMetadataFactory
{
    public static MetadataReference CreateFromSource(
        string source,
        string assemblyName,
        CompilationOptions? options = null)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            assemblyName,
            [tree],
            TestMetadataReferences.Default,
            options ?? new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        return MetadataReference.CreateFromImage(peStream.ToArray());
    }

    public static MetadataReference CreateFileReferenceFromSource(
        string source,
        string assemblyName,
        CompilationOptions? options = null)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            assemblyName,
            [tree],
            TestMetadataReferences.Default,
            options ?? new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        var outputDirectory = Path.Combine(Path.GetTempPath(), $"raven-test-metadata-{Guid.NewGuid():N}");
        Directory.CreateDirectory(outputDirectory);
        var assemblyPath = Path.Combine(outputDirectory, $"{assemblyName}.dll");
        File.WriteAllBytes(assemblyPath, peStream.ToArray());
        return MetadataReference.CreateFromFile(assemblyPath);
    }
}
