using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class DiscriminatedUnionCodeGenTests
{
    [Fact]
    public void UnionCaseConstructor_AssignsFields()
    {
        var code = """
union Option {
    Some(value: int, label: string)
}

class Container {
    public Create() -> Option.Some {
        return Option.Some(value: 42, label: "ok")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var createMethod = containerType.GetMethod("Create", BindingFlags.Public | BindingFlags.Instance)!;
        var instance = Activator.CreateInstance(containerType)!;

        var caseValue = createMethod.Invoke(instance, Array.Empty<object?>());
        Assert.NotNull(caseValue);

        var unionType = runtimeAssembly.GetType("Option", throwOnError: true)!;
        var caseType = unionType.GetNestedType("Some", BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(caseType, caseValue!.GetType());

        var valueField = caseType.GetField("<value>k__BackingField", BindingFlags.NonPublic | BindingFlags.Instance)!;
        var labelField = caseType.GetField("<label>k__BackingField", BindingFlags.NonPublic | BindingFlags.Instance)!;

        Assert.Equal(42, (int)valueField.GetValue(caseValue)!);
        Assert.Equal("ok", (string)labelField.GetValue(caseValue)!);

        var valueProperty = caseType.GetProperty("value", BindingFlags.Public | BindingFlags.Instance)!;
        var labelProperty = caseType.GetProperty("label", BindingFlags.Public | BindingFlags.Instance)!;

        Assert.False(valueProperty.CanWrite);
        Assert.False(labelProperty.CanWrite);
        Assert.Equal(42, (int)valueProperty.GetValue(caseValue)!);
        Assert.Equal("ok", (string)labelProperty.GetValue(caseValue)!);
    }

    [Fact]
    public void DiscriminatedUnionConversion_SetsTagAndPayload()
    {
        var code = """
union Option {
    None
    Some(value: int)
}

class Container {
    public Create() -> Option {
        return Option.Some(value: 42)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var createMethod = containerType.GetMethod("Create", BindingFlags.Public | BindingFlags.Instance)!;
        var instance = Activator.CreateInstance(containerType)!;

        var unionValue = createMethod.Invoke(instance, Array.Empty<object?>());
        Assert.NotNull(unionValue);

        var unionType = runtimeAssembly.GetType("Option", throwOnError: true)!;
        var tagField = unionType.GetField("<Tag>", BindingFlags.Instance | BindingFlags.NonPublic)!;
        var payloadField = unionType.GetField("<Payload>", BindingFlags.Instance | BindingFlags.NonPublic)!;

        Assert.Equal(1, (int)tagField.GetValue(unionValue)!);

        var payload = payloadField.GetValue(unionValue);
        Assert.NotNull(payload);

        var caseType = unionType.GetNestedType("Some", BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(caseType, payload!.GetType());

        var valueProperty = caseType.GetProperty("value", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.Equal(42, (int)valueProperty.GetValue(payload)!);
    }

    [Fact]
    public void GenericUnionCaseConstruction_PreservesOuterTypeArguments()
    {
        var code = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

class Container {
    public Create() -> Result<int>.Error {
        return Result<int>.Error(message: "boom")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var createMethod = containerType.GetMethod("Create", BindingFlags.Public | BindingFlags.Instance)!;
        var instance = Activator.CreateInstance(containerType)!;

        var caseValue = createMethod.Invoke(instance, Array.Empty<object?>());
        Assert.NotNull(caseValue);

        var unionTypeDefinition = runtimeAssembly.GetType("Result`1", throwOnError: true)!;
        var errorTypeDefinition = unionTypeDefinition.GetNestedType("Error", BindingFlags.Public | BindingFlags.NonPublic)!;
        var closedCaseType = errorTypeDefinition.MakeGenericType(typeof(int));

        Assert.Equal(closedCaseType, caseValue!.GetType());
        Assert.Collection(
            caseValue.GetType().GetGenericArguments(),
            arg => Assert.Equal(typeof(int), arg));
    }
}
