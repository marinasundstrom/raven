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

        var valueProperty = caseType.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance)!;
        var labelProperty = caseType.GetProperty("Label", BindingFlags.Public | BindingFlags.Instance)!;

        Assert.False(valueProperty.CanWrite);
        Assert.False(labelProperty.CanWrite);
        Assert.Equal(42, (int)valueProperty.GetValue(caseValue)!);
        Assert.Equal("ok", (string)labelProperty.GetValue(caseValue)!);
    }

    [Fact]
    public void DiscriminatedUnionStruct_AnnotatedWithMarkerAttribute()
    {
        var code = """
union Option {
    Some(value: int)
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
        var unionType = runtimeAssembly.GetType("Option", throwOnError: true)!;

        Assert.Contains(
            unionType.GetCustomAttributesData(),
            a => a.AttributeType.FullName == "System.Runtime.CompilerServices.DiscriminatedUnionAttribute");
    }

    [Fact]
    public void DiscriminatedUnionCaseTypes_AnnotatedWithMarkerAttribute()
    {
        var code = """
union Option {
    None
    Some(value: int)
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
        var unionType = runtimeAssembly.GetType("Option", throwOnError: true)!;

        var caseTypes = unionType.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotEmpty(caseTypes);

        foreach (var caseType in caseTypes)
        {
            var attribute = Assert.Single(caseType
                .GetCustomAttributesData()
                .Where(a => a.AttributeType.FullName == "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute"));

            Assert.Single(attribute.ConstructorArguments);
            Assert.Equal(unionType, attribute.ConstructorArguments[0].Value);
        }
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

        var valueProperty = caseType.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.Equal(42, (int)valueProperty.GetValue(payload)!);
    }

    [Fact]
    public void DiscriminatedUnion_EmitsImplicitConversionOperators()
    {
        var code = """
union Option {
    None
    Some(value: int)
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
        var unionType = runtimeAssembly.GetType("Option", throwOnError: true)!;
        var caseType = unionType.GetNestedType("Some", BindingFlags.Public | BindingFlags.NonPublic)!;

        var conversionMethod = unionType.GetMethod(
            "op_Implicit",
            BindingFlags.Public | BindingFlags.Static,
            binder: null,
            types: new[] { caseType },
            modifiers: null)!;

        var ctor = caseType.GetConstructor(new[] { typeof(int) })!;
        var caseInstance = ctor.Invoke(new object?[] { 7 });

        var unionValue = conversionMethod.Invoke(null, new[] { caseInstance });
        Assert.NotNull(unionValue);

        var tagField = unionType.GetField("<Tag>", BindingFlags.Instance | BindingFlags.NonPublic)!;
        var payloadField = unionType.GetField("<Payload>", BindingFlags.Instance | BindingFlags.NonPublic)!;

        Assert.Equal(1, (int)tagField.GetValue(unionValue)!);

        var payload = payloadField.GetValue(unionValue);
        Assert.NotNull(payload);
        Assert.Equal(caseType, payload!.GetType());
    }

    [Fact]
    public void DiscriminatedUnion_EmitsTryGetMethods()
    {
        var code = """
union Result {
    Ok(value: int)
    Error(message: string)
}

class Container {
    public GetOk() -> Result {
        return Result.Ok(value: 7)
    }

    public GetError() -> Result {
        return Result.Error(message: "boom")
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
        var unionType = runtimeAssembly.GetType("Result", throwOnError: true)!;
        var okCaseType = unionType.GetNestedType("Ok", BindingFlags.Public | BindingFlags.NonPublic)!;
        var errorCaseType = unionType.GetNestedType("Error", BindingFlags.Public | BindingFlags.NonPublic)!;
        var okTryGetMethod = unionType.GetMethod(
            "TryGetOk",
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            types: new[] { okCaseType.MakeByRefType() },
            modifiers: null)!;
        var errorTryGetMethod = unionType.GetMethod(
            "TryGetError",
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            types: new[] { errorCaseType.MakeByRefType() },
            modifiers: null)!;

        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var container = Activator.CreateInstance(containerType)!;
        var getOkMethod = containerType.GetMethod("GetOk", BindingFlags.Public | BindingFlags.Instance)!;
        var getErrorMethod = containerType.GetMethod("GetError", BindingFlags.Public | BindingFlags.Instance)!;

        var okUnionValue = getOkMethod.Invoke(container, Array.Empty<object?>());
        var okArgs = new object?[] { Activator.CreateInstance(okCaseType)! };
        var okResult = (bool)okTryGetMethod.Invoke(okUnionValue, okArgs)!;
        Assert.True(okResult);
        var okValueProperty = okCaseType.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.Equal(7, (int)okValueProperty.GetValue(okArgs[0])!);

        var errorUnionValue = getErrorMethod.Invoke(container, Array.Empty<object?>());
        var errorArgs = new object?[] { Activator.CreateInstance(errorCaseType)! };
        var errorResult = (bool)errorTryGetMethod.Invoke(errorUnionValue, errorArgs)!;
        Assert.True(errorResult);
        var errorMessageProperty = errorCaseType.GetProperty("Message", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.Equal("boom", (string)errorMessageProperty.GetValue(errorArgs[0])!);

        var mismatchArgs = new object?[] { Activator.CreateInstance(errorCaseType)! };
        var mismatchResult = (bool)errorTryGetMethod.Invoke(okUnionValue, mismatchArgs)!;
        Assert.False(mismatchResult);
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

    [Fact]
    public void GenericDiscriminatedUnionConversion_ClosesTypeArguments()
    {
        var code = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

class Container {
    public static CreateOk() -> Result<int> {
        return .Ok(99)
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
        var createMethod = containerType.GetMethod("CreateOk", BindingFlags.Public | BindingFlags.Static)!;

        var unionValue = createMethod.Invoke(null, Array.Empty<object?>());
        Assert.NotNull(unionValue);

        var unionTypeDefinition = runtimeAssembly.GetType("Result`1", throwOnError: true)!;
        var caseTypeDefinition = unionTypeDefinition.GetNestedType("Ok", BindingFlags.Public | BindingFlags.NonPublic)!;
        var closedUnionType = unionTypeDefinition.MakeGenericType(typeof(int));
        var closedCaseType = caseTypeDefinition.MakeGenericType(typeof(int));

        Assert.Equal(closedUnionType, unionValue!.GetType());

        var conversionMethod = closedUnionType.GetMethod(
            "op_Implicit",
            BindingFlags.Public | BindingFlags.Static,
            binder: null,
            types: new[] { closedCaseType },
            modifiers: null)!;

        var ctor = closedCaseType.GetConstructor(new[] { typeof(int) })!;
        var caseInstance = ctor.Invoke(new object?[] { 7 });

        var converted = conversionMethod.Invoke(null, new[] { caseInstance });
        Assert.NotNull(converted);
        Assert.Equal(closedUnionType, converted!.GetType());
    }

    [Fact]
    public void UnionCaseToString_FormatsUnionNameAndParameters()
    {
        var code = """
union Shape {
    Rectangle(width: int, height: int)
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
        var unionType = runtimeAssembly.GetType("Shape", throwOnError: true)!;
        var caseType = unionType.GetNestedType("Rectangle", BindingFlags.Public | BindingFlags.NonPublic)!;

        var ctor = caseType.GetConstructor(new[] { typeof(int), typeof(int) })!;
        var caseInstance = ctor.Invoke(new object?[] { 3, 6 });
        var toString = caseType.GetMethod("ToString", BindingFlags.Public | BindingFlags.Instance)!;

        var text = (string)toString.Invoke(caseInstance, Array.Empty<object?>())!;
        Assert.Equal("Shape.Rectangle(width=3, height=6)", text);
    }

    [Fact]
    public void UnionToString_IndicatesActiveCase()
    {
        var code = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

class Container {
    public static Create() -> Result<int> {
        return .Ok(5)
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
        var createMethod = containerType.GetMethod("Create", BindingFlags.Public | BindingFlags.Static)!;

        var unionValue = createMethod.Invoke(null, Array.Empty<object?>());
        Assert.NotNull(unionValue);

        var unionTypeDefinition = runtimeAssembly.GetType("Result`1", throwOnError: true)!;
        var closedUnionType = unionTypeDefinition.MakeGenericType(typeof(int));
        Assert.Equal(closedUnionType, unionValue!.GetType());

        var toString = closedUnionType.GetMethod("ToString", BindingFlags.Public | BindingFlags.Instance)!;
        var text = (string)toString.Invoke(unionValue, Array.Empty<object?>())!;

        Assert.Equal("Result<int>.Ok(value=5)", text);
    }
}
