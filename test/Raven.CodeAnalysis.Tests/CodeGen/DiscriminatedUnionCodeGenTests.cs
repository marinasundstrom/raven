using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;

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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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

        var caseType = runtimeAssembly.GetType("Option_Some", throwOnError: true)!;
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
    public void PublicUnionCases_AreEmittedAsPublicNestedStructs()
    {
        const string code = """
import System.*

public union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "public-union",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;

        var okCase = assembly.GetType("Result_Ok`1", throwOnError: true)!.MakeGenericType(typeof(int));
        Assert.True(okCase.IsPublic);
        var okCtor = okCase.GetConstructor(BindingFlags.Public | BindingFlags.Instance, binder: null, new[] { typeof(int) }, modifiers: null);
        Assert.NotNull(okCtor);

        var errorCase = assembly.GetType("Result_Error", throwOnError: true)!;
        Assert.True(errorCase.IsPublic);
        var errorCtor = errorCase.GetConstructor(BindingFlags.Public | BindingFlags.Instance, binder: null, new[] { typeof(string) }, modifiers: null);
        Assert.NotNull(errorCtor);
    }

    [Fact]
    public void InternalUnionCases_AreEmittedWithInternalAccessibility()
    {
        const string code = """
union Hidden<T> {
    Case(value: T)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "internal-union",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;

        var caseType = assembly.GetType("Hidden_Case`1", throwOnError: true)!.MakeGenericType(typeof(int));
        Assert.True(caseType.IsNotPublic);
        var ctor = caseType.GetConstructor(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance, binder: null, new[] { typeof(int) }, modifiers: null);
        Assert.NotNull(ctor);
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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
            a => a.AttributeType.FullName == "System.Runtime.CompilerServices.UnionAttribute");
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionType = runtimeAssembly.GetType("Option", throwOnError: true)!;
        var caseTypes = runtimeAssembly.GetTypes()
            .Where(type => type.GetCustomAttributesData().Any(a => a.AttributeType.FullName == "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute"))
            .ToArray();
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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
        var payloadField = unionType.GetField("<SomePayload>", BindingFlags.Instance | BindingFlags.NonPublic)!;

        Assert.Equal((byte)1, (byte)tagField.GetValue(unionValue)!);

        var payload = payloadField.GetValue(unionValue);
        Assert.NotNull(payload);

        var caseType = runtimeAssembly.GetType("Option_Some", throwOnError: true)!;
        Assert.Equal(caseType, payload!.GetType());

        var valueProperty = caseType.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.Equal(42, (int)valueProperty.GetValue(payload)!);
    }

    [Fact]
    public void UnitPayloadCaseShorthand_ConstructsCaseBeforeImplicitUnionConversion()
    {
        const string code = """
union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Container {
    public Make() -> Result<(), string> {
        .Ok
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "unit-case-shorthand",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var containerType = assembly.GetType("Container", throwOnError: true)!;
        var makeMethod = containerType.GetMethod("Make", BindingFlags.Public | BindingFlags.Instance)!;
        var instance = Activator.CreateInstance(containerType)!;

        var unionValue = makeMethod.Invoke(instance, Array.Empty<object?>());
        Assert.NotNull(unionValue);

        var unionType = unionValue!.GetType();
        Assert.Equal("Result`2", unionType.Name);
        Assert.Equal("Unit", unionType.GetGenericArguments()[0].Name);
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionType = runtimeAssembly.GetType("Option", throwOnError: true)!;
        var caseType = runtimeAssembly.GetType("Option_Some", throwOnError: true)!;

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
        var payloadField = unionType.GetField("<SomePayload>", BindingFlags.Instance | BindingFlags.NonPublic)!;

        Assert.Equal((byte)1, (byte)tagField.GetValue(unionValue)!);

        var payload = payloadField.GetValue(unionValue);
        Assert.NotNull(payload);
        Assert.Equal(caseType, payload!.GetType());
    }

    [Fact]
    public void DiscriminatedUnion_UsesExplicitLayoutAndOffsets()
    {
        var code = """
union Result {
    Ok(value: int)
    Error(message: string)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionType = runtimeAssembly.GetType("Result", throwOnError: true)!;

        var layout = unionType.StructLayoutAttribute;
        Assert.NotNull(layout);
        Assert.Equal(LayoutKind.Sequential, layout!.Value);
    }

    [Fact]
    public void DiscriminatedUnion_GenericUsesSequentialLayout()
    {
        var code = """
union Option<T> {
    Some(value: T)
    None
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionDefinition = runtimeAssembly.GetType("Option`1", throwOnError: true)!;
        var unionType = unionDefinition.MakeGenericType(typeof(int));

        var layout = unionType.StructLayoutAttribute;
        Assert.NotNull(layout);
        Assert.Equal(LayoutKind.Sequential, layout!.Value);
    }

    [Fact]
    public void DiscriminatedUnionConversion_DoesNotBoxValuePayload()
    {
        var code = """
union Option {
    Some(value: int)
    None
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionType = runtimeAssembly.GetType("Option", throwOnError: true)!;
        var caseType = runtimeAssembly.GetType("Option_Some", throwOnError: true)!;
        var conversionMethod = unionType.GetMethod(
            "op_Implicit",
            BindingFlags.Public | BindingFlags.Static,
            binder: null,
            types: new[] { caseType },
            modifiers: null)!;

        var opcodes = ILReader.GetOpCodes(conversionMethod);
        Assert.DoesNotContain(opcodes, opcode => opcode == OpCodes.Box);
    }

    [Fact]
    public void DiscriminatedUnionConversion_DoesNotAllocate()
    {
        var code = """
import System.*

union Option {
    Some(value: int)
    None
}

class Container {
    public static Measure() -> long {
        val before = GC.GetAllocatedBytesForCurrentThread()
        val opt: Option = .Some(123)
        val after = GC.GetAllocatedBytesForCurrentThread()
        return after - before
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var measureMethod = containerType.GetMethod("Measure", BindingFlags.Public | BindingFlags.Static)!;

        var allocated = (long)measureMethod.Invoke(null, Array.Empty<object?>())!;
        Assert.Equal(0L, allocated);
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionType = runtimeAssembly.GetType("Result", throwOnError: true)!;
        var okCaseType = runtimeAssembly.GetType("Result_Ok", throwOnError: true)!;
        var errorCaseType = runtimeAssembly.GetType("Result_Error", throwOnError: true)!;
        var okTryGetMethod = unionType.GetMethod(
            "TryGetValue",
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            types: new[] { okCaseType.MakeByRefType() },
            modifiers: null)!;
        var errorTryGetMethod = unionType.GetMethod(
            "TryGetValue",
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            types: new[] { errorCaseType.MakeByRefType() },
            modifiers: null)!;
        Assert.Null(unionType.GetMethod("TryGetOk", BindingFlags.Public | BindingFlags.Instance));
        Assert.Null(unionType.GetMethod("TryGetError", BindingFlags.Public | BindingFlags.Instance));

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
union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}

class Container {
    public Create() -> Error<string> {
        return Result<int, string>.Error(message: "boom")
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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

        var closedCaseType = runtimeAssembly.GetType("Result_Error`1", throwOnError: true)!.MakeGenericType(typeof(string));

        Assert.Equal(closedCaseType, caseValue!.GetType());
        Assert.Collection(
            caseValue.GetType().GetGenericArguments(),
            arg => Assert.Equal(typeof(string), arg));
    }

    [Fact]
    public void GenericUnionCases_OnlyCaptureUsedTypeParameters()
    {
        var code = """
union Result<T, E> {
    Ok(value: T)
    Error(message: E)
    Pair(left: T, right: E)
    None
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;

        Assert.Single(runtimeAssembly.GetType("Result_Ok`1", throwOnError: true)!.GetGenericArguments());
        Assert.Single(runtimeAssembly.GetType("Result_Error`1", throwOnError: true)!.GetGenericArguments());
        Assert.Equal(2, runtimeAssembly.GetType("Result_Pair`2", throwOnError: true)!.GetGenericArguments().Length);
        Assert.Empty(runtimeAssembly.GetType("Result_None", throwOnError: true)!.GetGenericArguments());
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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
        var caseTypeDefinition = runtimeAssembly.GetType("Result_Ok`1", throwOnError: true)!;
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var caseType = runtimeAssembly.GetType("Shape_Rectangle", throwOnError: true)!;

        var ctor = caseType.GetConstructor(new[] { typeof(int), typeof(int) })!;
        var caseInstance = ctor.Invoke(new object?[] { 3, 6 });
        var toString = caseType.GetMethod("ToString", BindingFlags.Public | BindingFlags.Instance)!;

        var text = (string)toString.Invoke(caseInstance, Array.Empty<object?>())!;
        Assert.Equal("Shape.Rectangle(Width=3, Height=6)", text);
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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

        Assert.Equal("Result.Ok(5)", text);
    }

    [Fact]
    public void UnionMemberCaseInvocation_InLambda_ReturnsExpectedResult()
    {
        var code = """
import System.*

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Container {
    public static Build() -> Result<int, string> {
        val factory: Func<int, Result<int, string>> = x => Result.Ok(x)
        return factory(42)
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var buildMethod = containerType.GetMethod("Build", BindingFlags.Public | BindingFlags.Static)!;

        var unionValue = buildMethod.Invoke(null, Array.Empty<object?>());
        Assert.NotNull(unionValue);
        Assert.Equal("Result.Ok(42)", unionValue!.ToString());
    }

    [Fact]
    public void UnionCaseCanonicalForms_EmitEquivalentRuntimeValues()
    {
        var code = """
union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Container {
    public static CaseValue() -> Ok<int> {
        return Ok(2)
    }

    public static CaseValueExplicit() -> Ok<int> {
        return Ok<int>(2)
    }

    public static CarrierQualified() -> Result<int, string> {
        return Result<int, string>.Ok(2)
    }

    public static CarrierTargetTyped() -> Result<int, string> {
        val value: Result<int, string> = .Ok(2)
        return value
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;

        var caseValue = containerType.GetMethod("CaseValue", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, Array.Empty<object?>());
        var caseValueExplicit = containerType.GetMethod("CaseValueExplicit", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, Array.Empty<object?>());
        var carrierQualified = containerType.GetMethod("CarrierQualified", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, Array.Empty<object?>());
        var carrierTargetTyped = containerType.GetMethod("CarrierTargetTyped", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, Array.Empty<object?>());

        Assert.NotNull(caseValue);
        Assert.NotNull(caseValueExplicit);
        Assert.NotNull(carrierQualified);
        Assert.NotNull(carrierTargetTyped);

        Assert.Equal("Result.Ok(2)", caseValue!.ToString());
        Assert.Equal("Result.Ok(2)", caseValueExplicit!.ToString());
        Assert.Equal("Result.Ok(2)", carrierQualified!.ToString());
        Assert.Equal("Result.Ok(2)", carrierTargetTyped!.ToString());
    }

    [Fact]
    public void UnionCaseToString_HandlesGenericValueTypes()
    {
        var code = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;

        var caseType = runtimeAssembly.GetType("Result_Ok`1", throwOnError: true)!.MakeGenericType(typeof(int));

        var ctor = caseType.GetConstructor(new[] { typeof(int) })!;
        var caseInstance = ctor.Invoke(new object?[] { 99 });
        var toString = caseType.GetMethod("ToString", BindingFlags.Public | BindingFlags.Instance)!;

        var text = (string)toString.Invoke(caseInstance, Array.Empty<object?>())!;
        Assert.Equal("Result.Ok(99)", text);
    }

    [Fact]
    public void UnionToString_EscapesStringValues()
    {
        var code = """
union Shape {
    Label(text: string)
}

class Container {
    public static Create() -> Shape {
        return .Label("a\"b")
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionType = runtimeAssembly.GetType("Shape", throwOnError: true)!;
        var caseType = runtimeAssembly.GetType("Shape_Label", throwOnError: true)!;

        var ctor = caseType.GetConstructor(new[] { typeof(string) })!;
        var caseInstance = ctor.Invoke(new object?[] { "a\"b" });
        var caseToString = caseType.GetMethod("ToString", BindingFlags.Public | BindingFlags.Instance)!;
        var caseText = (string)caseToString.Invoke(caseInstance, Array.Empty<object?>())!;
        Assert.Equal("Shape.Label(\"a\\\"b\")", caseText);

        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var createMethod = containerType.GetMethod("Create", BindingFlags.Public | BindingFlags.Static)!;
        var unionValue = createMethod.Invoke(null, Array.Empty<object?>());
        Assert.NotNull(unionValue);

        var unionToString = unionType.GetMethod("ToString", BindingFlags.Public | BindingFlags.Instance)!;
        var unionText = (string)unionToString.Invoke(unionValue, Array.Empty<object?>())!;
        Assert.Equal("Shape.Label(\"a\\\"b\")", unionText);
    }

    [Fact]
    public void UnionToString_ReturnsUninitializedWhenTagIsInvalid()
    {
        var code = """
union Maybe<T> {
    None
    Some(value: T)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionTypeDefinition = runtimeAssembly.GetType("Maybe`1", throwOnError: true)!;
        var closedUnionType = unionTypeDefinition.MakeGenericType(typeof(int));

        var instance = Activator.CreateInstance(closedUnionType)!;
        var tagField = closedUnionType.GetField("<Tag>", BindingFlags.Instance | BindingFlags.NonPublic)!;
        tagField.SetValue(instance, (byte)255);
        var toString = closedUnionType.GetMethod("ToString", BindingFlags.Public | BindingFlags.Instance)!;

        var text = (string)toString.Invoke(instance, Array.Empty<object?>())!;
        Assert.Equal("<Uninitialized>", text);
    }

    [Fact]
    public void DiscriminatedUnion_EmitsSingleToStringPerType()
    {
        var code = """
import System.Console.*

union Test {
    Something(value: string)
    Nothing
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references = [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var unionType = runtimeAssembly.GetType("Test", throwOnError: true)!;

        var unionToStrings = unionType.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly)
            .Where(m => m.Name == nameof(object.ToString));
        Assert.Single(unionToStrings);

        foreach (var caseType in runtimeAssembly.GetTypes().Where(t => t.Name is "Something" or "Nothing"))
        {
            var caseToStrings = caseType
                .GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly)
                .Where(m => m.Name == nameof(object.ToString));

            Assert.Single(caseToStrings);
        }
    }

    [Fact]
    public void GenericUnionCaseType_IsRegisteredForCodeGeneration()
    {
        var code = """
import System.*

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}

extension ResultExtensions<T, E> for Result<T, E> {
    public IsError: bool {
        get {
            if self is .Error(_) {
                return true
            }
            return false
        }
    }
}

class Container {
    public CreateError() -> Result<int, string> {
        return Result<int, string>.Error(message: "oops")
    }

    public Check() -> bool {
        var value = CreateError()
        return value.IsError
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var extensionContainer = runtimeAssembly.GetType("ResultExtensions`2")
            ?? runtimeAssembly.GetType("ResultExtensions");
        Assert.NotNull(extensionContainer);
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var check = containerType.GetMethod("Check", BindingFlags.Public | BindingFlags.Instance)!;
        var instance = Activator.CreateInstance(containerType)!;

        var isError = (bool)check.Invoke(instance, Array.Empty<object?>())!;
        Assert.True(isError);
    }

    [Fact]
    public void ImplicitTailReturn_ConvertsUnionCaseToUnion()
    {
        const string code = """
import System.*
import System.Linq.*
import System.Collections.Generic.*

union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

class Container {
    public static Create(items: IEnumerable<int>) -> Result<int, string> {
        val values = items.Take(1).ToList()
        if values.Count == 1 {
            return .Ok(values[0])
        }
        .Error("oops")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "implicit-tail-return-union",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var runtimeAssembly = loaded.Assembly;
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var create = containerType.GetMethod("Create", BindingFlags.Public | BindingFlags.Static)!;
        var empty = Array.Empty<int>();
        var value = create.Invoke(null, [empty])!;

        Assert.Equal("Result.Error(\"oops\")", value.ToString());
    }

    [Fact]
    public void ExtensionAccessibility_IsPreservedInMetadata()
    {
        const string code = """
import System.*

public extension IntExtensions for int {
    public Double() -> int {
        return self * 2
    }
}

internal extension StringExtensions for string {
    internal Echo() -> string {
        return self
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "extension-accessibility",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;

        var publicExtensions = assembly.GetType("IntExtensions", throwOnError: true)!;
        Assert.True(publicExtensions.IsPublic);
        var doubleMethod = publicExtensions.GetMethod("Double", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(doubleMethod);

        var internalExtensions = assembly.GetType("StringExtensions", throwOnError: true)!;
        Assert.True(internalExtensions.IsNotPublic);
        var echoMethod = internalExtensions.GetMethod("Echo", BindingFlags.NonPublic | BindingFlags.Static);
        Assert.NotNull(echoMethod);
        Assert.True(echoMethod!.IsAssembly);
    }

    [Fact]
    public void ExtensionMarkerMetadata_IsEmitted()
    {
        const string code = """
class Widget {
    public val Id: int
}

extension WidgetExtensions for Widget {
    public static Build() -> Widget {
        return Widget()
    }

    public Describe() -> int {
        return self.Id
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "extension-markers",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;

        var extensionMarkerAttribute = assembly.GetType("System.Runtime.CompilerServices.ExtensionMarkerNameAttribute", throwOnError: true)!;
        var extensionContainer = assembly.GetType("WidgetExtensions", throwOnError: true)!;
        var markerType = assembly
            .GetTypes()
            .Where(static t => t.Name.StartsWith("<>__RavenExtensionGrouping_For_", StringComparison.Ordinal))
            .SelectMany(t => t.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic))
            .FirstOrDefault(static t => t.Name.StartsWith("<>__RavenExtensionMarker_", StringComparison.Ordinal));
        Assert.NotNull(markerType);

        var markerMethod = markerType!.GetMethod("<Extension>$", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(markerMethod);
        Assert.Equal("Widget", markerMethod!.GetParameters().Single().ParameterType.Name);

        var buildMethod = extensionContainer.GetMethod("Build", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(buildMethod);
        var buildMarkerNames = buildMethod!
            .GetCustomAttributes(extensionMarkerAttribute, inherit: false)
            .Select(attr => (string)extensionMarkerAttribute.GetProperty("Name")!.GetValue(attr)!)
            .ToArray();
        Assert.Contains(markerType.Name, buildMarkerNames);

        var describeMethod = extensionContainer.GetMethod("Describe", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(describeMethod);
        var describeMarkerNames = describeMethod!
            .GetCustomAttributes(extensionMarkerAttribute, inherit: false)
            .Select(attr => (string)extensionMarkerAttribute.GetProperty("Name")!.GetValue(attr)!)
            .ToArray();
        Assert.Contains(markerType.Name, describeMarkerNames);
    }

    [Fact]
    public void GenericExtensionProperty_WithSiblingUnion_EmitsCaseTypes()
    {
        var code = """
import System.*

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}

union Outcome<T, E> {
    Success(value: T)
    Failure(data: E)
}

extension ResultExtensions<T, E> for Result<T, E> {
    public IsError: bool {
        get {
            if self is .Error(_) {
                return true
            }
            return false
        }
    }
}

class Container {
    private Parse(text: string) -> Result<int, string> {
        if text == "42" {
            return Result<int, string>.Ok(42)
        }
        return Result<int, string>.Error("bad")
    }

    public Check(text: string) -> bool {
        return Parse(text).IsError
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var containerType = runtimeAssembly.GetType("Container", throwOnError: true)!;
        var check = containerType.GetMethod("Check", BindingFlags.Public | BindingFlags.Instance)!;
        var instance = Activator.CreateInstance(containerType)!;
        var isError = (bool)check.Invoke(instance, new object?[] { "foo" })!;
        var isOk = (bool)check.Invoke(instance, new object?[] { "42" })!;

        Assert.True(isError);
        Assert.False(isOk);
    }
}
