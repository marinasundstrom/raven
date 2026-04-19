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

public class UnionCodeGenTests
{
    [Fact]
    public void UnionCaseConstructor_AssignsFields()
    {
        var code = """
union Option {
    case Some(value: int, label: string)
}

class Container {
    public func Create() -> Option {
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
        Assert.Equal("Option", caseValue!.GetType().Name);
        Assert.Contains("Some", caseValue.ToString(), StringComparison.Ordinal);
    }

    [Fact]
    public void Union_DoesNotEmitImplicitConversionOperators()
    {
        const string code = """
union Option {
    case Some(value: int)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "union-conversion-ctor",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var optionType = assembly.GetType("Option", throwOnError: true)!;
        var conversionMethods = optionType
            .GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Where(method => method.Name == "op_Implicit")
            .ToArray();

        Assert.Empty(conversionMethods);
    }

    [Fact]
    public void PublicUnionCases_AreEmittedAsPublicTypes()
    {
        const string code = """
import System.*

public union Result<T> {
    case Ok(value: T)
    case Error(message: string)
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

        var errorCase = assembly.GetType("Result_Error`1", throwOnError: true)!.MakeGenericType(typeof(int));
        Assert.True(errorCase.IsPublic);
        var errorCtor = errorCase.GetConstructor(BindingFlags.Public | BindingFlags.Instance, binder: null, new[] { typeof(string) }, modifiers: null);
        Assert.NotNull(errorCtor);
    }

    [Fact]
    public void GenericOptionNoneCase_UsesDirectUnionConstructorOnConstructedCarrier()
    {
        const string code = """
class Runner {
    public static func Run(flag: bool) -> Option<int> {
        val input: Option<int> = Some(42)
        return input match {
            Some(val value) => Option<int>.Some(value)
            None => None
        }
    }
}

union Option<T> {
    case Some(value: T)
    case None
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "generic-option-none-create",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var runnerType = assembly.GetType("Runner", throwOnError: true)!;
        var runMethod = runnerType.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var calledMembers = ILReader.GetCalledMembers(runMethod);

        Assert.DoesNotContain(
            calledMembers,
            member => member.Contains("System.Option`1::Create", StringComparison.Ordinal));
        Assert.DoesNotContain(
            calledMembers,
            member => member.EndsWith("::Create", StringComparison.Ordinal));
        Assert.DoesNotContain(
            calledMembers,
            member => member.EndsWith("::op_Implicit", StringComparison.Ordinal));

        var fromSome = runMethod.Invoke(null, [true]);
        var fromNone = runMethod.Invoke(null, [false]);

        Assert.NotNull(fromSome);
        Assert.NotNull(fromNone);
    }

    [Fact]
    public void GenericUnionErrorSubtypeConversion_RunsSuccessfully()
    {
        const string code = """
import System.*

class Runner {
    public static func Run() -> string {
        val result: Result<string, Exception> = Error(InvalidOperationException("x"))
        return result match {
            Error(val e) => e.GetType().Name
            Ok(val value) => value
        }
    }
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "generic-union-error-subtype",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var runnerType = assembly.GetType("Runner", throwOnError: true)!;
        var runMethod = runnerType.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        var output = runMethod.Invoke(null, Array.Empty<object?>());
        Assert.Equal("InvalidOperationException", output);
    }

    [Fact]
    public void InternalUnionCases_AreEmittedWithInternalAccessibility()
    {
        const string code = """
union Hidden<T> {
    case Case(value: T)
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
    case Some(value: int)
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
    public void UnionWithoutStorageModifier_EmitsReferenceTypeCarrier()
    {
        var code = """
union Option {
    case Some(value: int)
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

        Assert.True(unionType.IsClass);
        Assert.False(unionType.IsValueType);
    }

    [Fact]
    public void UnionStruct_EmitsValueTypeCarrier()
    {
        var code = """
union struct Option {
    case Some(value: int)
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

        Assert.True(unionType.IsValueType);
    }

    [Fact]
    public void Union_EmitsConventionalValueProperty()
    {
        var code = """
union Option {
    case None
    case Some(value: int)
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
        var valueProperty = unionType.GetProperty("Value", BindingFlags.Instance | BindingFlags.Public);

        Assert.NotNull(valueProperty);
        Assert.Equal(typeof(object), valueProperty!.PropertyType);
        var nullability = new NullabilityInfoContext().Create(valueProperty);
        Assert.Equal(NullabilityState.NotNull, nullability.ReadState);
    }

    [Fact]
    public void DefaultStructUnion_ValuePropertyReturnsNull()
    {
        var code = """
union struct Maybe<T> {
    case None
    case Some(value: T)
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
        var valueProperty = closedUnionType.GetProperty("Value", BindingFlags.Instance | BindingFlags.Public)!;
        var hasValueProperty = closedUnionType.GetProperty("HasValue", BindingFlags.Instance | BindingFlags.Public)!;
        var nullability = new NullabilityInfoContext().Create(valueProperty);
        var value = valueProperty.GetValue(instance);

        Assert.Equal(NullabilityState.Nullable, nullability.ReadState);
        Assert.Equal(false, hasValueProperty.GetValue(instance));
        Assert.Null(value);
    }

    [Fact]
    public void ClassUnion_WithNullableMember_EmitsNullableValueProperty()
    {
        var code = """
union Maybe(string? | int)
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
        var unionType = runtimeAssembly.GetType("Maybe", throwOnError: true)!;
        var valueProperty = unionType.GetProperty("Value", BindingFlags.Instance | BindingFlags.Public)!;
        var nullability = new NullabilityInfoContext().Create(valueProperty);
        var instance = Activator.CreateInstance(unionType, [null])!;

        Assert.Equal(typeof(object), valueProperty.PropertyType);
        Assert.Equal(NullabilityState.Nullable, nullability.ReadState);
        Assert.Null(valueProperty.GetValue(instance));
    }

    [Fact]
    public void StructUnion_ConstructedWithNullablePayload_HasValueIsTrueWhenValueIsNull()
    {
        var code = """
union struct Maybe<T>(T)
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
        var closedUnionType = unionTypeDefinition.MakeGenericType(typeof(string));
        var instance = Activator.CreateInstance(closedUnionType, [null])!;
        var valueProperty = closedUnionType.GetProperty("Value", BindingFlags.Instance | BindingFlags.Public)!;
        var hasValueProperty = closedUnionType.GetProperty("HasValue", BindingFlags.Instance | BindingFlags.Public)!;

        Assert.Equal(true, hasValueProperty.GetValue(instance));
        Assert.Null(valueProperty.GetValue(instance));
    }

    [Fact]
    public void DefaultStructUnion_NullPatternMatchesInactiveState()
    {
        var code = """
class Runner {
    public static func DescribeDefault() -> string {
        val value: Maybe<int> = default
        return value match {
            null => "inactive"
            Some(val payload) => payload.ToString()
            None => "none"
        }
    }

    public static func DescribeSome() -> string {
        val value: Maybe<int> = Some(42)
        return value match {
            null => "inactive"
            Some(val payload) => payload.ToString()
            None => "none"
        }
    }
}

union struct Maybe<T> {
    case None
    case Some(value: T)
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
        var runnerType = runtimeAssembly.GetType("Runner", throwOnError: true)!;
        var defaultMethod = runnerType.GetMethod("DescribeDefault", BindingFlags.Public | BindingFlags.Static)!;
        var someMethod = runnerType.GetMethod("DescribeSome", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal("inactive", defaultMethod.Invoke(null, Array.Empty<object?>()));
        Assert.Equal("42", someMethod.Invoke(null, Array.Empty<object?>()));
    }

    [Fact]
    public void NominalUnion_EmitsConstructorsForMemberTypes()
    {
        var code = """
record Left(value: int)
record Right(message: string)

union Either(Left | Right)
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
        var eitherType = runtimeAssembly.GetType("Either", throwOnError: true)!;
        var leftType = runtimeAssembly.GetType("Left", throwOnError: true)!;
        var rightType = runtimeAssembly.GetType("Right", throwOnError: true)!;

        Assert.NotNull(eitherType.GetConstructor([leftType]));
        Assert.NotNull(eitherType.GetConstructor([rightType]));
    }

    [Fact]
    public void GenericParenthesizedUnion_ConstructorsAssignCarrierState()
    {
        const string code = """
class Runner {
    public static func Left() -> Either<int, string> {
        return 42
    }

    public static func Right() -> Either<int, string> {
        return "invoice"
    }
}

union Either<T1, T2>(T1 | T2)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "generic-parenthesized-union",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var runnerType = assembly.GetType("Runner", throwOnError: true)!;
        var leftMethod = runnerType.GetMethod("Left", BindingFlags.Public | BindingFlags.Static)!;
        var rightMethod = runnerType.GetMethod("Right", BindingFlags.Public | BindingFlags.Static)!;

        var left = leftMethod.Invoke(null, Array.Empty<object?>());
        var right = rightMethod.Invoke(null, Array.Empty<object?>());

        Assert.Equal("Either<Int32, String>(42)", left?.ToString());
        Assert.Equal("Either<Int32, String>(\"invoice\")", right?.ToString());
    }

    [Fact]
    public void ParenthesizedUnion_ToString_UsesUnionNameAndEscapesStringPayload()
    {
        const string code = """
class Runner {
    public static func Value() -> Message {
        return "a\"b"
    }
}

union Message(string | int)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "parenthesized-union-tostring",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var runnerType = assembly.GetType("Runner", throwOnError: true)!;
        var valueMethod = runnerType.GetMethod("Value", BindingFlags.Public | BindingFlags.Static)!;

        var value = valueMethod.Invoke(null, Array.Empty<object?>());

        Assert.NotNull(value);
        Assert.Equal("Message(\"a\\\"b\")", value!.ToString());
    }

    [Fact]
    public void ParenthesizedUnion_ToString_FormatsTypeArgumentsAndFriendlyPayloadTypeNames()
    {
        const string code = """
import System.Collections.Generic.*

class Runner {
    public static func Value() -> MyResult<string> {
        return List<string>()
    }
}

union MyResult<T>(List<T> | int)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "parenthesized-union-friendly-type-names",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var runnerType = assembly.GetType("Runner", throwOnError: true)!;
        var valueMethod = runnerType.GetMethod("Value", BindingFlags.Public | BindingFlags.Static)!;

        var value = valueMethod.Invoke(null, Array.Empty<object?>());

        Assert.NotNull(value);
        Assert.Equal("MyResult<String>(System.Collections.Generic.List<String>)", value!.ToString());
    }

    [Fact]
    public void GenericParenthesizedUnion_ExplicitCastExtractsMemberType()
    {
        const string code = """
import System.*

class Runner {
    public static func Left() -> int {
        val value: Either<int, string> = 42
        return (int)value
    }

    public static func Invalid() -> string {
        val value: Either<int, string> = 42
        return (string)value
    }
}

union Either<T1, T2>(T1 | T2)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "generic-parenthesized-union-cast",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var runnerType = assembly.GetType("Runner", throwOnError: true)!;
        var leftMethod = runnerType.GetMethod("Left", BindingFlags.Public | BindingFlags.Static)!;
        var invalidMethod = runnerType.GetMethod("Invalid", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(42, leftMethod.Invoke(null, Array.Empty<object?>()));
        Assert.Throws<TargetInvocationException>(() => invalidMethod.Invoke(null, Array.Empty<object?>()));
        try
        {
            invalidMethod.Invoke(null, Array.Empty<object?>());
        }
        catch (TargetInvocationException ex)
        {
            Assert.IsType<InvalidCastException>(ex.InnerException);
        }
    }

    [Fact]
    public void GenericParenthesizedUnion_TypePatternMatchExtractsMemberValue()
    {
        const string code = """
class Runner {
    public static func DescribeLeft() -> string {
        val value: Either<int, string> = 42
        return value match {
            int amount => "cash $amount"
            string reference => "card $reference"
        }
    }

    public static func DescribeRight() -> string {
        val value: Either<int, string> = "invoice"
        return value match {
            int amount => "cash $amount"
            string reference => "card $reference"
        }
    }
}

union Either<T1, T2>(T1 | T2)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "generic-parenthesized-union-match",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var runnerType = assembly.GetType("Runner", throwOnError: true)!;
        var leftMethod = runnerType.GetMethod("DescribeLeft", BindingFlags.Public | BindingFlags.Static)!;
        var rightMethod = runnerType.GetMethod("DescribeRight", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal("cash 42", leftMethod.Invoke(null, Array.Empty<object?>()));
        Assert.Equal("card invoice", rightMethod.Invoke(null, Array.Empty<object?>()));
    }

    [Fact]
    public void GenericParenthesizedUnion_NestedGenericMemberTypesEmitConstructorsAndConstraints()
    {
        const string code = """
import System.Collections.Generic.*

union MyResult2<T>(List<T> | int)
    where T : class

union MyResult3(List<int> | string)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "generic-parenthesized-union-nested-members",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;

        var genericUnionDefinition = assembly.GetType("MyResult2`1", throwOnError: true)!;
        var genericParameter = genericUnionDefinition.GetGenericArguments().Single();
        Assert.True((genericParameter.GenericParameterAttributes & GenericParameterAttributes.ReferenceTypeConstraint) != 0);

        var constructedGenericUnion = genericUnionDefinition.MakeGenericType(typeof(string));
        Assert.NotNull(constructedGenericUnion.GetConstructor([typeof(List<string>)]));
        Assert.NotNull(constructedGenericUnion.GetConstructor([typeof(int)]));

        var concreteUnion = assembly.GetType("MyResult3", throwOnError: true)!;
        Assert.NotNull(concreteUnion.GetConstructor([typeof(List<int>)]));
        Assert.NotNull(concreteUnion.GetConstructor([typeof(string)]));
    }

    [Fact]
    public void ParenthesizedUnion_NominalDeconstructionPatternExtractsMemberValue()
    {
        const string code = """
class Runner {
    public static func DescribeCash() -> string {
        val value = Payment(Cash(42.0m))
        return value match {
            Cash(val amount) => "cash $amount"
            Card(val reference) => "card $reference"
        }
    }

    public static func DescribeCard() -> string {
        val value = Payment(Card("invoice"))
        return value match {
            Cash(val amount) => "cash $amount"
            Card(val reference) => "card $reference"
        }
    }
}

record Cash(Amount: decimal)
record Card(Reference: string)

union Payment(Cash | Card)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "parenthesized-union-nominal-match",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var runnerType = assembly.GetType("Runner", throwOnError: true)!;
        var cashMethod = runnerType.GetMethod("DescribeCash", BindingFlags.Public | BindingFlags.Static)!;
        var cardMethod = runnerType.GetMethod("DescribeCard", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal("cash 42,0", cashMethod.Invoke(null, Array.Empty<object?>()));
        Assert.Equal("card invoice", cardMethod.Invoke(null, Array.Empty<object?>()));
    }

    [Fact]
    public void DiscriminatedUnionCaseTypes_AnnotatedWithMarkerAttribute()
    {
        var code = """
union Option {
    case None
    case Some(value: int)
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
            .Where(type => type.GetCustomAttributesData().Any(a => a.AttributeType.FullName == "System.Runtime.CompilerServices.UnionCaseAttribute"))
            .ToArray();
        Assert.NotEmpty(caseTypes);

        foreach (var caseType in caseTypes)
        {
            var attribute = Assert.Single(caseType
                .GetCustomAttributesData()
                .Where(a => a.AttributeType.FullName == "System.Runtime.CompilerServices.UnionCaseAttribute"));

            Assert.Single(attribute.ConstructorArguments);
            Assert.Equal(unionType, attribute.ConstructorArguments[0].Value);
        }
    }

    [Fact]
    public void DiscriminatedUnionConversion_SetsTagAndPayload()
    {
        var code = """
union Option {
    case None
    case Some(value: int)
}

class Container {
    public func Create() -> Option {
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
    case Ok(value: T)
    case Error(error: E)
}

class Container {
    public func Make() -> Result<(), string> {
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
    public void DiscriminatedUnion_UsesCaseTypedConstructors()
    {
        var code = """
union Option {
    case None
    case Some(value: int)
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

        var unionCtor = unionType.GetConstructor(new[] { caseType })!;

        var ctor = caseType.GetConstructor(new[] { typeof(int) })!;
        var caseInstance = ctor.Invoke(new object?[] { 7 });

        var unionValue = unionCtor.Invoke(new[] { caseInstance });
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
    case Ok(value: int)
    case Error(message: string)
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
    case Some(value: T)
    case None
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
    case Some(value: int)
    case None
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
        var unionCtor = unionType.GetConstructor(new[] { caseType })!;

        var ilBytes = unionCtor.GetMethodBody()!.GetILAsByteArray();
        Assert.NotNull(ilBytes);
        Assert.DoesNotContain(unchecked((byte)OpCodes.Box.Value), ilBytes!);
    }

    [Fact]
    public void DiscriminatedUnionConversion_DoesNotAllocate()
    {
        var code = """
import System.*

union Option {
    case Some(value: int)
    case None
}

class Container {
    public static func Measure() -> long {
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
    case Ok(value: int)
    case Error(message: string)
}

class Container {
    public func GetOk() -> Result {
        return Result.Ok(value: 7)
    }

    public func GetError() -> Result {
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
        var okArgs = new object?[] { null };
        var okResult = (bool)okTryGetMethod.Invoke(okUnionValue, okArgs)!;
        Assert.True(okResult);
        var okValueProperty = okCaseType.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.Equal(7, (int)okValueProperty.GetValue(okArgs[0])!);

        var errorUnionValue = getErrorMethod.Invoke(container, Array.Empty<object?>());
        var errorArgs = new object?[] { null };
        var errorResult = (bool)errorTryGetMethod.Invoke(errorUnionValue, errorArgs)!;
        Assert.True(errorResult);
        var errorMessageProperty = errorCaseType.GetProperty("Message", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.Equal("boom", (string)errorMessageProperty.GetValue(errorArgs[0])!);

        var mismatchArgs = new object?[] { null };
        var mismatchResult = (bool)errorTryGetMethod.Invoke(okUnionValue, mismatchArgs)!;
        Assert.False(mismatchResult);
    }

    [Fact]
    public void DiscriminatedUnionCase_EmitsSynthesizedDeconstruct()
    {
        var code = """
union Result {
    case Ok(value: int)
    case Error(message: string)
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
        var okCaseType = runtimeAssembly.GetType("Result_Ok", throwOnError: true)!;
        var deconstructMethod = okCaseType.GetMethod(
            "Deconstruct",
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            types: [typeof(int).MakeByRefType()],
            modifiers: null)!;

        var okValue = Activator.CreateInstance(okCaseType, [7])!;
        var args = new object?[] { 0 };
        deconstructMethod.Invoke(okValue, args);

        Assert.Equal(7, (int)args[0]!);
    }

    [Fact]
    public void DiscriminatedUnionCase_EmitsSynthesizedPropertyGetter()
    {
        var code = """
union Result {
    case Ok(value: int)
    case Error(message: string)
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
        var okCaseType = runtimeAssembly.GetType("Result_Ok", throwOnError: true)!;
        var okValue = Activator.CreateInstance(okCaseType, [7])!;
        var valueProperty = okCaseType.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance)!;

        Assert.Equal(7, (int)valueProperty.GetValue(okValue)!);
    }

    [Fact]
    public void GenericUnionCaseConstruction_PreservesOuterTypeArguments()
    {
        var code = """
union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}

class Container {
    public func Create() -> Result<int, string> {
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
        Assert.Contains("Result_Error", caseValue!.GetType().Name, StringComparison.Ordinal);
        Assert.Contains("Error", caseValue.ToString(), StringComparison.Ordinal);
    }

    [Fact]
    public void GenericUnionCases_OnlyCaptureUsedTypeParameters()
    {
        var code = """
union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
    case Pair(left: T, right: E)
    case None
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
    case Ok(value: T)
    case Error(message: string)
}

class Container {
    public static func CreateOk() -> Result<int> {
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

        Assert.Equal(closedCaseType, unionValue!.GetType());
        Assert.True(closedUnionType.IsAssignableFrom(unionValue.GetType()));

        var ctor = closedCaseType.GetConstructor(new[] { typeof(int) })!;
        var caseInstance = ctor.Invoke(new object?[] { 7 });
        Assert.IsAssignableFrom(closedUnionType, caseInstance);
    }

    [Fact]
    public void UnionCaseToString_FormatsUnionNameAndParameters()
    {
        var code = """
union Shape {
    case Rectangle(width: int, height: int)
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
    case Ok(value: T)
    case Error(message: string)
}

class Container {
    public static func Create() -> Result<int> {
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

        Assert.Equal("Result<Int32>.Ok(5)", text);
    }

    [Fact]
    public void GenericUnionCaseToString_DirectCaseInstance_FormatsCapturedTypeArguments()
    {
        var code = """
union Result<T> {
    case Ok(value: T)
    case Error(message: string)
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
        var okCaseType = runtimeAssembly.GetType("Result_Ok`1", throwOnError: true)!.MakeGenericType(typeof(int));
        var okCase = okCaseType.GetConstructor(new[] { typeof(int) })!.Invoke([42]);

        Assert.Equal("Result<Int32>.Ok(42)", okCase!.ToString());
    }

    [Fact]
    public void GenericUnionCaseDisplayNameHelper_DirectCaseInstance_FormatsCapturedTypeArguments()
    {
        var code = """
union Result<T> {
    case Ok(value: T)
    case Error(message: string)
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
        var okCaseType = runtimeAssembly.GetType("Result_Ok`1", throwOnError: true)!.MakeGenericType(typeof(int));
        var okCase = okCaseType.GetConstructor(new[] { typeof(int) })!.Invoke([42]);
        var displayNameHelper = okCaseType.GetMethod("<RavenUnionDisplayName>", BindingFlags.Instance | BindingFlags.NonPublic)!;

        var displayName = (string)displayNameHelper.Invoke(okCase, Array.Empty<object?>())!;

        Assert.Equal("Result<Int32>", displayName);
    }

    [Fact]
    public void UnionMemberCaseInvocation_InLambda_ReturnsExpectedResult()
    {
        var code = """
import System.*

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}

class Container {
    public static func Build() -> Result<int, string> {
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
        Assert.Equal("Result<Int32, String>.Ok(42)", unionValue!.ToString());
    }

    [Fact]
    public void UnionCaseCanonicalForms_EmitEquivalentRuntimeValues()
    {
        var code = """
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}

class Container {
    public static func CaseValue() -> Ok<int> {
        return Ok(2)
    }

    public static func CaseValueExplicit() -> Ok<int> {
        return Ok<int>(2)
    }

    public static func CarrierQualified() -> Result<int, string> {
        return Result<int, string>.Ok(2)
    }

    public static func CarrierTargetTyped() -> Result<int, string> {
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

        Assert.Equal("Result<Int32>.Ok(2)", caseValue!.ToString());
        Assert.Equal("Result<Int32>.Ok(2)", caseValueExplicit!.ToString());
        Assert.Equal("Result<Int32, String>.Ok(2)", carrierQualified!.ToString());
        Assert.Equal("Result<Int32, String>.Ok(2)", carrierTargetTyped!.ToString());
    }

    [Fact]
    public void UnionCaseToString_HandlesGenericValueTypes()
    {
        var code = """
union Result<T> {
    case Ok(value: T)
    case Error(message: string)
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
        Assert.Equal("Result<Int32>.Ok(99)", text);
    }

    [Fact]
    public void UnionToString_EscapesStringValues()
    {
        var code = """
union Shape {
    case Label(text: string)
}

class Container {
    public static func Create() -> Shape {
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
    public void UnionToString_QuotesGenericStringPayload()
    {
        var code = """
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}

record CustomError(message: string)

class Container {
    public static func Create() -> Result<string, CustomError> {
        return Result<string, CustomError>.Ok("Foo")
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

        var text = unionValue!.ToString();
        Assert.Equal("Result<String, CustomError>.Ok(\"Foo\")", text);
    }

    [Fact]
    public void UnionToString_QuotesGenericCharPayload()
    {
        var code = """
class Container {
    public static func Create() -> Either<char, string> {
        return 'x'
    }
}

union Either<T1, T2>(T1 | T2)
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
        Assert.Equal("Either<Char, String>('x')", unionValue!.ToString());
    }

    [Fact]
    public void UnionToString_ReturnsUninitializedWhenTagIsInvalid()
    {
        var code = """
union Maybe<T> {
    case None
    case Some(value: T)
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
    case Something(value: string)
    case Nothing
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
    case Ok(value: T)
    case Error(message: E)
}

extension ResultExtensions<T, E> for Result<T, E> {
    public val IsError: bool {
        get {
            if self is .Error(_) {
                return true
            }
            return false
        }
    }
}

class Container {
    public func CreateError() -> Result<int, string> {
        return Result<int, string>.Error(message: "oops")
    }

    public func Check() -> bool {
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
    case Ok(value: T)
    case Error(data: E)
}

class Container {
    public static func Create(items: IEnumerable<int>) -> Result<int, string> {
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

        Assert.Equal("Result<Int32, String>.Error(\"oops\")", value.ToString());
    }

    [Fact]
    public void ExtensionAccessibility_IsPreservedInMetadata()
    {
        const string code = """
import System.*

public extension IntExtensions for int {
    public func Double() -> int {
        return self * 2
    }
}

internal extension StringExtensions for string {
    internal func Echo() -> string {
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
    public static func Build() -> Widget {
        return Widget()
    }

    public func Describe() -> int {
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
    case Ok(value: T)
    case Error(message: E)
}

union Outcome<T, E> {
    case Success(value: T)
    case Failure(data: E)
}

extension ResultExtensions<T, E> for Result<T, E> {
    public val IsError: bool {
        get {
            if self is .Error(_) {
                return true
            }
            return false
        }
    }
}

class Container {
    private func Parse(text: string) -> Result<int, string> {
        if text == "42" {
            return Result<int, string>.Ok(42)
        }
        return Result<int, string>.Error("bad")
    }

    public func Check(text: string) -> bool {
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
