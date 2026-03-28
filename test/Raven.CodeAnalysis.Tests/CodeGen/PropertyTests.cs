using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class PropertyTests
{
    [Fact]
    public void AutoProperty_GeneratesBackingField()
    {
        var code = """
class Sample {
    public var Value: int { get; set; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSemanticModel(syntaxTree);
        var compilationType = compilation.SourceGlobalNamespace.LookupType("Sample");
        Assert.NotNull(compilationType);
        Assert.Contains("Value", compilationType!.GetMembers().Select(m => m.Name));

        var propertySymbol = Assert.Single(
            compilationType.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Value");
        Assert.NotNull(propertySymbol.GetMethod);
        Assert.NotNull(propertySymbol.SetMethod);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Sample", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;

        var property = type.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(property);
        Assert.Equal(0, (int)property!.GetValue(instance)!);

        property.SetValue(instance, 42);
        Assert.Equal(42, (int)property.GetValue(instance)!);
    }

    [Fact]
    public void StaticAutoProperty_GeneratesBackingField()
    {
        var code = """
class Counter {
    public static var Count: int { get; set; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSemanticModel(syntaxTree);
        var compilationType = compilation.SourceGlobalNamespace.LookupType("Counter");
        Assert.NotNull(compilationType);
        Assert.Contains("Count", compilationType!.GetMembers().Select(m => m.Name));

        var propertySymbol = Assert.Single(
            compilationType.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Count");
        Assert.NotNull(propertySymbol.GetMethod);
        Assert.NotNull(propertySymbol.SetMethod);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Counter", throwOnError: true)!;

        var property = type.GetProperty("Count", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(property);
        Assert.Equal(0, (int)property!.GetValue(null)!);

        property.SetValue(null, 7);
        Assert.Equal(7, (int)property.GetValue(null)!);
    }

    [Fact]
    public void ImplicitVarAutoProperty_GeneratesBackingFieldAndGetterSetter()
    {
        var code = """
class Entity {
    public var Id: string
    public var Count: int
    public init(id: string, count: int) {
        Id = id
        Count = count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSemanticModel(syntaxTree);
        var compilationType = compilation.SourceGlobalNamespace.LookupType("Entity");
        Assert.NotNull(compilationType);

        var idProperty = Assert.Single(
            compilationType!.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Id");
        Assert.NotNull(idProperty.GetMethod);
        Assert.NotNull(idProperty.SetMethod);

        var countProperty = Assert.Single(
            compilationType.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Count");
        Assert.NotNull(countProperty.GetMethod);
        Assert.NotNull(countProperty.SetMethod);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Entity", throwOnError: true)!;

        // Verify CLR property metadata
        var idClrProperty = type.GetProperty("Id", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(idClrProperty);
        Assert.NotNull(idClrProperty!.GetMethod);
        Assert.NotNull(idClrProperty.SetMethod);

        var countClrProperty = type.GetProperty("Count", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(countClrProperty);
        Assert.NotNull(countClrProperty!.GetMethod);
        Assert.NotNull(countClrProperty.SetMethod);

        // Verify backing fields are named <Prop>k__BackingField (auto-property convention)
        Assert.NotNull(type.GetField("<Id>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic));
        Assert.NotNull(type.GetField("<Count>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic));

        // Verify round-trip via constructor + getter + setter
        var ctor = type.GetConstructor(BindingFlags.Public | BindingFlags.Instance, [typeof(string), typeof(int)]);
        Assert.NotNull(ctor);
        var instance = ctor!.Invoke(["hello", 42]);

        Assert.Equal("hello", (string)idClrProperty.GetValue(instance)!);
        Assert.Equal(42, (int)countClrProperty.GetValue(instance)!);

        idClrProperty.SetValue(instance, "world");
        countClrProperty.SetValue(instance, 99);

        Assert.Equal("world", (string)idClrProperty.GetValue(instance)!);
        Assert.Equal(99, (int)countClrProperty.GetValue(instance)!);
    }

    [Fact]
    public void ImplicitValAutoProperty_GeneratesBackingFieldAndGetterOnly()
    {
        var code = """
class Point {
    public val X: int
    public val Y: int
    public init(x: int, y: int) {
        X = x
        Y = y
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSemanticModel(syntaxTree);
        var compilationType = compilation.SourceGlobalNamespace.LookupType("Point");
        Assert.NotNull(compilationType);

        var xProperty = Assert.Single(
            compilationType!.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "X");
        Assert.NotNull(xProperty.GetMethod);
        Assert.Null(xProperty.SetMethod);

        var yProperty = Assert.Single(
            compilationType.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Y");
        Assert.NotNull(yProperty.GetMethod);
        Assert.Null(yProperty.SetMethod);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Point", throwOnError: true)!;

        var xClrProperty = type.GetProperty("X", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(xClrProperty);
        Assert.NotNull(xClrProperty!.GetMethod);
        Assert.Null(xClrProperty.SetMethod);

        var yClrProperty = type.GetProperty("Y", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(yClrProperty);
        Assert.NotNull(yClrProperty!.GetMethod);
        Assert.Null(yClrProperty.SetMethod);

        // Verify backing fields use auto-property naming
        Assert.NotNull(type.GetField("<X>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic));
        Assert.NotNull(type.GetField("<Y>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic));

        // Verify values set through init are readable
        var ctor = type.GetConstructor(BindingFlags.Public | BindingFlags.Instance, [typeof(int), typeof(int)]);
        Assert.NotNull(ctor);
        var instance = ctor!.Invoke([3, 7]);

        Assert.Equal(3, (int)xClrProperty.GetValue(instance)!);
        Assert.Equal(7, (int)yClrProperty.GetValue(instance)!);
    }

    [Fact]
    public void ValProperty_WithPrivateSetterOnly_SynthesizesGetter()
    {
        var code = """
class Shipment {
    val Status: int { private set; }

    init() {
        Status = 2
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSemanticModel(syntaxTree);
        var compilationType = compilation.SourceGlobalNamespace.LookupType("Shipment");
        Assert.NotNull(compilationType);

        var propertySymbol = Assert.Single(
            compilationType!.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Status");
        Assert.NotNull(propertySymbol.GetMethod);
        Assert.NotNull(propertySymbol.SetMethod);
        Assert.Equal(Accessibility.Private, propertySymbol.SetMethod!.DeclaredAccessibility);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Shipment", throwOnError: true)!;
        var clrProperty = type.GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(clrProperty);
        Assert.NotNull(clrProperty!.GetMethod);
        Assert.NotNull(clrProperty.SetMethod);
        Assert.True(clrProperty.SetMethod!.IsPrivate);
    }

    [Fact]
    public void VarProperty_WithInitOnlyAccessor_SynthesizesGetter()
    {
        var code = """
class Shipment {
    var Status: int { init; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSemanticModel(syntaxTree);
        var compilationType = compilation.SourceGlobalNamespace.LookupType("Shipment");
        Assert.NotNull(compilationType);

        var propertySymbol = Assert.Single(
            compilationType!.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Status");
        Assert.NotNull(propertySymbol.GetMethod);
        Assert.NotNull(propertySymbol.SetMethod);
        Assert.Equal(MethodKind.InitOnly, propertySymbol.SetMethod!.MethodKind);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Shipment", throwOnError: true)!;
        var clrProperty = type.GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(clrProperty);
        Assert.NotNull(clrProperty!.GetMethod);
        Assert.NotNull(clrProperty.SetMethod);
    }

    [Fact]
    public void ImplicitVarAutoProperty_BoolProperty_RoundTrips()
    {
        // Regression test: get_IsPriority() raised InvalidProgramException before fix
        var code = """
class Shipment {
    public var IsPriority: bool
    public init(isPriority: bool) {
        IsPriority = isPriority
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Shipment", throwOnError: true)!;
        var ctor = type.GetConstructor(BindingFlags.Public | BindingFlags.Instance, [typeof(bool)]);
        Assert.NotNull(ctor);

        var trueInstance = ctor!.Invoke([true]);
        var falseInstance = ctor.Invoke([false]);

        var prop = type.GetProperty("IsPriority", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.True((bool)prop.GetValue(trueInstance)!);
        Assert.False((bool)prop.GetValue(falseInstance)!);
    }

    [Fact]
    public void ImplicitVarAutoProperty_StaticProperty_GeneratesBackingField()
    {
        var code = """
class Registry {
    public static var Version: string
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSemanticModel(syntaxTree);
        var compilationType = compilation.SourceGlobalNamespace.LookupType("Registry");
        Assert.NotNull(compilationType);

        var propSymbol = Assert.Single(
            compilationType!.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Version");
        Assert.NotNull(propSymbol.GetMethod);
        Assert.NotNull(propSymbol.SetMethod);
        Assert.True(propSymbol.IsStatic);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Registry", throwOnError: true)!;

        var clrProp = type.GetProperty("Version", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(clrProp);
        Assert.NotNull(clrProp!.GetMethod);
        Assert.NotNull(clrProp.SetMethod);

        Assert.NotNull(type.GetField("<Version>k__BackingField", BindingFlags.Static | BindingFlags.NonPublic));

        clrProp.SetValue(null, "1.0");
        Assert.Equal("1.0", (string)clrProp.GetValue(null)!);
    }

    [Fact]
    public void PrivateStoredPropertyInitializer_LowersToFieldWithoutClrProperty()
    {
        var code = """
class Counter {
    private var orderCount: int = 0

    func Add(count: int) -> () {
        orderCount = orderCount + count
    }

    func Get() -> int => orderCount
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
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
        var type = runtimeAssembly.GetType("Counter", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;

        var property = type.GetProperty("orderCount", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.Null(property);

        var field = type.GetField("orderCount", BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(field);
        Assert.Equal(0, (int)field!.GetValue(instance)!);

        var addMethod = type.GetMethod("Add", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(addMethod);
        addMethod!.Invoke(instance, [3]);
        Assert.Equal(3, (int)field.GetValue(instance)!);
        var getMethod = type.GetMethod("Get", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(getMethod);
        Assert.Equal(3, (int)getMethod!.Invoke(instance, null)!);
    }

    [Fact]
    public void PromotedPrimaryConstructorProperty_CompoundAssignment_EmitsValidIL()
    {
        var code = """
import System.Console.*

val service = ShipmentOrderService()
service.RecordPending(2)
service.RecordPending(3)
WriteLine(service.PendingCount)

class ShipmentOrderService(var pendingCount: int = 0) {
    val PendingCount: int => pendingCount

    func RecordPending(count: int) -> () {
        pendingCount += count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        using var writer = new StringWriter();
        var originalOut = Console.Out;

        try
        {
            Console.SetOut(writer);
            _ = entryPoint!.Invoke(null, [Array.Empty<string>()]);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        Assert.Equal("5", writer.ToString().Trim());
    }

    [Fact]
    public void PrivatePromotedPrimaryConstructorProperty_LowersToFieldOnlyStorage()
    {
        var code = """
class Person(private var name: string) {
    func SetName(value: string) -> () {
        name = value
    }

    func GetName() -> string => name
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
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
        var type = runtimeAssembly.GetType("Person", throwOnError: true)!;

        var property = type.GetProperty("name", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.Null(property);

        var field = type.GetField("name", BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(field);
    }

    [Fact]
    public void ProtectedPromotedPrimaryConstructorProperty_LowersToFieldOnlyStorage()
    {
        var code = """
class Person(protected var name: string) {
    func SetName(value: string) -> () {
        name = value
    }

    func GetName() -> string => name
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
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
        var type = runtimeAssembly.GetType("Person", throwOnError: true)!;

        var property = type.GetProperty("name", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.Null(property);

        var field = type.GetField("name", BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(field);
    }

    [Fact]
    public void InternalPromotedPrimaryConstructorProperty_EmitsPropertyStorage()
    {
        var code = """
class Person(internal var Name: string)
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
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
        var type = runtimeAssembly.GetType("Person", throwOnError: true)!;

        var property = type.GetProperty("Name", BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(property);

        var directField = type.GetField("Name", BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.Null(directField);
    }

    [Fact]
    public void RecordEquality_IncludesPublicPromotedProperties()
    {
        var code = """
record class Person(Name: string, Age: int)
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var personType = loaded.Assembly.GetType("Person", throwOnError: true)!;
        var constructor = personType
            .GetConstructors(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
            .Single(c =>
            {
                var parameters = c.GetParameters();
                return parameters.Length == 2 &&
                       parameters[0].ParameterType == typeof(string) &&
                       parameters[1].ParameterType == typeof(int);
            });

        var left = constructor.Invoke(["Ada", 1]);
        var right = constructor.Invoke(["Ada", 2]);

        Assert.False(left.Equals(right));

        var opEquality = personType.GetMethod(
            "op_Equality",
            BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic,
            binder: null,
            [personType, personType],
            modifiers: null);
        Assert.NotNull(opEquality);
        Assert.False((bool)opEquality!.Invoke(null, [left, right])!);
    }

    [Fact]
    public void RecordEquality_ExcludesNonPublicPromotedProperties()
    {
        var code = """
record class Person(Name: string, private var Secret: int)
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var personType = loaded.Assembly.GetType("Person", throwOnError: true)!;
        var constructors = personType.GetConstructors(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        var primaryCtor = constructors.SingleOrDefault(c =>
        {
            var parameters = c.GetParameters();
            return parameters.Length == 2 &&
                   parameters[0].ParameterType == typeof(string) &&
                   parameters[1].ParameterType == typeof(int);
        });
        var copyCtor = constructors.SingleOrDefault(c =>
        {
            var parameters = c.GetParameters();
            return parameters.Length == 1 &&
                   parameters[0].ParameterType == personType;
        });
        Assert.NotNull(primaryCtor);
        Assert.NotNull(copyCtor);

        var left = primaryCtor!.Invoke(["Ada", 1]);
        var right = primaryCtor.Invoke(["Ada", 2]);
        Assert.True(left.Equals(right));
        Assert.Equal(left.GetHashCode(), right.GetHashCode());

        var toStringValue = left.ToString();
        Assert.NotNull(toStringValue);
        Assert.Contains("Name = \"Ada\"", toStringValue, StringComparison.Ordinal);
        Assert.DoesNotContain("Secret", toStringValue, StringComparison.Ordinal);

        var deconstruct = personType.GetMethod("Deconstruct", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(deconstruct);
        Assert.Single(deconstruct!.GetParameters());

        var copy = copyCtor!.Invoke([left]);
        Assert.True(copy.Equals(left));
    }

    [Fact]
    public void RecordInequalityOperator_InvertsValueEquality()
    {
        var code = """
record class Person(Name: string, Age: int)
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var personType = loaded.Assembly.GetType("Person", throwOnError: true)!;
        var constructor = personType
            .GetConstructors(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
            .Single(c =>
            {
                var parameters = c.GetParameters();
                return parameters.Length == 2 &&
                       parameters[0].ParameterType == typeof(string) &&
                       parameters[1].ParameterType == typeof(int);
            });

        var left = constructor.Invoke(["Ada", 1]);
        var same = constructor.Invoke(["Ada", 1]);
        var different = constructor.Invoke(["Ada", 2]);

        var opInequality = personType.GetMethod(
            "op_Inequality",
            BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic,
            binder: null,
            [personType, personType],
            modifiers: null);
        Assert.NotNull(opInequality);
        Assert.False((bool)opInequality!.Invoke(null, [left, same])!);
        Assert.True((bool)opInequality.Invoke(null, [left, different])!);
    }

    [Fact]
    public void DerivedRecordCopyConstructor_CopiesBaseAndDerivedState()
    {
        var code = """
abstract record class Person(Name: string)
record class Employee(Name: string, Id: int) : Person(Name)
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var employeeType = loaded.Assembly.GetType("Employee", throwOnError: true)!;
        var constructors = employeeType.GetConstructors(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        var primaryCtor = constructors.Single(c =>
        {
            var parameters = c.GetParameters();
            return parameters.Length == 2 &&
                   parameters[0].ParameterType == typeof(string) &&
                   parameters[1].ParameterType == typeof(int);
        });
        var copyCtor = constructors.Single(c =>
        {
            var parameters = c.GetParameters();
            return parameters.Length == 1 &&
                   parameters[0].ParameterType == employeeType;
        });

        var original = primaryCtor.Invoke(["Ada", 42]);
        var copy = copyCtor.Invoke([original]);

        Assert.True(copy.Equals(original));
        Assert.Equal("Ada", employeeType.GetProperty("Name")!.GetValue(copy));
        Assert.Equal(42, employeeType.GetProperty("Id")!.GetValue(copy));
    }

    [Fact]
    public void RecordDeconstruct_AssignsOutParameters()
    {
        var code = """
record class Person(Name: string, Age: int)
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var personType = loaded.Assembly.GetType("Person", throwOnError: true)!;
        var constructor = personType
            .GetConstructors(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
            .Single(c =>
            {
                var parameters = c.GetParameters();
                return parameters.Length == 2 &&
                       parameters[0].ParameterType == typeof(string) &&
                       parameters[1].ParameterType == typeof(int);
            });

        var person = constructor.Invoke(["Ada", 42]);
        var deconstruct = personType.GetMethod("Deconstruct", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(deconstruct);

        object?[] args = [null, null];
        deconstruct!.Invoke(person, args);

        Assert.Equal("Ada", args[0]);
        Assert.Equal(42, args[1]);
    }

}
