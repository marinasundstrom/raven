using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.Versioning;
using System.Security;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class CustomAttributeEmissionTests
{
    [Fact]
    public void AttributeTypeArgument_CanReferenceSourceTypeDeclaredLater()
    {
        const string source = """
import System.*
import System.Text.Json.Serialization.*

[JsonDerivedType(typeof(Button), "Button")]
open class Control { }

class Button : Control { }
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var controlType = assembly.GetType("Control", throwOnError: true)!;
        var attribute = Assert.Single(controlType.GetCustomAttributesData(), a => a.AttributeType.Name == "JsonDerivedTypeAttribute");
        Assert.Equal(assembly.GetType("Button", throwOnError: true), attribute.ConstructorArguments[0].Value);
    }

    [Fact]
    public void CustomAttributes_AreEmitted()
    {
        const string source = """
[System.Obsolete("Widget")]
class Widget
{
    [System.Obsolete("Field")]
    public field storage: string

    public var Value: string { get; set; }

    [System.Obsolete("Method")]
    public func M([System.Obsolete("Parameter")] x: string) -> string
    {
        return x
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var widgetType = assembly.GetType("Widget", throwOnError: true)!;

        var typeAttribute = Assert.Single(widgetType.GetCustomAttributesData(), a => a.AttributeType.Name == "ObsoleteAttribute");
        Assert.Equal("Widget", typeAttribute.ConstructorArguments[0].Value);

        var field = widgetType.GetField("storage", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(field);
        var fieldAttribute = Assert.Single(field!.GetCustomAttributesData(), a => a.AttributeType.Name == "ObsoleteAttribute");
        Assert.Equal("Field", fieldAttribute.ConstructorArguments[0].Value);

        var property = widgetType.GetProperty("Value", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(property);

        var method = widgetType.GetMethod("M", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(method);
        var methodAttribute = Assert.Single(method!.GetCustomAttributesData(), a => a.AttributeType.Name == "ObsoleteAttribute");
        Assert.Equal("Method", methodAttribute.ConstructorArguments[0].Value);

        var parameter = Assert.Single(method.GetParameters());
        Assert.Empty(parameter.GetCustomAttributesData());
    }

    [Fact]
    public void AutoPropertyMembers_HaveSynthesizedAttributes()
    {
        const string source = """
class Widget
{
    public var Value: string { get; set; }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var widgetType = assembly.GetType("Widget", throwOnError: true)!;
        var property = widgetType.GetProperty("Value", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(property);

        var getter = property!.GetGetMethod();
        Assert.NotNull(getter);
        Assert.Contains(getter!.GetCustomAttributesData(), a => a.AttributeType == typeof(CompilerGeneratedAttribute));

        var setter = property.GetSetMethod();
        Assert.NotNull(setter);
        Assert.Contains(setter!.GetCustomAttributesData(), a => a.AttributeType == typeof(CompilerGeneratedAttribute));

        var backingField = widgetType.GetField("<Value>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(backingField);

        var fieldAttributes = backingField!.GetCustomAttributesData();
        Assert.Contains(fieldAttributes, a => a.AttributeType == typeof(CompilerGeneratedAttribute));
        var debuggerBrowsable = Assert.Single(fieldAttributes, a => a.AttributeType == typeof(DebuggerBrowsableAttribute));
        Assert.Equal(DebuggerBrowsableState.Never, (DebuggerBrowsableState)debuggerBrowsable.ConstructorArguments[0].Value!);
    }

    [Fact]
    public void ClosureClass_IsNamedDisplayClass_AndHasCompilerGeneratedAttribute()
    {
        const string source = """
class C
{
    func Run() -> int
    {
        var x = 1
        val f = () -> int => x
        return f()
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var outerType = assembly.GetType("C", throwOnError: true)!;

        var closureType = outerType.GetNestedTypes(BindingFlags.NonPublic)
            .FirstOrDefault(t => t.Name.Contains("DisplayClass"));

        Assert.NotNull(closureType);
        Assert.Contains(closureType!.GetCustomAttributesData(), a => a.AttributeType == typeof(CompilerGeneratedAttribute));
    }

    [Fact]
    public void AssemblyTargetedAttribute_IsEmitted()
    {
        const string source = """
import System.Runtime.Versioning.*

[assembly: TargetFramework(".NETCoreApp,Version=v9.0")]

class C { }
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var targetFrameworkAttribute = Assert.Single(
            assembly.GetCustomAttributesData(),
            static a => a.AttributeType == typeof(TargetFrameworkAttribute));

        Assert.Equal(".NETCoreApp,Version=v9.0", targetFrameworkAttribute.ConstructorArguments[0].Value);
    }

    [Fact]
    public void ModuleTargetedAttribute_IsEmitted()
    {
        const string source = """
import System.Security.*

[module: UnverifiableCode]
class C { }
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var moduleAttribute = Assert.Single(
            assembly.ManifestModule.GetCustomAttributesData(),
            static a => a.AttributeType == typeof(UnverifiableCodeAttribute));
        Assert.NotNull(moduleAttribute);
    }

    [Fact]
    public void FieldTargetedAutoPropertyAttribute_IsEmittedOnBackingFieldOnly()
    {
        const string source = """
import System.*

class FieldMarkerAttribute : Attribute
{
    init(value: string) { }
}

class Widget
{
    [field: FieldMarker("backing")]
    var Value: string { get; set; }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var widgetType = assembly.GetType("Widget", throwOnError: true)!;

        var property = widgetType.GetProperty("Value", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(property);
        Assert.DoesNotContain(property!.GetCustomAttributesData(), static a => a.AttributeType.Name == "FieldMarkerAttribute");

        var backingField = widgetType.GetField("<Value>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(backingField);

        var fieldAttribute = Assert.Single(
            backingField!.GetCustomAttributesData(),
            static a => a.AttributeType.Name == "FieldMarkerAttribute");
        Assert.Equal("backing", fieldAttribute.ConstructorArguments[0].Value);
    }
}
