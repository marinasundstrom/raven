using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.Versioning;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

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
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var controlType = assembly.GetType("Control", throwOnError: true)!;
        var attribute = Assert.Single(controlType.GetCustomAttributesData(), a => a.AttributeType.Name == "JsonDerivedTypeAttribute");
        Assert.Equal(assembly.GetType("Button", throwOnError: true), attribute.ConstructorArguments[0].Value);
    }

    [Fact]
    public void CustomAttributes_AreEmitted()
    {
        const string source = """
class InfoAttribute : System.Attribute
{
    public init(name: string, type: System.Type) {}
}

[Info(name: "Widget", type: typeof(int))]
class Widget
{
    [Info(name: "Field", type: typeof(string))]
    public var field: string

    [Info(name: "Property", type: typeof(bool))]
    public Value: string { get; set; }

    [Info(name: "Method", type: typeof(double))]
    public M([Info(name: "Parameter", type: typeof(long))] x: string) -> string
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

        var typeAttribute = Assert.Single(widgetType.GetCustomAttributesData(), a => a.AttributeType.Name == "InfoAttribute");
        Assert.Equal("Widget", typeAttribute.ConstructorArguments[0].Value);
        Assert.Equal(typeof(int), typeAttribute.ConstructorArguments[1].Value);

        var field = widgetType.GetField("field", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(field);
        var fieldAttribute = Assert.Single(field!.GetCustomAttributesData(), a => a.AttributeType.Name == "InfoAttribute");
        Assert.Equal("Field", fieldAttribute.ConstructorArguments[0].Value);
        Assert.Equal(typeof(string), fieldAttribute.ConstructorArguments[1].Value);

        var property = widgetType.GetProperty("Value", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(property);
        var propertyAttribute = Assert.Single(property!.GetCustomAttributesData(), a => a.AttributeType.Name == "InfoAttribute");
        Assert.Equal("Property", propertyAttribute.ConstructorArguments[0].Value);
        Assert.Equal(typeof(bool), propertyAttribute.ConstructorArguments[1].Value);

        var method = widgetType.GetMethod("M", BindingFlags.Instance | BindingFlags.Public);
        Assert.NotNull(method);
        var methodAttribute = Assert.Single(method!.GetCustomAttributesData(), a => a.AttributeType.Name == "InfoAttribute");
        Assert.Equal("Method", methodAttribute.ConstructorArguments[0].Value);
        Assert.Equal(typeof(double), methodAttribute.ConstructorArguments[1].Value);

        var parameter = Assert.Single(method.GetParameters());
        var parameterAttribute = Assert.Single(parameter.GetCustomAttributesData(), a => a.AttributeType.Name == "InfoAttribute");
        Assert.Equal("Parameter", parameterAttribute.ConstructorArguments[0].Value);
        Assert.Equal(typeof(long), parameterAttribute.ConstructorArguments[1].Value);
    }

    [Fact]
    public void AutoPropertyMembers_HaveSynthesizedAttributes()
    {
        const string source = """
class Widget
{
    public Value: string { get; set; }
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
}
