using System;
using System.IO;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.MetadataFixtures.Pins;

namespace Raven.CodeAnalysis.Tests;

public class EnumCodeGenTests
{
    [Fact]
    public void SourceEnumBackingFieldRetainsRequiredCliFlags()
    {
        const string code = """
            public enum Mode { Default = 0, Active = 4 }
            """;
        var compilation = Compilation.Create("enum_field_metadata", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(code)).AddReferences(TestMetadataReferences.Default);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        stream.Position = 0;
        using var pe = new PEReader(stream, PEStreamOptions.LeaveOpen);
        var reader = pe.GetMetadataReader();
        var type = reader.TypeDefinitions.Select(reader.GetTypeDefinition)
            .Single(type => reader.GetString(type.Name) == "Mode");
        var field = type.GetFields().Select(reader.GetFieldDefinition)
            .Single(field => reader.GetString(field.Name) == "value__");
        Assert.Equal(FieldAttributes.Public | FieldAttributes.SpecialName | FieldAttributes.RTSpecialName, field.Attributes);
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        var enumType = loaded.Assembly.GetType("Mode", true)!;
        Assert.Equal(typeof(int), Enum.GetUnderlyingType(enumType));
        Assert.Equal(4, Convert.ToInt32(Enum.Parse(enumType, "Active")));
    }

    [Theory]
    [InlineData(".Public | .Instance | .Static")]
    [InlineData(".Public | (.Instance | .Static)")]
    [InlineData("(.Public | .Instance) | .Static")]
    public void NestedTargetTypedFlagsRetainContext(string expression)
    {
        const string code = """
            import System.Reflection.*
            class Program {
                public static func Run() -> int {
                    let flags: BindingFlags = FLAGS
                    return (int)flags
                }
            }
            """;
        var compilation = Compilation.Create("nested_enum_flags", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(code.Replace("FLAGS", expression))).AddReferences(TestMetadataReferences.Default);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        var method = loaded.Assembly.GetType("Program", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        Assert.Equal(28, method.Invoke(null, Array.Empty<object>()));
    }

    [Fact]
    public void SourceEnumConstantPattern_OnPropertyResult_MatchesQualifiedMember()
    {
        const string code = """
enum PinEventTypes {
    Falling
    Rising
}

record Args(val ChangeType: PinEventTypes)

class Program {
    public static func Run() -> bool {
        let args = Args(PinEventTypes.Rising)

        if args.ChangeType is PinEventTypes.Rising {
            return true
        }

        return false
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("source_enum_property_constant_pattern", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var value = (bool)run.Invoke(null, Array.Empty<object>())!;

        Assert.True(value);
    }

    [Fact]
    public void MetadataEnumConstantPatterns_MatchExactValue()
    {
        const string code = """
import System.Text.Json.*

class Program {
    public static func Run() -> bool {
        return JsonValueKind.Number is JsonValueKind.Number &&
            (JsonValueKind.Number is JsonValueKind.String) == false &&
            JsonValueKind.True is .True
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("metadata_enum_constant_pattern", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var value = (bool)run.Invoke(null, Array.Empty<object>())!;

        Assert.True(value);
    }

    [Fact]
    public void MetadataEnumConstantPattern_OnPropertyResult_MatchesExactValue()
    {
        const string code = """
import System.Text.Json.*

class Program {
    public static func Run() -> bool {
        use document = JsonDocument.Parse("32")
        let element = document.RootElement

        if element.ValueKind is JsonValueKind.String {
            return false
        }

        if element.ValueKind is JsonValueKind.Number {
            return true
        }

        return false
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("metadata_enum_property_constant_pattern", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var value = (bool)run.Invoke(null, Array.Empty<object>())!;

        Assert.True(value);
    }

    [Fact]
    public void CustomMetadataEnumConstantPattern_OnPropertyResult_MatchesQualifiedMember()
    {
        const string code = """
import Raven.MetadataFixtures.Pins.*

class Program {
    public static func Run(args: PinEventArgs) -> bool {
        if args.ChangeType is PinEventTypes.Rising {
            return true
        }

        return false
    }
}
""";

        var fixtureReference = MetadataReference.CreateFromFile(typeof(PinEventTypes).Assembly.Location);
        var references = TestMetadataReferences.Default.Append(fixtureReference).ToArray();
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("custom_metadata_enum_property_constant_pattern", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var parameterType = Assert.Single(run.GetParameters()).ParameterType;
        var enumType = parameterType.GetProperty(nameof(PinEventArgs.ChangeType))!.PropertyType;
        var rising = Enum.Parse(enumType, nameof(PinEventTypes.Rising));
        var args = Activator.CreateInstance(parameterType, rising)!;
        var value = (bool)run.Invoke(null, [args])!;

        Assert.True(value);
    }

    [Fact]
    public void ByteBackedMetadataEnumMember_EmitsAsConstantOperand()
    {
        const string code = """
import System.Text.Json.*

class Program {
    public static func Run() -> bool {
        return JsonValueKind.Null == JsonValueKind.Null
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("metadata_byte_enum_constant", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var value = (bool)run.Invoke(null, Array.Empty<object>())!;

        Assert.True(value);
    }

    [Fact]
    public void TargetTypedEnumMembers_EmitRuntimeValues()
    {
        const string code = """
enum DeviceType {
    Harddrive
    Monitor
    CPU
}

class Program {
    public static func Run() -> bool {
        return Pick() == DeviceType.CPU && IsMonitor(.Monitor)
    }

    static func Pick() -> DeviceType {
        return .CPU
    }

    static func IsMonitor(value: DeviceType) -> bool {
        return value == DeviceType.Monitor
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("target_typed_enum_member", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var value = (bool)run.Invoke(null, Array.Empty<object>())!;

        Assert.True(value);
    }

    [Fact]
    public void TargetTypedEnumMember_EmitsRuntimeValue()
    {
        const string code = """
enum DeviceType {
    Harddrive
    Monitor
    CPU
}

class Program {
    public static func Run() -> bool {
        let selected: DeviceType = .Monitor
        return selected == DeviceType.Monitor
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("target_typed_enum_member", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var value = (bool)run.Invoke(null, Array.Empty<object>())!;

        Assert.True(value);
    }

    [Fact]
    public void ExplicitEnumConversions_FollowClrOpenValueRules()
    {
        const string code = """
enum DeviceType : int {
    Harddrive = 1
    Monitor = 2
    CPU = 3
}

enum OtherDeviceType : byte {
    CPU = 3
}

class Program {
    public static func Run() -> bool {
        let unknown = (DeviceType)123
        let other = (OtherDeviceType)DeviceType.CPU
        return (int)unknown == 123 &&
            (long)DeviceType.Monitor == 2L &&
            other == OtherDeviceType.CPU
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("open_enum_conversion", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var value = (bool)run.Invoke(null, Array.Empty<object>())!;

        Assert.True(value);
    }
}
