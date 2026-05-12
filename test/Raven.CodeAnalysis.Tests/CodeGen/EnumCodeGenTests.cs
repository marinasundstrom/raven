using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class EnumCodeGenTests
{
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
        val selected: DeviceType = .Monitor
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
        val unknown = (DeviceType)123
        val other = (OtherDeviceType)DeviceType.CPU
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
