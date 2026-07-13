using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class PatternLocalCodeGenTests
{
    [Fact]
    public void NullPattern_LoweredForIsAndMatchExpressions()
    {
        var code = """
class NullPatterns {
    public static func IsNull(value: object?) -> int {
        if value is null {
            return 1
        }

        return 0
    }

    public static func MatchNull(value: object?) -> int {
        return match value {
            null => 1
            _ => 0
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("null_pattern_lowering", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("NullPatterns", throwOnError: true)!;

        Assert.Equal(1, type.GetMethod("IsNull")!.Invoke(null, [null]));
        Assert.Equal(0, type.GetMethod("IsNull")!.Invoke(null, ["value"]));
        Assert.Equal(1, type.GetMethod("MatchNull")!.Invoke(null, [null]));
        Assert.Equal(0, type.GetMethod("MatchNull")!.Invoke(null, ["value"]));
    }

    [Fact]
    public void EmptyPropertyPattern_LoweredToNonNullPatternForIsAndMatchExpressions()
    {
        var code = """
class EmptyPropertyPatterns {
    public static func IsNonNull(value: object?) -> int {
        if value is { } {
            return 1
        }

        return 0
    }

    public static func MatchNonNull(value: object?) -> int {
        return match value {
            { } => 1
            _ => 0
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("empty_property_pattern_lowering", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("EmptyPropertyPatterns", throwOnError: true)!;

        Assert.Equal(0, type.GetMethod("IsNonNull")!.Invoke(null, [null]));
        Assert.Equal(1, type.GetMethod("IsNonNull")!.Invoke(null, ["value"]));
        Assert.Equal(0, type.GetMethod("MatchNonNull")!.Invoke(null, [null]));
        Assert.Equal(1, type.GetMethod("MatchNonNull")!.Invoke(null, ["value"]));
    }

    [Fact]
    public void TypedEmptyPropertyPattern_LoweredToDeclarationPatternForIsAndMatchExpressions()
    {
        var code = """
class EmptyPropertyPatterns {
    public static func IsWidget(value: object?) -> int {
        if value is Widget { } {
            return 1
        }

        return 0
    }

    public static func MatchWidget(value: object?) -> int {
        return match value {
            Widget { } => 1
            _ => 0
        }
    }
}

class Widget {}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("typed_empty_property_pattern_lowering", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("EmptyPropertyPatterns", throwOnError: true)!;
        var widgetType = loaded.Assembly.GetType("Widget", throwOnError: true)!;
        var widget = Activator.CreateInstance(widgetType)!;

        Assert.Equal(0, type.GetMethod("IsWidget")!.Invoke(null, [null]));
        Assert.Equal(0, type.GetMethod("IsWidget")!.Invoke(null, ["value"]));
        Assert.Equal(1, type.GetMethod("IsWidget")!.Invoke(null, [widget]));
        Assert.Equal(0, type.GetMethod("MatchWidget")!.Invoke(null, [null]));
        Assert.Equal(0, type.GetMethod("MatchWidget")!.Invoke(null, ["value"]));
        Assert.Equal(1, type.GetMethod("MatchWidget")!.Invoke(null, [widget]));
    }

    [Fact]
    public void RangePattern_LoweredToComparisonPatternsForIsAndMatchExpressions()
    {
        var code = """
class RangePatterns {
    public static func IsClosed(value: int) -> int {
        if value is 2..4 {
            return 1
        }

        return 0
    }

    public static func IsExclusive(value: int) -> int {
        if value is 2..<4 {
            return 1
        }

        return 0
    }

    public static func MatchClosed(value: int) -> int {
        return match value {
            2..4 => 1
            _ => 0
        }
    }

    public static func MatchExclusive(value: int) -> int {
        return match value {
            2..<4 => 1
            _ => 0
        }
    }

    public static func MatchOpen(value: int) -> int {
        return match value {
            ..1 => 10
            5.. => 20
            _ => 0
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("range_pattern_lowering", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("RangePatterns", throwOnError: true)!;

        Assert.Equal(0, type.GetMethod("IsClosed")!.Invoke(null, [1]));
        Assert.Equal(1, type.GetMethod("IsClosed")!.Invoke(null, [2]));
        Assert.Equal(1, type.GetMethod("IsClosed")!.Invoke(null, [4]));
        Assert.Equal(0, type.GetMethod("IsClosed")!.Invoke(null, [5]));
        Assert.Equal(1, type.GetMethod("IsExclusive")!.Invoke(null, [2]));
        Assert.Equal(1, type.GetMethod("IsExclusive")!.Invoke(null, [3]));
        Assert.Equal(0, type.GetMethod("IsExclusive")!.Invoke(null, [4]));
        Assert.Equal(0, type.GetMethod("MatchClosed")!.Invoke(null, [1]));
        Assert.Equal(1, type.GetMethod("MatchClosed")!.Invoke(null, [2]));
        Assert.Equal(1, type.GetMethod("MatchClosed")!.Invoke(null, [4]));
        Assert.Equal(0, type.GetMethod("MatchClosed")!.Invoke(null, [5]));
        Assert.Equal(1, type.GetMethod("MatchExclusive")!.Invoke(null, [2]));
        Assert.Equal(1, type.GetMethod("MatchExclusive")!.Invoke(null, [3]));
        Assert.Equal(0, type.GetMethod("MatchExclusive")!.Invoke(null, [4]));
        Assert.Equal(10, type.GetMethod("MatchOpen")!.Invoke(null, [1]));
        Assert.Equal(0, type.GetMethod("MatchOpen")!.Invoke(null, [3]));
        Assert.Equal(20, type.GetMethod("MatchOpen")!.Invoke(null, [5]));
    }

    [Fact]
    public void GuardStyleIsNotPatternLocal_IsAvailableAfterEarlyReturn()
    {
        var code = """
class GuardedText {
    func Read(value: object) -> string {
        if value is not string text {
            return "not text"
        }

        return text
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("guarded_pattern_local", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("GuardedText", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Read")!;

        Assert.Equal("hello", method.Invoke(instance, ["hello"]));
        Assert.Equal("not text", method.Invoke(instance, [42]));
    }

    [Fact]
    public void GuardStyleIsNotPatternLocal_CanBeUsedInSubsequentComputation()
    {
        var code = """
class GuardedText {
    func ReadLength(value: object) -> int {
        if value is not string text {
            return -1
        }

        val suffix = text + "!"
        return suffix.Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("guarded_pattern_local_computation", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("GuardedText", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("ReadLength")!;

        Assert.Equal(6, method.Invoke(instance, ["hello"]));
        Assert.Equal(-1, method.Invoke(instance, [42]));
    }
}
