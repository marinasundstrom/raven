using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class IntegralCastCodeGenTests
{
    [Fact]
    public void NumericWidening_PreservesBoundaryValuesAcrossLocalsCallsAndReturns()
    {
        const string code = """
        class Widening {
            func Short(value: short) -> int { let local: int = value; return Int(local) }
            func Int(value: int) -> int { return value }
            func Unsigned(value: uint) -> long { let local: long = value; return Long(local) }
            func Long(value: long) -> long { return value }
            func Floating(value: ulong) -> double { let local: double = value; return Double(local) }
            func Double(value: double) -> double { return value }
            func Single(value: uint) -> float { return value }
            func Decimal(value: ulong) -> decimal { let local: decimal = value; return local }
            func Character(value: char) -> uint { let local: uint = value; return local }
        }
        """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("numeric_widening", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(code)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var type = loaded.Assembly.GetType("Widening", true)!;
        var instance = Activator.CreateInstance(type)!;
        object? Invoke(string name, object value) => type.GetMethod(name)!.Invoke(instance, [value]);
        Assert.Equal(-32768, Invoke("Short", short.MinValue));
        Assert.Equal(4294967295L, Invoke("Unsigned", uint.MaxValue));
        Assert.Equal((double)ulong.MaxValue, Invoke("Floating", ulong.MaxValue));
        Assert.Equal((float)uint.MaxValue, Invoke("Single", uint.MaxValue));
        Assert.Equal((decimal)ulong.MaxValue, Invoke("Decimal", ulong.MaxValue));
        Assert.Equal(65535u, Invoke("Character", char.MaxValue));
    }

    [Fact]
    public void IntegralCasts_ExecuteNarrowingAndUnsignedConversions()
    {
        const string code = """
        class Casts {
            func Signed(value: int) -> sbyte { return (sbyte)value }
            func Short(value: int) -> short { return (short)value }
            func UnsignedShort(value: int) -> ushort { return (ushort)value }
            func Unsigned(value: int) -> uint { return (uint)value }
            func UnsignedFromInt(value: int) -> ulong { return (ulong)value }
            func UnsignedLong(value: long) -> ulong { return (ulong)value }
            func Widen(value: uint) -> long { return (long)value }
            func WidenUnsigned(value: uint) -> ulong { return (ulong)value }
            func Narrow(value: ulong) -> int { return (int)value }
        }
        """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("integral_casts", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(code)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var type = loaded.Assembly.GetType("Casts", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        object? Invoke(string name, object value) => type.GetMethod(name)!.Invoke(instance, [value]);
        Assert.Equal((sbyte)-1, Invoke("Signed", 255));
        Assert.Equal((short)-1, Invoke("Short", 65535));
        Assert.Equal(ushort.MaxValue, Invoke("UnsignedShort", -1));
        Assert.Equal(uint.MaxValue, Invoke("Unsigned", -1));
        Assert.Equal(ulong.MaxValue, Invoke("UnsignedLong", -1L));
        Assert.Equal(ulong.MaxValue, Invoke("UnsignedFromInt", -1));
        Assert.Equal(4294967295L, Invoke("Widen", uint.MaxValue));
        Assert.Equal(4294967295UL, Invoke("WidenUnsigned", uint.MaxValue));
        Assert.Equal(-1, Invoke("Narrow", ulong.MaxValue));
    }
}
