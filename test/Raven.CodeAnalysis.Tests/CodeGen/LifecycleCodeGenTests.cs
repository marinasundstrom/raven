using System;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class LifecycleCodeGenTests
{
    [Fact]
    public void StaticInitAndFinally_Emit()
    {
        const string code = """
import System.*

class Customer {
    static init {
        var x = 1
    }

    init {
        var y = 2
    }

    finally {
        var z = 3
    }
}
""";

        using var loaded = EmitAndLoad(code, allowUnsafe: false);
        var customerType = loaded.Assembly.GetType("Customer", throwOnError: true)!;

        Assert.NotNull(customerType.TypeInitializer);
        Assert.NotNull(customerType.GetConstructor(Array.Empty<Type>()));
        Assert.NotNull(customerType.GetMethod("Finalize", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.DeclaredOnly));
    }

    [Fact]
    public void UnsafeLifecycleBlocks_Emit()
    {
        const string code = """
import System.*

class UnsafeLifecycle {
    public static field StaticValue: int = 0
    public field Value: int = 0
    public field FinalValue: int = 0

    unsafe static init {
        var local = 7
        val pointer: *int = &local;
        StaticValue = *pointer
    }

    unsafe init {
        var local = 11
        val pointer: *int = &local;
        Value = *pointer
    }

    unsafe finally {
        var local = 13
        val pointer: *int = &local;
        FinalValue = *pointer
    }
}
""";

        using var loaded = EmitAndLoad(code, allowUnsafe: true);
        var type = loaded.Assembly.GetType("UnsafeLifecycle", throwOnError: true)!;

        RuntimeHelpers.RunClassConstructor(type.TypeHandle);
        var staticValue = (int)type.GetField("StaticValue", BindingFlags.Public | BindingFlags.Static)!.GetValue(null)!;
        Assert.Equal(7, staticValue);

        var instance = Activator.CreateInstance(type)!;
        var instanceValue = (int)type.GetField("Value", BindingFlags.Public | BindingFlags.Instance)!.GetValue(instance)!;
        Assert.Equal(11, instanceValue);

        Assert.NotNull(type.GetMethod("Finalize", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.DeclaredOnly));
    }

    private static TestAssemblyLoader.LoadedAssembly EmitAndLoad(string code, bool allowUnsafe)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithAllowUnsafe(allowUnsafe);

        var compilation = Compilation.Create("lifecycle_codegen", options)
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        return TestAssemblyLoader.LoadFromStream(peStream, references);
    }
}
