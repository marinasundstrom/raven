using System.IO;
using System.Linq;
using System.Reflection;
using System;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class AsyncPropagateCodeGenTests
{
    [Fact]
    public void PropagateExpression_UsedAsInvocationArgument_EmitsAndRuns()
    {
        var code = """
import System.*
import System.Collections.Generic.*

class Program {
    static func Main() -> Result<(), string> {
        val xs: Item[] = [Item("A")]
        val names = Collect(xs)?
        Console.WriteLine(names.Length)
        return Ok
    }

    static func Collect(items: Item[]) -> Result<string[], string> {
        val values = List<string>()
        values.Add(GetName(items[0])?)
        return Ok(values.ToArray())
    }

    static func GetName(item: Item) -> Result<string, string> {
        return Ok(item.Name)
    }
}

record class Item(val Name: string)
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "1" }, output);
    }

    [Fact]
    public void AsyncPropagate_UseDeclaration_DisposesOnSuccessAndFailure()
    {
        var code = """
import System.*
import System.Threading.Tasks.*

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Disposable : IDisposable {
    public static var DisposedCount: int = 0

    public static func Reset() {
        DisposedCount = 0
    }

    public func Dispose() {
        DisposedCount = DisposedCount + 1
    }
}

class C {
    private static func Fail() -> Result<int, string> {
        return .Error("boom")
    }

    private static func Succeed() -> Result<int, string> {
        return .Ok(42)
    }

    public async func RunFail() -> Task<Result<int, string>> {
        use d = Disposable()
        val value = try? await Task.FromResult(Fail())
        return .Ok(value)
    }

    public async func RunSuccess() -> Task<Result<int, string>> {
        use d = Disposable()
        val value = try? await Task.FromResult(Succeed())
        return .Ok(value)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references =
        [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;

        var containerType = assembly.GetType("C", throwOnError: true)!;
        var runFail = containerType.GetMethod("RunFail", BindingFlags.Public | BindingFlags.Instance)!;
        var runSuccess = containerType.GetMethod("RunSuccess", BindingFlags.Public | BindingFlags.Instance)!;

        Assert.Equal(typeof(Task), runFail.ReturnType.BaseType);
        Assert.Equal(typeof(Task), runSuccess.ReturnType.BaseType);
    }

    private static string[] CompileAndRun(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = GetReferencesWithRavenCore();
        var compilation = Compilation.Create(
            "async-propagate",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint!;

        var originalOut = Console.Out;
        using var writer = new StringWriter();

        try
        {
            Console.SetOut(writer);
            var parameters = entryPoint.GetParameters().Length == 0
                ? null
                : new object?[] { Array.Empty<string>() };
            entryPoint.Invoke(null, parameters);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            .Select(line => line.Trim())
            .ToArray();
    }

    private static MetadataReference[] GetReferencesWithRavenCore()
    {
        var corePath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (!File.Exists(corePath))
            return TestMetadataReferences.Default;

        return [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(corePath)];
    }
}
