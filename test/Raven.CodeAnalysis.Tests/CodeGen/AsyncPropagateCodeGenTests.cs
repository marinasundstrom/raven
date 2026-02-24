using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class AsyncPropagateCodeGenTests
{
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
}
