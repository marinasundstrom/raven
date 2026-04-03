using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MemberBindingCodeGenTests
{
    [Fact]
    public void MemberBinding_StaticField_FromReferenceAssembly_ResolvesRuntimeField()
    {
        const string code = """
class Program {
    public static func Get() -> string {
        .Empty
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var get = type.GetMethod("Get")!;

        var value = (string)get.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(string.Empty, value);
    }

    [Fact]
    public void ExtensionPropertyGetter_ReturnsTargetTypedCaseInvocation_FromReturnStatement()
    {
        const string code = """
union Option<T> {
    Some(value: T)
    None
}

union Result<T, E> {
    Ok(value: T)
    Error(value: E)
}

extension ResultExtensions<T, E> for Result<T, E> {
    val IsOk: Option<T> {
        get {
            if self is .Ok(val value) {
                return .Some(value)
            }
            .None
        }
    }
}

class Program {
    public static func Get() -> string {
        val r: Result<int, string> = .Ok(42)
        r.IsOk.ToString()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var get = type.GetMethod("Get")!;

        var value = (string)get.Invoke(null, Array.Empty<object>())!;
        Assert.Contains("Some", value, StringComparison.Ordinal);
    }

    [Fact]
    public void GenericExtension_GetType_ResultCanBeStoredAsType()
    {
        const string code = """
import System.*
import System.Collections.Generic.*

extension TypeProbe<T> for T {
    func TypeNameLength() -> int {
        val type = self.GetType()
        return type.Name.Length
    }
}

class Program {
    public static func Run() -> int {
        val obj = List<int>()
        return obj.TypeNameLength()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        var value = (int)run.Invoke(null, Array.Empty<object>())!;
        Assert.True(value > 0);
    }

    [Fact]
    public void GenericExtension_GetType_WithLambdaPredicate_ExecutesWithoutBadImageFormat()
    {
        const string code = """
import System.*
import System.Linq.*
import System.Collections.Generic.*

extension TypeProbe<T> for T {
    func CountReadableInstanceProperties() -> int {
        val type = self.GetType()
        return type
            .GetProperties()
            .Where(pi => !(pi.GetMethod?.IsStatic ?? false) && pi.GetMethod?.GetParameters()?.Length == 0)
            .Count()
    }
}

class Program {
    public static func Run() -> int {
        val obj = List<int>()
        return obj.CountReadableInstanceProperties()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        var value = (int)run.Invoke(null, Array.Empty<object>())!;
        Assert.True(value > 0);
    }

    [Fact]
    public void ResultWithMessage_GenericMapErrorLambda_ExecutesWithoutBadImageFormat()
    {
        const string code = """
interface IError {
    val Message: string
}

record ParseError(val Message: string) : IError

record ContextError<TError: IError>(
    val Message: string,
    val InnerError: TError
) : IError {
    val Cause: TError => InnerError
}

union Result<T, E> {
    Ok(value: T)
    Error(value: E)
}

extension ErrorExtensions<TError: IError> for TError {
    func WithMessage(message: string) -> ContextError<TError> {
        return ContextError<TError>(message, self)
    }
}

extension ResultExtensions<T, E> for Result<T, E> {
    func MapError<E2>(mapper: E -> E2) -> Result<T, E2> {
        self match {
            Ok(val value) => Ok(value)
            Error(val error) => Error(mapper(error))
        }
    }
}

extension ResultErrorContextExtensions<T, E: IError> for Result<T, E> {
    func WithMessage(message: string) -> Result<T, ContextError<E>> {
        self.MapError(error => error.WithMessage(message))
    }
}

class Program {
    public static func Run() -> string {
        val result: Result<int, ParseError> = Error(ParseError("invalid"))
        val wrapped = result.WithMessage("context")

        return wrapped match {
            Ok(val value) => value.ToString()
            Error(val error) => error.Message + ":" + error.Cause.Message
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        var value = (string)run.Invoke(null, Array.Empty<object>())!;
        Assert.Equal("context:invalid", value);
    }
}
