using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class AssignmentStatementTests
{
    [Fact]
    public void AssignmentStatement_EmitsAndRuns()
    {
        var code = """
class Foo {
    func Run() -> int {
        var x: int = 0
        x = 42
        return x
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);
        var value = (int)method!.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(42, value);
    }

    [Fact]
    public void DiscardAssignment_UnitReturningInvocation_DoesNotThrow()
    {
        var code = """
class Foo {
    func Run() -> unit {
        _ = System.Console.WriteLine("discard")
        return ()
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);

        var returnValue = method!.Invoke(instance, Array.Empty<object?>());
        Assert.Null(returnValue);
    }

    [Fact]
    public void DiscardLetBinding_UnitReturningInvocation_DoesNotThrow()
    {
        var code = """
class Foo {
    func Run() -> unit {
        val _ = System.Console.WriteLine("discard")
        return ()
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);

        var returnValue = method!.Invoke(instance, Array.Empty<object?>());
        Assert.Null(returnValue);
    }

    [Fact]
    public void DiscardLetBinding_DoesNotDeclareSymbol()
    {
        var code = """
class Foo {
    func Run() -> unit {
        val _ = 42
        return ()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        var model = compilation.GetSemanticModel(syntaxTree);
        var declarator = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single();

        Assert.Null(model.GetDeclaredSymbol(declarator));
    }

    [Fact]
    public void CollectionPatternDeclaration_WithMiddleRest_EmitsAndRuns()
    {
        var code = """
class Foo {
    func Run() -> int {
        val values: int[] = [1, 2, 3, 4]
        val [first, ..middle, last] = values
        return first + middle[0] + last
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);
        var value = (int)method!.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(7, value);
    }

    [Fact]
    public void CollectionPatternDeclaration_WithMiddleRest_OnList_EmitsAndRuns()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    func Run() -> int {
        val values: List<int> = [1, 2, 3, 4]
        val [first, ..middle, last] = values
        return first + middle[0] + last
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);
        var value = (int)method!.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(7, value);
    }

    [Fact]
    public void CollectionPatternDeclaration_WithFixedSegment_EmitsAndRuns()
    {
        var code = """
class Foo {
    func Run() -> int {
        val values: int[] = [1, 2, 3]
        val [..2 start, tail] = values
        return start[0] + start[1] + tail
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);
        var value = (int)method!.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(6, value);
    }

    [Fact]
    public void StringPatternDeclaration_WithFixedSegment_EmitsAndRuns()
    {
        var code = """
class Foo {
    func Run() -> string {
        val text = "rune"
        val [first, ..2 middle, last] = text
        return first.ToString() + ":" + middle + ":" + last.ToString()
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);
        var value = (string)method!.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal("r:un:e", value);
    }
}
