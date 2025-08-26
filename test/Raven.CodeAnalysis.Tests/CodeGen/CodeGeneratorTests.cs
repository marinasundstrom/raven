using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

    public class CodeGeneratorTests
    {
        [Fact]
        public void Emit_ShouldGenerateClass()
        {
        var code = """
class Foo {
    Test() -> void {
        return;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);

        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references = [
                MetadataReference.CreateFromFile(runtimePath)];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success);

        peStream.Seek(0, SeekOrigin.Begin);

        var resolver = new PathAssemblyResolver(references.Select(r => ((PortableExecutableReference)r).FilePath));
        using var mlc = new MetadataLoadContext(resolver);

        var assembly = mlc.LoadFromStream(peStream);

        Assert.NotNull(assembly.GetType("Foo", true));
    }

    [Fact]
    public void DiscardedReturnValue_DoesNotRequireExplicitUnitType()
    {
        var code = """
import System.Text.*

let sb = new StringBuilder()
sb.AppendLine("test")
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);

        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references = [
                MetadataReference.CreateFromFile(runtimePath)];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success);
    }
}
