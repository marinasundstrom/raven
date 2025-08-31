using System;
using System.IO;
using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceTests
{
    [Fact]
    public void GetCompletions_AfterDot_ReturnsMembers()
    {
        var code = """
import System.*;

Console.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "WriteLine");
    }
}
