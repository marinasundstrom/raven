using System;
using System.IO;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PartialClassCodeGenTests
{
    [Fact]
    public void PartialClassDeclarations_EmitSuccessfully()
    {
        const string source = """
partial class Container {
    val x: int = 0;
};

partial class Container {
    val y: int = 0;
};
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "lib",
                [tree],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
    }

    [Fact]
    public void PartialClassDeclarationsAcrossSyntaxTrees_EmitSuccessfully()
    {
        const string sourceA = """
partial class Container {
    val x: int = 0;
};
""";

        const string sourceB = """
partial class Container {
    val y: int = 0;
};
""";

        var treeA = SyntaxTree.ParseText(sourceA);
        var treeB = SyntaxTree.ParseText(sourceB);
        var compilation = Compilation.Create(
                "lib",
                [treeA, treeB],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
    }
}
