using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class PropagationCodeGenTests
{
    [Fact]
    public void GenericStructUnion_PropagationMaterializesEmptyCaseCarrier()
    {
        const string code = """
union Option<T> {
    case Some(value: T)
    case None
}

class Harness {
    private static func NoneValue() -> Option<int> {
        return .None
    }

    private static func PropagateNone() -> Option<int> {
        val value = NoneValue()?
        return .Some(value)
    }

    public static func Check() -> bool {
        return PropagateNone() is .None
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(
            "struct-union-propagation",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var harnessType = runtimeAssembly.GetType("Harness", throwOnError: true)!;
        var check = harnessType.GetMethod("Check", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(true, check.Invoke(null, null));
    }
}
