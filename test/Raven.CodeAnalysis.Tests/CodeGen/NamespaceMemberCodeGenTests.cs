using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class NamespaceMemberCodeGenTests
{
    [Fact]
    public void TopLevelFunctionAndConst_EmitIntoNamespaceContainer()
    {
        const string source = """
namespace Utilities {
    public const Answer: int = 41

    public func AddOne(value: int) -> int => value + 1
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "namespaceMembers",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, compilation.References);
        var container = loaded.Assembly.GetType("Utilities.NamespaceMembers", throwOnError: true)!;
        Assert.True(container.IsAbstract);
        Assert.True(container.IsSealed);
        Assert.Contains(
            container.GetCustomAttributesData(),
            attribute => attribute.AttributeType.FullName == "System.Runtime.CompilerServices.TopLevelAttribute");

        var method = container.GetMethod("AddOne", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(method);
        Assert.Equal(42, method!.Invoke(null, [41]));

        var field = container.GetField("Answer", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(field);
        Assert.True(field!.IsLiteral);
        Assert.Equal(41, field.GetRawConstantValue());
    }
}
