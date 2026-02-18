using System.IO;
using System.Linq;
using System.Runtime.Loader;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SealedHierarchyCodeGenTests
{
    [Fact]
    public void SealedHierarchy_EmittedType_IsNotILSealed()
    {
        var source = """
sealed class Expr {}
class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = Compilation.Create(
                "sealed_hierarchy_il_shape",
                [tree],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));

        stream.Position = 0;
        var alc = new AssemblyLoadContext("SealedNotILSealed", isCollectible: true);
        try
        {
            var assembly = alc.LoadFromStream(stream);
            var exprType = assembly.GetType("Expr");
            Assert.NotNull(exprType);
            Assert.False(exprType!.IsSealed);
            Assert.True(exprType.IsAbstract);
        }
        finally
        {
            alc.Unload();
        }
    }

    [Fact]
    public void SealedHierarchy_EmitsClosedHierarchyAttribute()
    {
        var source = """
sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = Compilation.Create(
                "sealed_hierarchy_attr",
                [tree],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));

        stream.Position = 0;
        var alc = new AssemblyLoadContext("SealedClosedHierarchyAttr", isCollectible: true);
        try
        {
            var assembly = alc.LoadFromStream(stream);
            var exprType = assembly.GetType("Expr");
            Assert.NotNull(exprType);

            var attributes = exprType!.GetCustomAttributesData();
            var closedAttr = attributes.FirstOrDefault(a =>
                a.AttributeType.FullName == "System.Runtime.CompilerServices.ClosedHierarchyAttribute");
            Assert.NotNull(closedAttr);
        }
        finally
        {
            alc.Unload();
        }
    }
}
