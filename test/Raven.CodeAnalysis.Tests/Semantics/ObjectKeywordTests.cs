using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PrimitiveKeywordTests
{
    [Theory]
    [InlineData("bool", SpecialType.System_Boolean)]
    [InlineData("char", SpecialType.System_Char)]
    [InlineData("sbyte", SpecialType.System_SByte)]
    [InlineData("byte", SpecialType.System_Byte)]
    [InlineData("short", SpecialType.System_Int16)]
    [InlineData("ushort", SpecialType.System_UInt16)]
    [InlineData("int", SpecialType.System_Int32)]
    [InlineData("uint", SpecialType.System_UInt32)]
    [InlineData("long", SpecialType.System_Int64)]
    [InlineData("ulong", SpecialType.System_UInt64)]
    [InlineData("nint", SpecialType.System_IntPtr)]
    [InlineData("nuint", SpecialType.System_UIntPtr)]
    [InlineData("float", SpecialType.System_Single)]
    [InlineData("double", SpecialType.System_Double)]
    [InlineData("decimal", SpecialType.System_Decimal)]
    [InlineData("string", SpecialType.System_String)]
    [InlineData("object", SpecialType.System_Object)]
    [InlineData("unit", SpecialType.System_Unit)]
    public void PrimitiveKeyword_BindsToExpectedSpecialType(string keyword, SpecialType expectedSpecialType)
    {
        var source = $$"""
        func f(value: {{keyword}}) { }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var parameter = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var type = model.GetTypeInfo(parameter.TypeAnnotation!.Type).Type!;

        Assert.Equal(expectedSpecialType, type.SpecialType);
    }
}
