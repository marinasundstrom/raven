using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SpecialTypeConstraintMetadataTests
{
    [Theory]
    [InlineData("class", GenericParameterAttributes.ReferenceTypeConstraint)]
    [InlineData("struct", GenericParameterAttributes.NotNullableValueTypeConstraint | GenericParameterAttributes.DefaultConstructorConstraint)]
    [InlineData("new()", GenericParameterAttributes.DefaultConstructorConstraint)]
    public void SpecialOwnerConstraintsPreserveClrFlags(string constraint, GenericParameterAttributes expected)
    {
        var tree = SyntaxTree.ParseText($"class Box<T> where T: {constraint} {{ }}");
        var compilation = Compilation.Create("SpecialBounds", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var parameter = Assembly.Load(image.ToArray()).GetType("Box`1")!.GetGenericArguments().Single();
        Assert.Equal(expected, parameter.GenericParameterAttributes & GenericParameterAttributes.SpecialConstraintMask);
    }

}
