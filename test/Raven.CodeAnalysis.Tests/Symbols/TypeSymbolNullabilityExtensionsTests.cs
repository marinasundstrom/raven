using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class TypeSymbolNullabilityExtensionsTests
{
    [Fact]
    public void GetNonNullableType_IsTotalAndPreservesUnchangedIdentity()
    {
        var compilation = Compilation.Create("nullable_api");
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableString = stringType.WithNullableAnnotation(NullableAnnotation.Annotated);

        Assert.Same(stringType, stringType.GetNonNullableType());
        Assert.Same(stringType, nullableString.GetNonNullableType());
    }

    [Fact]
    public void TryGetNullableUnderlyingType_DistinguishesNullableDecoration()
    {
        var compilation = Compilation.Create("nullable_api");
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = intType.WithNullableAnnotation(NullableAnnotation.Annotated);

        Assert.False(intType.TryGetNullableUnderlyingType(out var absentUnderlying));
        Assert.Null(absentUnderlying);
        Assert.True(nullableInt.TryGetNullableUnderlyingType(out var underlying));
        Assert.Same(intType, underlying);
    }

    [Fact]
    public void WithNullableAnnotation_IsIdempotentAndReversible()
    {
        var compilation = Compilation.Create("nullable_api");
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var nullableString = stringType.WithNullableAnnotation(NullableAnnotation.Annotated);
        var nullableAgain = nullableString.WithNullableAnnotation(NullableAnnotation.Annotated);
        var nonNullableString = nullableString.WithNullableAnnotation(NullableAnnotation.NotAnnotated);
        var nonNullableAgain = stringType.WithNullableAnnotation(NullableAnnotation.NotAnnotated);

        Assert.True(nullableString.IsNullable);
        Assert.Same(nullableString, nullableAgain);
        Assert.Same(stringType, nonNullableString);
        Assert.Same(stringType, nonNullableAgain);
    }

    [Fact]
    public void WithNullableAnnotation_RejectsNoneForConcreteType()
    {
        var compilation = Compilation.Create("nullable_api");
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        Assert.Throws<ArgumentOutOfRangeException>(() =>
            stringType.WithNullableAnnotation(NullableAnnotation.None));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void TypeInfo_KeepsDeclaredAnnotationSeparateFromFlowState(bool diagnosticsFirst)
    {
        const string source = """
            func Length(value: string?) -> int {
                if value is not null {
                    return value.Length
                }

                return 0
            }
            """;

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "nullable_type_info",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(syntaxTree).GetTypeInfo(receiver);

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.True(typeInfo.Type!.IsNullable);
        Assert.False(typeInfo.Type.WithNullableAnnotation(NullableAnnotation.NotAnnotated).IsNullable);
        Assert.True(typeInfo.Type.IsNullable);
    }
}
