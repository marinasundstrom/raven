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
        var nullableString = stringType.GetNullableType();

        Assert.Same(stringType, stringType.GetNonNullableType());
        Assert.Same(stringType, nullableString.GetNonNullableType());
    }

    [Fact]
    public void TryGetNullableUnderlyingType_DistinguishesNullableDecoration()
    {
        var compilation = Compilation.Create("nullable_api");
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = intType.GetNullableType();

        Assert.False(intType.TryGetNullableUnderlyingType(out var absentUnderlying));
        Assert.Null(absentUnderlying);
        Assert.True(nullableInt.TryGetNullableUnderlyingType(out var underlying));
        Assert.Same(intType, underlying);
    }

    [Fact]
    public void NullableTypeTransforms_AreIdempotentAndReversible()
    {
        var compilation = Compilation.Create("nullable_api");
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var nullableString = stringType.GetNullableType();
        var nullableAgain = nullableString.GetNullableType();
        var nonNullableString = nullableString.GetNonNullableType();
        var nonNullableAgain = stringType.GetNonNullableType();

        Assert.True(nullableString.IsNullable);
        Assert.Same(nullableString, nullableAgain);
        Assert.Same(stringType, nonNullableString);
        Assert.Same(stringType, nonNullableAgain);
    }

    [Fact]
    public void GetNullableAbiProjection_IsTotalAndDistinguishesClrRepresentations()
    {
        var compilation = Compilation.Create("nullable_projection_api")
            .AddReferences(TestMetadataReferences.Default);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        Assert.Equal(NullableAbiProjection.None, intType.GetNullableAbiProjection());
        Assert.Equal(
            NullableAbiProjection.NullableValueType,
            intType.GetNullableType().GetNullableAbiProjection());
        Assert.Equal(
            NullableAbiProjection.AnnotatedUnderlyingType,
            stringType.GetNullableType().GetNullableAbiProjection());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void TypeInfo_ReportsStaticNullableTypeRegardlessOfQueryOrder(bool diagnosticsFirst)
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
            _ = compilation.GetDiagnostics();

        var receiver = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(syntaxTree).GetTypeInfo(receiver);

        Assert.True(typeInfo.Type!.IsNullable);
        Assert.False(typeInfo.Type.GetNonNullableType().IsNullable);
        Assert.True(typeInfo.Type.IsNullable);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }
}
