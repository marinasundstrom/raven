using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NullableTypeTests : CompilationTestBase
{
    [Fact]
    public void NullableReferenceAndValueTypes_AreBound()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);
        var nullableString = new NullableTypeSymbol(stringType, null, null, null, []);
        Assert.Equal(TypeKind.Nullable, nullableInt.TypeKind);
        Assert.Equal(TypeKind.Nullable, nullableString.TypeKind);
        Assert.Equal(SpecialType.System_Int32, ((NullableTypeSymbol)nullableInt).UnderlyingType.SpecialType);
        Assert.Equal(SpecialType.System_String, ((NullableTypeSymbol)nullableString).UnderlyingType.SpecialType);
    }

    [Fact]
    public void ReferencedLibrary_NullabilityAnnotations_AreRead()
    {
        var compilation = CreateCompilation();

        compilation.EnsureSetup();
        var consoleType = (INamedTypeSymbol)compilation.GetType(typeof(Console))!;
        var readLine = consoleType.GetMembers("ReadLine").OfType<IMethodSymbol>().First(m => m.Parameters.Length == 0);

        Assert.IsType<NullableTypeSymbol>(readLine.ReturnType);
        var underlying = ((NullableTypeSymbol)readLine.ReturnType).UnderlyingType;
        Assert.Equal(SpecialType.System_String, underlying.SpecialType);
    }

    [Fact]
    public void NullableSyntax_BindsToNullableTypeSymbol()
    {
        var source = """
        let s: string? = null
        let i: int? = null
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();

        var sType = model.GetTypeInfo(declarators[0].TypeAnnotation!.Type).Type;
        var iType = model.GetTypeInfo(declarators[1].TypeAnnotation!.Type).Type;

        var nullableString = Assert.IsType<NullableTypeSymbol>(sType);
        Assert.Equal(SpecialType.System_String, nullableString.UnderlyingType.SpecialType);

        var nullableInt = Assert.IsType<NullableTypeSymbol>(iType);
        Assert.Equal(SpecialType.System_Int32, nullableInt.UnderlyingType.SpecialType);
    }

    [Fact]
    public void NonNullable_To_Nullable_Conversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);
        var nullableString = new NullableTypeSymbol(stringType, null, null, null, []);

        var intConv = compilation.ClassifyConversion(intType, nullableInt);
        Assert.True(intConv.IsImplicit);
        Assert.False(intConv.IsIdentity);

        var stringConv = compilation.ClassifyConversion(stringType, nullableString);
        Assert.True(stringConv.IsImplicit);
        Assert.False(stringConv.IsIdentity);

        var reverse = compilation.ClassifyConversion(nullableString, stringType);
        Assert.False(reverse.IsImplicit);
    }

    [Fact]
    public void OverloadResolution_Prefers_NonNullable_WhenAvailable()
    {
        var source = """
        func f(x: string) -> int { 0 }
        func f2(x: string?) -> int { 1 }
        let s: string = ""
        let n: string? = null
        let a = f(s)
        let b = f2(n)
        let c = f2(null)
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocations = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().ToArray();

        var aSymbol = (IMethodSymbol)model.GetSymbolInfo(invocations[0]).Symbol!;
        Assert.Equal(SpecialType.System_String, aSymbol.Parameters[0].Type.SpecialType);

        var bSymbol = (IMethodSymbol)model.GetSymbolInfo(invocations[1]).Symbol!;
        Assert.IsType<NullableTypeSymbol>(bSymbol.Parameters[0].Type);

        var cSymbol = (IMethodSymbol)model.GetSymbolInfo(invocations[2]).Symbol!;
        Assert.IsType<NullableTypeSymbol>(cSymbol.Parameters[0].Type);
    }

    [Fact]
    public void ConsoleWriteLine_WithStringLiteral_Chooses_StringOverload()
    {
        var (compilation, tree) = CreateCompilation("System.Console.WriteLine(\"Foo\")");
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;

        var param = Assert.IsType<NullableTypeSymbol>(symbol.Parameters[0].Type);
        Assert.Equal(SpecialType.System_String, param.UnderlyingType.SpecialType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void UnionWithNull_ImplicitlyConvertsToNullable()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var union = new UnionTypeSymbol([stringType, compilation.NullTypeSymbol], compilation.Assembly, null, null, []);
        var nullableString = new NullableTypeSymbol(stringType, null, null, null, []);

        var conv = compilation.ClassifyConversion(union, nullableString);
        Assert.True(conv.IsImplicit);
    }

    [Fact]
    public void NullableType_In_Union_ReportsDiagnostic()
    {
        var (compilation, _) = CreateCompilation("let x: string? | int = null");
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.NullableTypeInUnion, diagnostic.Descriptor);
    }
}
