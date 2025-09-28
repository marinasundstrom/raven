using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ClassifyConversionTests : CompilationTestBase
{
    [Theory]
    [InlineData(SpecialType.System_Int32)]
    [InlineData(SpecialType.System_String)]
    [InlineData(SpecialType.System_Boolean)]
    public void IdentityConversions_AreImplicitAndIdentity(SpecialType specialType)
    {
        var compilation = CreateCompilation();
        var type = compilation.GetSpecialType(specialType);

        var conversion = compilation.ClassifyConversion(type, type);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsIdentity);
        Assert.False(conversion.IsNumeric);
        Assert.False(conversion.IsReference);
        Assert.False(conversion.IsBoxing);
        Assert.False(conversion.IsUnboxing);
        Assert.False(conversion.IsUserDefined);
    }

    [Fact]
    public void LiteralType_ConvertsToUnderlyingType()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var literal = new LiteralTypeSymbol(intType, 42, compilation);

        var conversion = compilation.ClassifyConversion(literal, intType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsIdentity);
    }

    [Fact]
    public void Null_ConvertsToNullableReference()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableString = new NullableTypeSymbol(stringType, null, null, null, []);

        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, nullableString);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
        Assert.False(conversion.IsIdentity);
    }

    [Fact]
    public void Null_ConvertsToUnionContainingNull()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var union = new UnionTypeSymbol([stringType, compilation.NullTypeSymbol], compilation.Assembly, null, null, []);

        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, union);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
    }

    [Fact]
    public void ValueType_LiftsToNullableImplicitly()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);

        var conversion = compilation.ClassifyConversion(intType, nullableInt);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.False(conversion.IsIdentity);
        Assert.False(conversion.IsReference);
        Assert.False(conversion.IsBoxing);
    }

    [Fact]
    public void NullableValueType_ToUnderlying_IsExplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);

        var conversion = compilation.ClassifyConversion(nullableInt, intType);

        Assert.True(conversion.Exists);
        Assert.False(conversion.IsImplicit);
        Assert.True(conversion.IsIdentity);
    }

    [Theory]
    [InlineData(SpecialType.System_Int32, SpecialType.System_Int64)]
    [InlineData(SpecialType.System_Int32, SpecialType.System_Double)]
    [InlineData(SpecialType.System_Single, SpecialType.System_Double)]
    public void ImplicitNumericConversions_AreMarkedNumeric(SpecialType sourceSpecialType, SpecialType destinationSpecialType)
    {
        var compilation = CreateCompilation();
        var source = compilation.GetSpecialType(sourceSpecialType);
        var destination = compilation.GetSpecialType(destinationSpecialType);

        var conversion = compilation.ClassifyConversion(source, destination);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsNumeric);
        Assert.False(conversion.IsIdentity);
    }

    [Theory]
    [InlineData(SpecialType.System_Double, SpecialType.System_Int32)]
    [InlineData(SpecialType.System_Int64, SpecialType.System_Int32)]
    public void ExplicitNumericConversions_AreMarkedNumeric(SpecialType sourceSpecialType, SpecialType destinationSpecialType)
    {
        var compilation = CreateCompilation();
        var source = compilation.GetSpecialType(sourceSpecialType);
        var destination = compilation.GetSpecialType(destinationSpecialType);

        var conversion = compilation.ClassifyConversion(source, destination);

        Assert.True(conversion.Exists);
        Assert.False(conversion.IsImplicit);
        Assert.True(conversion.IsNumeric);
    }

    [Fact]
    public void ReferenceConversion_ToBaseType_IsImplicit()
    {
        var source = """
        open class Animal {}
        class Dog : Animal {}
        """;

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var classes = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().ToArray();
        var animal = (INamedTypeSymbol)model.GetDeclaredSymbol(classes[0])!;
        var dog = (INamedTypeSymbol)model.GetDeclaredSymbol(classes[1])!;

        var conversion = compilation.ClassifyConversion(dog, animal);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
        Assert.False(conversion.IsIdentity);
    }

    [Fact]
    public void ReferenceConversion_ToImplementedInterface_IsImplicit()
    {
        var source = """
import System.*

class Foo : IDisposable {
    init() {}

    Dispose() -> unit {}
}
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var foo = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;
        var disposable = compilation.GetTypeByMetadataName("System.IDisposable")!;

        var conversion = compilation.ClassifyConversion(foo, disposable);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
        Assert.False(conversion.IsIdentity);
    }

    [Fact]
    public void ReferenceConversion_HandlesInterfaceCovariance()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        var listDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        var listOfString = (INamedTypeSymbol)listDefinition.Construct(stringType);
        var enumerableDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var enumerableOfObject = (INamedTypeSymbol)enumerableDefinition.Construct(objectType);

        var conversion = compilation.ClassifyConversion(listOfString, enumerableOfObject);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
    }

    [Fact]
    public void ReferenceConversion_ArrayToGenericIEnumerable_IsImplicit()
    {
        var compilation = CreateCompilation();
        var arrayType = compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_Int32));
        var enumerableDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var enumerableOfInt = (INamedTypeSymbol)enumerableDefinition.Construct(intType);

        var conversion = compilation.ClassifyConversion(arrayType, enumerableOfInt);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
    }

    [Fact]
    public void ReferenceConversion_ArrayToNonGenericIEnumerable_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var arrayType = compilation.CreateArrayTypeSymbol(intType);
        var enumerableType = compilation.GetSpecialType(SpecialType.System_Collections_IEnumerable);

        var conversion = compilation.ClassifyConversion(arrayType, enumerableType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
    }

    [Fact]
    public void ReferenceConversion_SourceInterfaceVariance_IsImplicit()
    {
        var source = """
interface Producer<out T> {}

class Widget : Producer<string> {}
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var interfaceSyntax = tree.GetRoot().DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var classSyntax = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var producerDefinition = (INamedTypeSymbol)model.GetDeclaredSymbol(interfaceSyntax)!;
        var widget = (INamedTypeSymbol)model.GetDeclaredSymbol(classSyntax)!;
        var producerOfObject = (INamedTypeSymbol)producerDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Object));

        var conversion = compilation.ClassifyConversion(widget, producerOfObject);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
    }

    [Fact]
    public void ReferenceConversion_HandlesInterfaceContravariance()
    {
        var source = """
import System.Collections.Generic

class Comparer : IComparer<object>
{
    init() {}

    Compare(x: object?, y: object?) -> int => 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var comparerDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var comparer = (INamedTypeSymbol)model.GetDeclaredSymbol(comparerDeclaration)!;
        var comparerDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IComparer`1")!;
        var comparerOfString = (INamedTypeSymbol)comparerDefinition.Construct(compilation.GetSpecialType(SpecialType.System_String));

        var conversion = compilation.ClassifyConversion(comparer, comparerOfString);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
    }

    [Fact]
    public void BoxingConversion_ValueTypeToObject_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        var conversion = compilation.ClassifyConversion(intType, objectType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsBoxing);
        Assert.False(conversion.IsReference);
        Assert.False(conversion.IsIdentity);
    }

    [Fact]
    public void UnboxingConversion_ObjectToValueType_IsExplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        var conversion = compilation.ClassifyConversion(objectType, intType);

        Assert.True(conversion.Exists);
        Assert.False(conversion.IsImplicit);
        Assert.True(conversion.IsUnboxing);
    }

    [Fact]
    public void UnionConversion_ValueTypeBranch_Boxes()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var union = new UnionTypeSymbol([intType, stringType], compilation.Assembly, null, null, []);

        var conversion = compilation.ClassifyConversion(intType, union);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsBoxing);
        Assert.False(conversion.IsReference);
    }

    [Fact]
    public void UnionConversion_ReferenceBranch_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var union = new UnionTypeSymbol([intType, stringType], compilation.Assembly, null, null, []);

        var conversion = compilation.ClassifyConversion(stringType, union);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.False(conversion.IsBoxing);
    }

    [Fact]
    public void UnionOfLiteralInts_ConvertsToInt()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var fortyTwo = new LiteralTypeSymbol(intType, 42, compilation);
        var thirteen = new LiteralTypeSymbol(intType, 13, compilation);
        var union = new UnionTypeSymbol(new[] { fortyTwo, thirteen }, compilation.Assembly, null, null, []);

        var conversion = compilation.ClassifyConversion(union, intType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsUnboxing);
        Assert.False(conversion.IsReference);
    }
}
