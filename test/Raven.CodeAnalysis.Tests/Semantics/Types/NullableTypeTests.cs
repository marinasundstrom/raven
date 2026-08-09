using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NullableTypeTests : CompilationTestBase
{
    [Fact]
    public void NullableReferenceAndValueTypes_AreBound()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var nullableInt = intType.GetNullableType();
        var nullableString = stringType.GetNullableType();

        Assert.Equal(SpecialType.System_Int32, Assert.IsType<NullableTypeSymbol>(nullableInt).UnderlyingType.SpecialType);
        Assert.Equal(SpecialType.System_String, Assert.IsType<NullableTypeSymbol>(nullableString).UnderlyingType.SpecialType);
    }

    [Fact]
    public void ReferencedLibrary_NullabilityAnnotations_AreRead()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();

        var consoleType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetType(typeof(Console)));
        var readLine = consoleType.GetMembers("ReadLine").OfType<IMethodSymbol>().First(m => m.Parameters.Length == 0);
        var returnType = Assert.IsType<NullableTypeSymbol>(readLine.ReturnType);

        Assert.Equal(SpecialType.System_String, returnType.UnderlyingType.SpecialType);
    }

    [Fact]
    public void ReferencedNullableReturnType_CannotImplicitlyConvertToNonNullableReference()
    {
        const string source = """
import System.*

func Main() -> unit {
    let value: string = Console.ReadLine()
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(d => d.Descriptor == CompilerDiagnostics.CannotAssignFromTypeToType));

        Assert.Equal("Cannot assign 'string?' to 'string'", diagnostic.GetMessage());
    }

    [Fact]
    public void NullableSyntax_BindsToNullableTypeSymbols()
    {
        const string source = """
let text: string? = null
let number: int? = null
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();

        var textType = model.GetTypeInfo(declarators[0].TypeAnnotation!.Type).Type;
        var numberType = model.GetTypeInfo(declarators[1].TypeAnnotation!.Type).Type;

        Assert.Equal(SpecialType.System_String, Assert.IsType<NullableTypeSymbol>(textType).UnderlyingType.SpecialType);
        Assert.Equal(SpecialType.System_Int32, Assert.IsType<NullableTypeSymbol>(numberType).UnderlyingType.SpecialType);
    }

    [Theory]
    [InlineData("value.Name")]
    [InlineData("if value is not null { let name = value.Name }")]
    [InlineData("if value is null { } else { let name = value.Name }")]
    [InlineData("if value != null { let name = value.Name }")]
    [InlineData("if value == null { } else { let name = value.Name }")]
    public void NullableValue_IsNeverImplicitlyRefined(string access)
    {
        var source = $$"""
class Person {
    val Name: string = ""
}

func Inspect(value: Person?) -> unit {
    {{access}}
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void GetTypeInfo_RemainsStaticInsideNullCheck()
    {
        const string source = """
class Person {
    val Name: string = ""
}

func Inspect(value: Person?) -> unit {
    if value is not null {
        let name = value.Name
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var type = compilation.GetSemanticModel(tree).GetTypeInfo(receiver).Type;

        Assert.IsType<NullableTypeSymbol>(type);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void IsNotNullCompatibilityOption_NarrowsOnlyInsideTrueBranch(bool diagnosticsFirst)
    {
        const string source = """
class Person {
    val Name: string = ""
}

func Inspect(value: Person?) -> unit {
    if value is not null {
        let name = value.Name
    }

    let original = value
}
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithEnableIsNotNullNarrowing(true);
        var (compilation, tree) = CreateCompilation(source, options);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var model = compilation.GetSemanticModel(tree);
        var identifiers = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(identifier => identifier.Identifier.ValueText == "value")
            .ToArray();
        var narrowedIdentifier = Assert.Single(identifiers.Where(identifier => identifier.Parent is MemberAccessExpressionSyntax));
        var outsideIdentifier = identifiers.Last();
        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetSymbolInfo(narrowedIdentifier).Symbol);

        Assert.False(model.GetTypeInfo(narrowedIdentifier).Type?.IsNullable);
        Assert.True(model.GetTypeInfo(outsideIdentifier).Type?.IsNullable);
        Assert.True(parameter.Type.IsNullable);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void IsNotNullCompatibilityOption_DoesNotNarrowElseBranch()
    {
        const string source = """
class Person {
    val Name: string = ""
}

func Inspect(value: Person?) -> unit {
    if value is not null {
    } else {
        let name = value.Name
    }
}
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithEnableIsNotNullNarrowing(true);
        var (compilation, _) = CreateCompilation(source, options);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void IsNotNullCompatibilityOption_DoesNotNarrowMutableLocal()
    {
        const string source = """
class Person {
    val Name: string = ""
}

func Inspect(input: Person?) -> unit {
    var value: Person? = input
    if value is not null {
        let name = value.Name
    }
}
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithEnableIsNotNullNarrowing(true);
        var (compilation, tree) = CreateCompilation(source, options);
        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.True(compilation.GetSemanticModel(tree).GetTypeInfo(receiver).Type?.IsNullable);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void IsNotNullCompatibilityOption_IncompleteGuardedBody_RemainsQueryable()
    {
        const string source = """
class Person {
    val Name: string = ""
}

func Inspect(value: Person?) -> unit {
    if value is not null {
        value.
    }
}
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithEnableIsNotNullNarrowing(true);
        var (compilation, tree) = CreateCompilation(source, options);
        var model = compilation.GetSemanticModel(tree);
        var guardedValue = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(identifier => identifier.Identifier.ValueText == "value");

        _ = compilation.GetDiagnostics();
        Assert.False(model.GetTypeInfo(guardedValue).Type?.IsNullable);
        _ = compilation.GetDiagnostics();
    }

    [Theory]
    [InlineData("if value is Person person { let name = person.Name }")]
    [InlineData("if let person: Person = value { let name = person.Name }")]
    public void PatternBinding_ProducesSeparateNonNullableValue(string pattern)
    {
        var source = $$"""
class Person {
    val Name: string = ""
}

func Inspect(value: Person?) -> unit {
    {{pattern}}
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;

        Assert.False(compilation.GetSemanticModel(tree).GetTypeInfo(receiver).Type?.IsNullable);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void NullableProperty_CheckDoesNotRefineStorage()
    {
        const string source = """
class Person {
    val Name: string = ""
}

class Container {
    var Current: Person? = null

    func Inspect() -> unit {
        if Current is not null {
            let name = Current.Name
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void TypedConditionalBinding_UnifiesReferenceAndValueNullability()
    {
        const string source = """
func NormalizeText(value: string?) -> string {
    if let text: string = value {
        return text
    }
    return ""
}

func NormalizeNumber(value: int?) -> int {
    if let number: int = value {
        return number
    }
    return 0
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NullableDelegateInvocation_RemainsInvalidAfterNullCheck()
    {
        const string source = """
import System.*

func Run(callback: Action<int>?) -> unit {
    if callback is not null {
        callback(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void NullableConditionalInvocation_IsValid()
    {
        const string source = """
import System.*

func Run(callback: Action<int>?) -> unit {
    callback?(2)
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void ConditionalAccess_ProducesNullableResultWithoutRefiningReceiver()
    {
        const string source = """
func GetLength(value: string?) -> int? {
    return value?.Length
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var conditionalAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<ConditionalAccessExpressionSyntax>()
            .Single();
        var model = compilation.GetSemanticModel(tree);

        Assert.True(model.GetTypeInfo(conditionalAccess.Expression).Type?.IsNullable);
        Assert.True(model.GetTypeInfo(conditionalAccess).Type?.IsNullable);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void ExplicitNullableGenericSyntax_BindsToSystemNullable()
    {
        const string source = """
import System.*

let number: Nullable<int> = null
""";

        var (compilation, tree) = CreateCompilation(source);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetSemanticModel(tree).GetTypeInfo(declarator.TypeAnnotation!.Type).Type);

        Assert.Equal(SpecialType.System_Nullable_T, type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, type.TypeArguments.Single().SpecialType);
    }

    [Theory]
    [InlineData("class Box<T : struct> { val Value: T? = null }", TypeParameterConstraintKind.ValueType)]
    [InlineData("class Box<T : class> { val Value: T? = null }", TypeParameterConstraintKind.ReferenceType)]
    [InlineData("class Box<T> { val Value: T? = null }", TypeParameterConstraintKind.None)]
    public void NullableTypeSyntax_WrapsTypeParameters_WithConstraints(
        string source,
        TypeParameterConstraintKind expectedConstraint)
    {
        var (compilation, tree) = CreateCompilation(source);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var type = compilation.GetSemanticModel(tree).GetTypeInfo(property.Type.Type).Type;
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(
            Assert.IsType<NullableTypeSymbol>(type).UnderlyingType);

        Assert.Equal(expectedConstraint, typeParameter.ConstraintKind);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NonNullableToNullableConversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableString = stringType.GetNullableType();

        Assert.True(compilation.ClassifyConversion(stringType, nullableString).IsImplicit);
        Assert.False(compilation.ClassifyConversion(nullableString, stringType).IsImplicit);
    }

    [Theory]
    [InlineData("string")]
    [InlineData("int")]
    public void NullLiteralConvertsOnlyToNullableType(string typeName)
    {
        var (validCompilation, _) = CreateCompilation($"let value: {typeName}? = null");
        var (invalidCompilation, _) = CreateCompilation($"let value: {typeName} = null");

        Assert.DoesNotContain(
            validCompilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
        Assert.Contains(
            invalidCompilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void NullableShorthandAndExplicitNullableGeneric_AreInteroperable()
    {
        const string source = """
import System.*

let shorthand: int? = 42
let explicitValue: Nullable<int> = shorthand
let roundTrip: int? = explicitValue
let hasValue = explicitValue.HasValue
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NullableNotNullTypeParameter_IsRepresentedExplicitly()
    {
        const string source = """
class Box<T : notnull> {
    val Value: T? = null
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var nullable = Assert.IsType<NullableTypeSymbol>(
            compilation.GetSemanticModel(tree).GetTypeInfo(property.Type.Type).Type);
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(nullable.UnderlyingType);

        Assert.True((typeParameter.ConstraintKind & TypeParameterConstraintKind.NotNull) != 0);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NullableTypeSymbol_LookupType_DoesNotThrow()
    {
        var compilation = CreateCompilation();
        var nullableString = compilation.GetSpecialType(SpecialType.System_String)
            .GetNullableType();

        var exception = Record.Exception(() => nullableString.LookupType("DoesNotExist"));

        Assert.Null(exception);
        Assert.Null(nullableString.LookupType("DoesNotExist"));
        Assert.False(nullableString.IsMemberDefined("DoesNotExist", out _));
    }

    [Theory]
    [InlineData("callback?(2)")]
    [InlineData("callback?.Invoke(2)")]
    public void NullableDelegateConditionalInvocation_IsValid(string invocation)
    {
        var source = $$"""
import System.*

func Run(callback: Action<int>?) -> unit {
    {{invocation}}
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Theory]
    [InlineData("val Handler: ((int) -> unit)? = null", "Handler?(2)")]
    [InlineData("var Handler: ((int) -> unit)? = null", "Handler?(2)")]
    public void NullableFunctionTypeMemberConditionalInvocation_IsValid(
        string declaration,
        string invocation)
    {
        var source = $$"""
class Runner {
    {{declaration}}

    func Run() -> unit {
        {{invocation}}
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        foreach (var expression in tree.GetRoot().DescendantNodes().OfType<ExpressionSyntax>())
            _ = model.GetTypeInfo(expression);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Theory]
    [InlineData("string", "null", true)]
    [InlineData("string?", "null", false)]
    [InlineData("object", "null", true)]
    [InlineData("object?", "null", false)]
    public void PropertyInitializer_EnforcesDeclaredNullability(
        string typeName,
        string initializer,
        bool expectsError)
    {
        var (compilation, _) = CreateCompilation($"class Holder {{ val Value: {typeName} = {initializer} }}");
        var hasError = compilation.GetDiagnostics()
            .Any(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);

        Assert.Equal(expectsError, hasError);
    }

    [Theory]
    [InlineData("string?", "\"value\"")]
    [InlineData("int?", "1")]
    [InlineData("Person?", "Person()")]
    public void NullableEqualityWithUnderlyingValue_IsAllowed(string typeName, string value)
    {
        var source = $$"""
class Person { }

func Compare(left: {{typeName}}) -> bool {
    left == {{value}}
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
