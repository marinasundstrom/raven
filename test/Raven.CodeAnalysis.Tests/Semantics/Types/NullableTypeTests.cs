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
        var nullableInt = intType.MakeNullable();
        var nullableString = stringType.MakeNullable();
        Assert.Equal(TypeKind.Nullable, nullableInt.TypeKind);
        Assert.Equal(TypeKind.Nullable, nullableString.TypeKind);
        Assert.Equal(SpecialType.System_Int32, ((NullableTypeSymbol)nullableInt).UnderlyingType.SpecialType);
        Assert.Equal(SpecialType.System_String, ((NullableTypeSymbol)nullableString).UnderlyingType.SpecialType);
    }

    [Fact]
    public void MetadataNullableDefinition_ReportsNullableSpecialType()
    {
        var compilation = CreateCompilation();
        var nullableDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Nullable`1"));

        Assert.Equal(SpecialType.System_Nullable_T, nullableDefinition.SpecialType);
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
        val s: string? = null
        val i: int? = null
        """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
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
    public void ExplicitNullableGenericSyntax_BindsToNamedNullableType()
    {
        var source = """
        import System.*

        val i: Nullable<int> = null
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var nullableInt = Assert.IsAssignableFrom<INamedTypeSymbol>(type);
        Assert.Equal(SpecialType.System_Nullable_T, nullableInt.SpecialType);
        Assert.Equal(SpecialType.System_Int32, nullableInt.TypeArguments.Single().SpecialType);
    }

    [Fact]
    public void NullableShorthandAndExplicitNullableGeneric_AreInteroperable()
    {
        var source = """
        import System.*

        val a: int? = 1
        val b: Nullable<int> = a
        val c: int? = b
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();

        var aSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(x => x.Identifier.ValueText == "a")));
        var bSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(x => x.Identifier.ValueText == "b")));
        var cSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(x => x.Identifier.ValueText == "c")));

        Assert.False(SymbolEqualityComparer.Default.Equals(aSymbol.Type, bSymbol.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(aSymbol.Type, cSymbol.Type));
    }

    [Fact]
    public void ExplicitNullableGeneric_AllowsNullableMembers()
    {
        var source = """
        import System.*

        val n: Nullable<int> = 1
        val hasValue = n.HasValue
        val value = n.GetValueOrDefault()
        """;

        var (compilation, _) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData("class Box<T : struct> { val value: T? = null }", TypeParameterConstraintKind.ValueType)]
    [InlineData("class Box<T : class> { val value: T? = null }", TypeParameterConstraintKind.ReferenceType)]
    [InlineData("class Box<T> { val value: T? = null }", TypeParameterConstraintKind.None)]
    public void NullableTypeSyntax_WrapsTypeParameters_WithConstraints(string source, TypeParameterConstraintKind expectedConstraint)
    {
        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var nullable = Assert.IsType<NullableTypeSymbol>(type);
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(nullable.UnderlyingType);
        Assert.Equal(expectedConstraint, typeParameter.ConstraintKind);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NullableTypeSyntax_NotNullTypeParameter_IsAllowed()
    {
        var source = "class Box<T : notnull> { val value: T? = null }";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);
    }

    [Fact]
    public void NullableTypeSymbol_LookupType_DoesNotThrow()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullable = intType.MakeNullable();

        var exception = Record.Exception(() => nullable.LookupType("DoesNotExist"));
        Assert.Null(exception);

        Assert.Null(nullable.LookupType("DoesNotExist"));
        Assert.False(nullable.IsMemberDefined("DoesNotExist", out _));
    }

    [Fact]
    public void NullableDelegateInvocation_ReportsError()
    {
        var source = """
import System.*

class Foo {
    Run() -> unit {
        val f: Action<int>? = null
        f(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess &&
                          diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void NullableDelegateConditionalInvocation_SuppressesWarning()
    {
        var source = """
import System.*

class Foo {
    Run() -> unit {
        val f: Action<int>? = null
        f?(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterNullCheck_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    Run() -> unit {
        val f: Action<int>? = null
        if f is not null {
            f(2)
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterIsNotNull_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    Run() -> unit {
        val f: Action<int>? = null
        if f is not null {
            f(2)
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterIsNotNull_WithParens_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    Run() -> unit {
        val f: Action<int>? = null
        if (f is not null) {
            f.Invoke(2)
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterGuardReturn_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    Run() -> unit {
        val f: Action<int>? = null
        if f is null {
            return
        }
        f(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterIsNullGuard_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    Run() -> unit {
        val f: Action<int>? = null
        if f is null {
            return
        }
        f(2)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NullableDelegateInvocation_AfterNotEqualsNullGuard_AllowsAccess()
    {
        var source = """
import System.*

class Foo {
    Run() -> unit {
        val f: Action<int>? = null
        if f != null {
            f(2)
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

    [Fact]
    public void NonNullable_To_Nullable_Conversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableInt = intType.MakeNullable();
        var nullableString = stringType.MakeNullable();

        var intConv = compilation.ClassifyConversion(intType, nullableInt);
        Assert.True(intConv.IsImplicit);
        Assert.False(intConv.IsIdentity);

        var stringConv = compilation.ClassifyConversion(stringType, nullableString);
        Assert.True(stringConv.IsImplicit);
        Assert.True(stringConv.IsIdentity);

        var reverse = compilation.ClassifyConversion(nullableString, stringType);
        Assert.True(reverse.IsImplicit);
    }

    [Fact]
    public void ExplicitNullableGeneric_AndNullableSyntax_UseSameConversionRules()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableSyntaxType = intType.MakeNullable();
        var nullableDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Nullable`1"));
        var explicitNullableType = nullableDefinition.Construct([intType]);

        var fromSyntaxToExplicit = compilation.ClassifyConversion(nullableSyntaxType, explicitNullableType);
        Assert.True(fromSyntaxToExplicit.Exists);
        Assert.True(fromSyntaxToExplicit.IsImplicit);

        var fromExplicitToSyntax = compilation.ClassifyConversion(explicitNullableType, nullableSyntaxType);
        Assert.True(fromExplicitToSyntax.Exists);
        Assert.True(fromExplicitToSyntax.IsImplicit);

        var fromValueToExplicit = compilation.ClassifyConversion(intType, explicitNullableType);
        Assert.True(fromValueToExplicit.Exists);
        Assert.True(fromValueToExplicit.IsImplicit);

        var fromExplicitToValue = compilation.ClassifyConversion(explicitNullableType, intType);
        Assert.True(fromExplicitToValue.Exists);
        Assert.False(fromExplicitToValue.IsImplicit);
    }

    [Fact]
    public void NullLiteral_To_Object_Conversion_IsNotImplicit()
    {
        var compilation = CreateCompilation();
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var nullType = compilation.NullTypeSymbol;

        var conversion = compilation.ClassifyConversion(nullType, objectType);

        Assert.False(conversion.IsImplicit);
    }

    [Fact]
    public void NullLiteral_To_NullableReference_Conversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableString = stringType.MakeNullable();

        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, nullableString);

        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.Exists);
    }

    [Fact]
    public void NullLiteral_To_NullableValue_Conversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = intType.MakeNullable();

        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, nullableInt);

        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.Exists);
    }

    [Fact]
    public void NullLiteral_To_UnionWithNull_Conversion_IsImplicit()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var union = new TypeUnionSymbol([stringType, compilation.NullTypeSymbol], compilation.Assembly, null, null, []);

        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, union);

        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.Exists);
    }

    [Fact]
    public void NullLiteral_To_UnionWithoutNull_Conversion_DoesNotExist()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var union = new TypeUnionSymbol([stringType, intType], compilation.Assembly, null, null, []);

        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, union);

        Assert.False(conversion.Exists);
    }

    [Fact]
    public void ObjectVariable_AssignedNull_RequiresNullable()
    {
        var (compilation, _) = CreateCompilation(
            "val x: object = null",
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(x => x.Descriptor == CompilerDiagnostics.CannotAssignFromTypeToType));
        Assert.Equal("Cannot assign 'null' to 'object'", diagnostic.GetMessage());
    }

    [Fact]
    public void OverloadResolution_Prefers_NonNullable_WhenAvailable()
    {
        var source = """
        func f(x: string) -> int { 0 }
        func f2(x: string?) -> int { 1 }
        val s: string = ""
        val n: string? = null
        val a = f(s)
        val b = f2(n)
        val c = f2(null)
        """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
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
        var (compilation, tree) = CreateCompilation(
            "System.Console.WriteLine(\"Foo\")",
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;

        var param = Assert.IsType<NullableTypeSymbol>(symbol.Parameters[0].Type);
        Assert.Equal(SpecialType.System_String, param.UnderlyingType.SpecialType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ConsoleWriteLine_WithNullLiteral_IsAmbiguous()
    {
        var (compilation, tree) = CreateCompilation(
            "System.Console.WriteLine(null)",
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbolInfo = model.GetSymbolInfo(invocation);

        Assert.Null(symbolInfo.Symbol);
        Assert.Equal(CandidateReason.Ambiguous, symbolInfo.CandidateReason);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
    }

    [Fact]
    public void ConsoleWriteLine_WithNullableLocal_Chooses_StringOverload()
    {
        const string source = """
            val value: string? = null
            System.Console.WriteLine(value)
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;

        var param = Assert.IsType<NullableTypeSymbol>(symbol.Parameters[0].Type);
        Assert.Equal(SpecialType.System_String, param.UnderlyingType.SpecialType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NullableString_EqualityComparisonWithStringLiteral_IsAllowed()
    {
        const string source = """
            val x: string? = null

            if x == "" { }
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var binary = tree.GetRoot().DescendantNodes().OfType<BinaryExpressionSyntax>().Single();
        var typeInfo = model.GetTypeInfo(binary);

        Assert.Equal(SpecialType.System_Boolean, typeInfo.Type?.SpecialType);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

    [Fact]
    public void NullableInt_EqualityComparisonWithIntLiteral_IsAllowed()
    {
        const string source = """
            val x: int? = null

            if x == 1 { }
            if x != 1 { }
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var binaries = tree.GetRoot().DescendantNodes().OfType<BinaryExpressionSyntax>().ToArray();

        Assert.Equal(2, binaries.Length);
        Assert.All(binaries, binary => Assert.Equal(SpecialType.System_Boolean, model.GetTypeInfo(binary).Type?.SpecialType));
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

    [Fact]
    public void NullableUserType_EqualityComparisonWithValue_IsAllowed()
    {
        const string source = """
            class Foo {}

            val x: Foo? = null
            val foo = Foo()

            if x == foo { }
            if x != foo { }
            """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var binaries = tree.GetRoot().DescendantNodes().OfType<BinaryExpressionSyntax>().ToArray();

        Assert.Equal(2, binaries.Length);
        Assert.All(binaries, binary => Assert.Equal(SpecialType.System_Boolean, model.GetTypeInfo(binary).Type?.SpecialType));
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

    [Fact]
    public void UnionWithNull_ImplicitlyConvertsToNullable()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var union = new TypeUnionSymbol([stringType, compilation.NullTypeSymbol], compilation.Assembly, null, null, []);
        var nullableString = stringType.MakeNullable();

        var conv = compilation.ClassifyConversion(union, nullableString);
        Assert.True(conv.IsImplicit);
    }

    [Fact]
    public void NullableType_In_Union_ReportsDiagnostic()
    {
        var (compilation, _) = CreateCompilation(
            "val x: string? | int = null",
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableTypeInUnion);
    }
}
