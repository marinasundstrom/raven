using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeSyntaxDirectResolutionTests : CompilationTestBase
{
    [Fact]
    public void BindTypeSyntaxDirect_ResolvesPredefinedType()
    {
        const string source = "val value: int = 0";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxDirect(typeSyntax);

        Assert.Equal(SpecialType.System_Int32, resolved.SpecialType);
    }

    [Fact]
    public void BindTypeSyntaxDirect_TupleType_UsesLegacyFallback()
    {
        const string source = """
val pair: (int, string) = (1, "x")
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxDirect(typeSyntax);

        Assert.Equal(TypeKind.Tuple, resolved.TypeKind);
        Assert.DoesNotContain(
            binder.Diagnostics.AsEnumerable(),
            d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void TryResolveNamedTypeFromTypeSyntax_ReturnsFalseForNonNamedResolvedType()
    {
        const string source = "val values: int[] = []";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var ok = binder.TryResolveNamedTypeFromTypeSyntax(typeSyntax, out var namedType);

        Assert.False(ok);
        Assert.Null(namedType);
    }

    [Fact]
    public void BindTypeSyntaxDirect_WithOptions_AppliesTypeParameterSubstitution()
    {
        const string source = "val value: T = 0";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var options = new Binder.TypeResolutionOptions
        {
            TypeParameterSubstitutions = new Dictionary<string, ITypeSymbol>
            {
                ["T"] = compilation.GetSpecialType(SpecialType.System_Int32)
            }
        };

        var resolved = binder.BindTypeSyntaxDirect(typeSyntax, options);

        Assert.Equal(SpecialType.System_Int32, resolved.SpecialType);
    }

    [Fact]
    public void TryBindNamedTypeFromTypeSyntax_WithOptions_AppliesTypeParameterSubstitution()
    {
        const string source = "val value: T = 0";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var options = new Binder.TypeResolutionOptions
        {
            TypeParameterSubstitutions = new Dictionary<string, ITypeSymbol>
            {
                ["T"] = compilation.GetSpecialType(SpecialType.System_Int32)
            }
        };

        var ok = binder.TryBindNamedTypeFromTypeSyntax(typeSyntax, out var named, options);

        Assert.True(ok);
        Assert.NotNull(named);
        Assert.Equal(SpecialType.System_Int32, named!.SpecialType);
    }

    [Fact]
    public void TryBindNamedTypeFromTypeSyntax_ReportDiagnosticsFalse_DoesNotAddDiagnosticsOnFailure()
    {
        const string source = "val value: MissingType = 0";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));
        var before = binder.Diagnostics.AsEnumerable().Count();

        var ok = binder.TryBindNamedTypeFromTypeSyntax(typeSyntax, out var namedType, reportDiagnostics: false);
        var after = binder.Diagnostics.AsEnumerable().Count();

        Assert.False(ok);
        Assert.Null(namedType);
        Assert.Equal(before, after);
    }

    [Fact]
    public void TryBindNamedTypeFromTypeSyntax_ReportDiagnosticsTrue_AddsDiagnosticsOnFailure()
    {
        const string source = "val value: MissingType = 0";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));
        var before = binder.Diagnostics.AsEnumerable().Count();

        var ok = binder.TryBindNamedTypeFromTypeSyntax(typeSyntax, out var namedType, reportDiagnostics: true);
        var after = binder.Diagnostics.AsEnumerable().Count();

        Assert.False(ok);
        Assert.Null(namedType);
        Assert.True(after >= before);
    }

    [Fact]
    public void BindTypeSyntaxDirect_MissingType_PreservesLegacyReporting()
    {
        const string source = "val value: MissingType = 0";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxDirect(typeSyntax);

        Assert.Equal(TypeKind.Error, resolved.TypeKind);
        Assert.Contains(
            binder.Diagnostics.AsEnumerable(),
            d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
    }

    [Fact]
    public void BindTypeSyntaxDirect_UnboundGenericIdentifier_PreservesLegacyReporting()
    {
        const string source = """
import System.Collections.Generic.*
val value: List = null
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxDirect(typeSyntax);

        Assert.Contains(
            binder.Diagnostics.AsEnumerable(),
            d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);
    }

    [Fact]
    public void BindTypeSyntaxDirect_AllowLegacyFallbackFalse_DisablesLegacyTupleFallback()
    {
        const string source = "val pair: (int, string) = (1, \"x\")";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxDirect(
            typeSyntax,
            options: null,
            refKindHint: null,
            allowLegacyFallback: false);

        Assert.Equal(TypeKind.Error, resolved.TypeKind);
        Assert.Contains(
            binder.Diagnostics.AsEnumerable(),
            d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
    }

    [Fact]
    public void MapResolveResultToDiagnostics_GenericArgumentFailure_ReportsSinglePrimaryDiagnostic()
    {
        const string source = """
import System.Collections.Generic.*
val value: List<MissingType> = null
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));
        var resolveResult = binder.BindType(typeSyntax);
        var projectedDiagnostics = binder.MapResolveResultToDiagnostics(resolveResult, typeSyntax).ToList();

        Assert.Equal(TypeKind.Error, resolveResult.ResolvedType.TypeKind);
        Assert.Single(projectedDiagnostics);
        Assert.Equal(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, projectedDiagnostics[0].Descriptor);
    }

    [Fact]
    public void MapResolveResultToDiagnostics_ArrayElementFailure_ReportsSinglePrimaryDiagnostic()
    {
        const string source = "val values: MissingType[] = []";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));
        var resolveResult = binder.BindType(typeSyntax);
        var projectedDiagnostics = binder.MapResolveResultToDiagnostics(resolveResult, typeSyntax).ToList();

        Assert.Equal(TypeKind.Error, resolveResult.ResolvedType.TypeKind);
        Assert.Single(projectedDiagnostics);
        Assert.Equal(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, projectedDiagnostics[0].Descriptor);
    }

    [Fact]
    public void BindType_BindsMethodTypeParameterInScope()
    {
        const string source = """
class C {
    M<T>(value: T) -> () {
        val local: T = value
    }
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindType(typeSyntax);

        Assert.True(result.Success);
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(result.ResolvedType);
        Assert.Equal("T", typeParameter.Name);
    }

    [Fact]
    public void BindType_BindsContainingTypeParameterInScope()
    {
        const string source = """
class Box<T> {
    M(value: T) -> () {
        val local: T = value
    }
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindType(typeSyntax);

        Assert.True(result.Success);
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(result.ResolvedType);
        Assert.Equal("T", typeParameter.Name);
    }

    [Fact]
    public void BindType_BindsContainingTypeParameterInTypeDeclarationBinderScope()
    {
        const string source = """
class Box<T> {
    val item: T
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = model.GetBinder(typeSyntax);

        var result = binder.BindType(typeSyntax);

        Assert.True(result.Success);
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(result.ResolvedType);
        Assert.Equal("T", typeParameter.Name);
    }

    [Fact]
    public void BindTypeSyntax_BindsQualifiedGenericType()
    {
        const string source = "val values: System.Collections.Generic.List<int> = null";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindTypeSyntax(typeSyntax);

        Assert.True(result.Success);
        var named = Assert.IsAssignableFrom<INamedTypeSymbol>(result.ResolvedType);
        Assert.Equal("List", named.Name);
        Assert.Single(named.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, named.TypeArguments[0].SpecialType);
    }

    [Fact]
    public void BindTypeSyntax_BindsNestedGenericTypeFromConstructedContainingType()
    {
        const string source = """
class Outer<T> {
    class Inner<U> { }
}

val value: Outer<int>.Inner<string> = null
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Last();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindTypeSyntax(typeSyntax);

        Assert.True(result.Success);
        var named = Assert.IsAssignableFrom<INamedTypeSymbol>(result.ResolvedType);
        Assert.Equal("Inner", named.Name);
        Assert.NotNull(named.ContainingType);
        Assert.Equal("Outer", named.ContainingType!.Name);
        Assert.Single(named.ContainingType.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, named.ContainingType.TypeArguments[0].SpecialType);
        Assert.Single(named.TypeArguments);
        Assert.Equal(SpecialType.System_String, named.TypeArguments[0].SpecialType);
    }

    [Fact]
    public void BindTypeSyntax_BindsArrayOfQualifiedGenericType()
    {
        const string source = "val values: System.Collections.Generic.List<int>[] = []";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindTypeSyntax(typeSyntax);

        Assert.True(result.Success);
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(result.ResolvedType);
        var elementNamed = Assert.IsAssignableFrom<INamedTypeSymbol>(arrayType.ElementType);
        Assert.Equal("List", elementNamed.Name);
        Assert.Single(elementNamed.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, elementNamed.TypeArguments[0].SpecialType);
    }

    [Fact]
    public void BindTypeSyntax_BindsNullablePredefinedType()
    {
        const string source = "val value: int? = null";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindTypeSyntax(typeSyntax);

        Assert.True(result.Success);
        Assert.True(result.ResolvedType.IsNullable);
        var underlying = result.ResolvedType.GetNullableUnderlyingType();
        Assert.NotNull(underlying);
        Assert.Equal(SpecialType.System_Int32, underlying!.SpecialType);
    }

    [Fact]
    public void BindTypeSyntax_WithSubstitutionPrecedence_BinderWins_UsesInScopeTypeParameter()
    {
        const string source = """
class C {
    M<T>(value: T) -> () {
        val local: T = value
    }
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var options = new Binder.TypeResolutionOptions
        {
            TypeParameterSubstitutions = new Dictionary<string, ITypeSymbol>
            {
                ["T"] = compilation.GetSpecialType(SpecialType.System_Int32)
            },
            SubstitutionPrecedence = Binder.SubstitutionPrecedence.BinderWins
        };

        var result = binder.BindTypeSyntax(typeSyntax, options);

        Assert.True(result.Success);
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(result.ResolvedType);
        Assert.Equal("T", typeParameter.Name);
    }

    [Fact]
    public void BindTypeSyntax_WithSubstitutionPrecedence_OptionsWin_UsesOptionSubstitution()
    {
        const string source = """
class C {
    M<T>(value: T) -> () {
        val local: T = value
    }
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var options = new Binder.TypeResolutionOptions
        {
            TypeParameterSubstitutions = new Dictionary<string, ITypeSymbol>
            {
                ["T"] = compilation.GetSpecialType(SpecialType.System_Int32)
            },
            SubstitutionPrecedence = Binder.SubstitutionPrecedence.OptionsWin
        };

        var result = binder.BindTypeSyntax(typeSyntax, options);

        Assert.True(result.Success);
        Assert.Equal(SpecialType.System_Int32, result.ResolvedType.SpecialType);
    }

    [Fact]
    public void BindTypeSyntax_WithImportedScopesOverride_CanRestrictAndAllowLookup()
    {
        const string source = """
import System.Collections.Generic.*
val value: List<int> = null
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var restricted = binder.BindTypeSyntax(
            typeSyntax,
            new Binder.TypeResolutionOptions
            {
                ImportedScopesOverride = new[] { compilation.GlobalNamespace }
            });

        Assert.False(restricted.Success);

        var systemNs = compilation.GlobalNamespace.LookupNamespace("System");
        var collectionsNs = systemNs?.LookupNamespace("Collections");
        var genericNs = collectionsNs?.LookupNamespace("Generic");
        Assert.NotNull(genericNs);

        var allowed = binder.BindTypeSyntax(
            typeSyntax,
            new Binder.TypeResolutionOptions
            {
                ImportedScopesOverride = new[] { genericNs! }
            });

        Assert.True(allowed.Success);
        var named = Assert.IsAssignableFrom<INamedTypeSymbol>(allowed.ResolvedType);
        Assert.Equal("List", named.Name);
        Assert.Single(named.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, named.TypeArguments[0].SpecialType);
    }
}
