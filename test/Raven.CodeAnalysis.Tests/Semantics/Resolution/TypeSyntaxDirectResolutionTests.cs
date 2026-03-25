using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeSyntaxResolutionTests : CompilationTestBase
{
    [Fact]
    public void BindTypeSyntaxAndReport_ResolvesPredefinedType()
    {
        const string source = "val value: int = 0";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxAndReport(typeSyntax);

        Assert.Equal(SpecialType.System_Int32, resolved.SpecialType);
    }

    [Fact]
    public void BindTypeSyntaxAndReport_ResolvesDecimalPredefinedType()
    {
        const string source = "val value: decimal = 1m";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxAndReport(typeSyntax);

        Assert.Equal(SpecialType.System_Decimal, resolved.SpecialType);
    }

    [Fact]
    public void BindTypeSyntaxAndReport_BindsTupleType()
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

        var resolved = binder.BindTypeSyntaxAndReport(typeSyntax);

        Assert.Equal(TypeKind.Tuple, resolved.TypeKind);
        Assert.DoesNotContain(
            binder.Diagnostics.AsEnumerable(),
            d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void BindTypeSyntax_BindsTupleType()
    {
        const string source = """
val pair: (left: int, right: string) = (1, "x")
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindTypeSyntax(typeSyntax);

        Assert.True(result.Success);
        var tupleType = Assert.IsAssignableFrom<ITupleTypeSymbol>(result.ResolvedType);
        Assert.Equal(2, tupleType.TupleElements.Length);
        Assert.Equal("left", tupleType.TupleElements[0].Name);
        Assert.Equal(SpecialType.System_Int32, tupleType.TupleElements[0].Type.SpecialType);
        Assert.Equal("right", tupleType.TupleElements[1].Name);
        Assert.Equal(SpecialType.System_String, tupleType.TupleElements[1].Type.SpecialType);
    }

    [Fact]
    public void BindTypeSyntax_BindsFunctionType()
    {
        const string source = "val callback: (int, string) -> bool = null";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindTypeSyntax(typeSyntax);

        Assert.True(result.Success);
        var functionType = Assert.IsAssignableFrom<INamedTypeSymbol>(result.ResolvedType);
        Assert.Equal(TypeKind.Delegate, functionType.TypeKind);
        var invoke = functionType.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Equal(2, invoke!.Parameters.Length);
        Assert.Equal(SpecialType.System_Int32, invoke.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, invoke.Parameters[1].Type.SpecialType);
        Assert.Equal(SpecialType.System_Boolean, invoke.ReturnType.SpecialType);
    }

    [Fact]
    public void BindTypeSyntax_BindsUnitType()
    {
        const string source = "val value: () = ()";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var result = binder.BindTypeSyntax(typeSyntax);

        Assert.True(result.Success);
        Assert.Equal(SpecialType.System_Unit, result.ResolvedType.SpecialType);
    }

    [Fact]
    public void DeclarationBinding_BindsGenericBaseTypeAndInterfaceViaBindTypeSyntax()
    {
        const string source = """
interface Marker<T> {}
abstract class Base<T> {}
class Derived : Base<int>, Marker<string> {}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(static x => x.Identifier.ValueText == "Derived");
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        var baseType = Assert.IsAssignableFrom<INamedTypeSymbol>(symbol.BaseType);
        Assert.Equal("Base", baseType.Name);
        Assert.Single(baseType.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, baseType.TypeArguments[0].SpecialType);

        var implemented = Assert.Single(symbol.Interfaces);
        Assert.Equal("Marker", implemented.Name);
        Assert.Single(implemented.TypeArguments);
        Assert.Equal(SpecialType.System_String, implemented.TypeArguments[0].SpecialType);
    }

    [Fact]
    public void DeclarationBinding_BindsInterfaceBaseListViaBindTypeSyntax()
    {
        const string source = """
interface Parent<T> {}
interface Child : Parent<int> {}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<InterfaceDeclarationSyntax>()
            .Single(static x => x.Identifier.ValueText == "Child");
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        var implemented = Assert.Single(symbol.Interfaces);
        Assert.Equal("Parent", implemented.Name);
        Assert.Single(implemented.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, implemented.TypeArguments[0].SpecialType);
    }

    [Fact]
    public void SemanticModel_GetTypeInfo_BindsFunctionTypeSyntax()
    {
        const string source = "val callback: (int, string) -> bool = null";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeInfo = model.GetTypeInfo(declarator.TypeAnnotation!.Type);

        var functionType = Assert.IsAssignableFrom<INamedTypeSymbol>(typeInfo.Type);
        Assert.Equal(TypeKind.Delegate, functionType.TypeKind);
        var invoke = functionType.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Equal(SpecialType.System_Boolean, invoke!.ReturnType.SpecialType);
        Assert.Equal(2, invoke.Parameters.Length);
        Assert.Equal(SpecialType.System_Int32, invoke.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, invoke.Parameters[1].Type.SpecialType);
    }

    [Fact]
    public void SemanticModel_GetSymbolInfo_OnTypeSyntax_UsesBindTypeSyntaxPath()
    {
        const string source = """
import System.Collections.Generic.*
val values: List<int> = null
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var symbolInfo = model.GetSymbolInfo(declarator.TypeAnnotation!.Type);

        var named = Assert.IsAssignableFrom<INamedTypeSymbol>(symbolInfo.Symbol);
        Assert.Equal("List", named.Name);
        Assert.Single(named.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, named.TypeArguments[0].SpecialType);
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
    public void BindTypeSyntaxAndReport_WithOptions_AppliesTypeParameterSubstitution()
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

        var resolved = binder.BindTypeSyntaxAndReport(typeSyntax, options);

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
    public void BindTypeSyntaxAndReport_MissingType_ProjectsDiagnostics()
    {
        const string source = "val value: MissingType = 0";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxAndReport(typeSyntax);

        Assert.Equal(TypeKind.Error, resolved.TypeKind);
        Assert.Contains(
            binder.Diagnostics.AsEnumerable(),
            d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
    }

    [Fact]
    public void BindTypeSyntaxAndReport_UnboundGenericIdentifier_ProjectsDiagnostics()
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

        var resolved = binder.BindTypeSyntaxAndReport(typeSyntax);

        Assert.Contains(
            binder.Diagnostics.AsEnumerable(),
            d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);
    }

    [Fact]
    public void BindTypeSyntaxAndReport_BindsTupleTypeViaBindType()
    {
        const string source = "val pair: (int, string) = (1, \"x\")";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(typeSyntax));

        var resolved = binder.BindTypeSyntaxAndReport(typeSyntax);

        Assert.Equal(TypeKind.Tuple, resolved.TypeKind);
        Assert.DoesNotContain(
            binder.Diagnostics.AsEnumerable(),
            d => d.Severity == DiagnosticSeverity.Error);
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
    func M<T>(value: T) -> () {
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
    func M(value: T) -> () {
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
        var declarator = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var typeSyntax = declarator.Type.Type;
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
    func M<T>(value: T) -> () {
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
    func M<T>(value: T) -> () {
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
