using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RecordClassSemanticTests : CompilationTestBase
{
    [Fact]
    public void RecordClassPrimaryConstructorCreatesPropertiesAndValueMembers()
    {
        var source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.GetSemanticModel(tree);

        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Person"));

        var nameProperty = Assert.IsAssignableFrom<IPropertySymbol>(
            Assert.Single(person.GetMembers("Name")));
        var ageProperty = Assert.IsAssignableFrom<IPropertySymbol>(
            Assert.Single(person.GetMembers("Age")));

        Assert.Equal(SpecialType.System_String, nameProperty.Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, ageProperty.Type.SpecialType);
        Assert.NotNull(nameProperty.GetMethod);
        Assert.NotNull(ageProperty.GetMethod);
        Assert.NotNull(nameProperty.SetMethod);
        Assert.NotNull(ageProperty.SetMethod);
        Assert.Equal(MethodKind.InitOnly, nameProperty.SetMethod!.MethodKind);
        Assert.Equal(MethodKind.InitOnly, ageProperty.SetMethod!.MethodKind);
        Assert.False(nameProperty.IsMutable);
        Assert.False(ageProperty.IsMutable);
        Assert.Equal("val Name: string { init; }", nameProperty.ToDisplayString());
        Assert.Equal("val Age: int { init; }", ageProperty.ToDisplayString());

        Assert.Contains(
            person.GetMembers("Equals").OfType<IMethodSymbol>(),
            method => method.Parameters.Length == 1 &&
                      SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, person));
        Assert.Contains(
            person.GetMembers("Equals").OfType<IMethodSymbol>(),
            method => method.Parameters.Length == 1 &&
                      method.Parameters[0].Type.SpecialType == SpecialType.System_Object);
        Assert.Contains(
            person.GetMembers("GetHashCode").OfType<IMethodSymbol>(),
            method => method.Parameters.Length == 0 &&
                      method.ReturnType.SpecialType == SpecialType.System_Int32);
        Assert.Contains(
            person.GetMembers("op_Equality").OfType<IMethodSymbol>(),
            method => method.MethodKind == MethodKind.UserDefinedOperator);
        Assert.Contains(
            person.GetMembers("op_Inequality").OfType<IMethodSymbol>(),
            method => method.MethodKind == MethodKind.UserDefinedOperator);

        var deconstruct = Assert.Single(
            person.GetMembers("Deconstruct").OfType<IMethodSymbol>()
                .Where(method => method.Parameters.Length == 2));
        Assert.Equal(SpecialType.System_Unit, deconstruct.ReturnType.SpecialType);
        Assert.All(deconstruct.Parameters, parameter => Assert.Equal(RefKind.Out, parameter.RefKind));

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordPattern_BindsPrimaryConstructorProperties()
    {
        var source = """
            val value: object = new Person("Ada", 42);

            val result = value match {
                Person(val name, val age) => name
                _ => ""
            };

            record class Person(Name: string, Age: int);
            """;

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        var model = compilation.GetSemanticModel(tree);

        var recordPattern = tree.GetRoot().DescendantNodes().OfType<RecordPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(recordPattern));

        Assert.Equal(2, boundPattern.Arguments.Length);
        Assert.Equal("Deconstruct", boundPattern.DeconstructMethod.Name);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void PositionalPattern_UsesDeconstructWhenAvailable()
    {
        var source = """
            val value: Pair = new Pair(1, 2);

            val result = value match {
                (val left, val right) => left
                _ => 0
            };

            record class Pair(Left: int, Right: int);
            """;

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        var model = compilation.GetSemanticModel(tree);

        var tuplePattern = tree.GetRoot().DescendantNodes().OfType<PositionalPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(tuplePattern));

        Assert.Equal(2, boundPattern.Arguments.Length);
        Assert.Equal("Deconstruct", boundPattern.DeconstructMethod.Name);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordClass_WithPrimaryConstructorAndNoBody_Binds()
    {
        var source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.GetSemanticModel(tree);

        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Person"));

        Assert.Equal(2, person.GetMembers().OfType<IPropertySymbol>().Count());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordClass_WithPrimaryConstructorAndBody_Binds()
    {
        var source = """
            record class Person(Name: string, Age: int) {}
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.GetSemanticModel(tree);

        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Person"));

        Assert.Equal(2, person.GetMembers().OfType<IPropertySymbol>().Count());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordDeclaration_WithoutClassKeyword_BindsAsRecordClass()
    {
        var source = """
            record Person(Name: string, Age: int) {}
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.GetSemanticModel(tree);

        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Person"));

        Assert.Equal(TypeKind.Class, person.TypeKind);
        Assert.Equal(2, person.GetMembers().OfType<IPropertySymbol>().Count());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordClass_NamedArgsCamelCase_MatchPascalCaseParams_BindsWithoutErrors()
    {
        // Regression test: Raven convention is PascalCase params but camelCase named argument labels.
        // Named arg "x:" must resolve to parameter "X", "y:" to "Y".
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var (compilation, tree) = CreateCompilation("""
            func CreatePoint() -> Point {
                Point(x: 1, y: 2)
            }
            record class Point(X: int, Y: int)
            """, options: options);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordClass_NonPublicPromotedParameter_ReportsValueSemanticsWarning()
    {
        var source = """
            record class Person(Name: string, private var Secret: int);
            """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NonPublicRecordPrimaryConstructorPropertyExcludedFromValueSemantics);
    }

    [Fact]
    public void RecordClass_PublicPromotedParameters_DoNotReportValueSemanticsWarning()
    {
        var source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NonPublicRecordPrimaryConstructorPropertyExcludedFromValueSemantics);
    }

    [Fact]
    public void RecordClass_PascalCaseNamedArgs_AlsoWorkAsAlternative()
    {
        // PascalCase named args should also work (exact match on parameter names).
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var (compilation, tree) = CreateCompilation("""
            func CreatePoint() -> Point {
                Point(X: 1, Y: 2)
            }
            record class Point(X: int, Y: int)
            """, options: options);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordClass_WithDuTypeInScope_CamelCaseNamedArgsDoNotShadowDuCases()
    {
        // Regression test: Primary constructor parameters with PascalCase names (e.g. Code: ErrorCode)
        // should NOT shadow DU case constructors (e.g. MissingArgument) when used as named args.
        var source = """
            import System.*

            func CreateError(code: ErrorCode) -> CustomError {
                CustomError(message: "test", code: code)
            }

            record class CustomError(
                Message: string,
                Code: ErrorCode
            )

            enum ErrorCode {
                MissingArgument
                InvalidName
            }
            """;

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var (compilation, _) = CreateCompilation(source, options: options);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordClass_SynthesizedMembers_DoNotBindToUnionCasesWithSameName()
    {
        // Regression: when unqualified DU cases (Left/Right) are in scope, synthesized
        // record members must still bind Left/Right to the record's own data members.
        var source = """
            func Inspect(pair: Pair) -> int {
                val text = pair.ToString()
                val hash = pair.GetHashCode()
                val (left, right) = pair
                return left + right + text.Length + hash
            }

            record class Pair(Left: int, Right: int)

            union Either<L, R> {
                Left(value: L)
                Right(value: R)
            }
            """;

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var (compilation, _) = CreateCompilation(source, options: options);
        Assert.Empty(compilation.GetDiagnostics());

        var pair = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Pair"));
        Assert.Contains(pair.GetMembers("ToString").OfType<IMethodSymbol>(), static m => m.Parameters.Length == 0);
        Assert.Contains(pair.GetMembers("GetHashCode").OfType<IMethodSymbol>(), static m => m.Parameters.Length == 0);
        Assert.Contains(pair.GetMembers("Deconstruct").OfType<IMethodSymbol>(), static m => m.Parameters.Length == 2);
    }
}
