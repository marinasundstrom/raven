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
    public void RecordDeconstruct_HasSynthesizedBody()
    {
        const string source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var deconstruct = person.GetMembers("Deconstruct").OfType<IMethodSymbol>()
            .Single(method => method.Parameters.Length == 2);

        Assert.True(compilation.TryGetSynthesizedMethodBody(deconstruct, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        Assert.Collection(
            body!.Statements,
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var byRefAssignment = Assert.IsType<BoundByRefAssignmentExpression>(assignment.Expression);
                var parameterAccess = Assert.IsType<BoundParameterAccess>(byRefAssignment.Reference);
                Assert.Equal("Name", parameterAccess.Parameter.Name);
            },
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var byRefAssignment = Assert.IsType<BoundByRefAssignmentExpression>(assignment.Expression);
                var parameterAccess = Assert.IsType<BoundParameterAccess>(byRefAssignment.Reference);
                Assert.Equal("Age", parameterAccess.Parameter.Name);
            },
            statement => Assert.IsType<BoundReturnStatement>(statement));
    }

    [Fact]
    public void RecordToString_HasSynthesizedBody()
    {
        const string source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var toString = person.GetMembers(nameof(object.ToString)).OfType<IMethodSymbol>()
            .Single(method => method.Parameters.Length == 0);

        Assert.True(compilation.TryGetSynthesizedMethodBody(toString, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        var returnStatement = Assert.IsType<BoundReturnStatement>(Assert.Single(body!.Statements));
        Assert.NotNull(returnStatement.Expression);
    }

    [Fact]
    public void GenericRecordToString_HasSynthesizedBody()
    {
        const string source = """
            record class Box<T>(Value: T);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var box = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var toString = box.GetMembers(nameof(object.ToString)).OfType<IMethodSymbol>()
            .Single(method => method.Parameters.Length == 0);

        Assert.True(compilation.TryGetSynthesizedMethodBody(toString, BoundTreeView.Original, out var body));
        Assert.NotNull(body);
        Assert.Contains(body!.Statements, static statement => statement is BoundLocalDeclarationStatement);
    }

    [Fact]
    public void RecordCopyConstructor_HasSynthesizedBody()
    {
        const string source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var copyConstructor = person.GetMembers(".ctor").OfType<IMethodSymbol>()
            .Single(method => method.Parameters.Length == 1 &&
                              SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, person));

        Assert.True(compilation.TryGetSynthesizedMethodBody(copyConstructor, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        Assert.Collection(
            body!.Statements,
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var fieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(assignment.Expression);
                Assert.Equal("<Name>k__BackingField", fieldAssignment.Field.Name);
            },
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var fieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(assignment.Expression);
                Assert.Equal("<Age>k__BackingField", fieldAssignment.Field.Name);
            },
            statement => Assert.IsType<BoundReturnStatement>(statement));
    }

    [Fact]
    public void DerivedRecordCopyConstructor_HasSynthesizedBody()
    {
        const string source = """
            abstract record class Person(Name: string);
            record class Employee(Name: string, Id: int) : Person(Name);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Last();
        var employee = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var copyConstructor = employee.GetMembers(".ctor").OfType<IMethodSymbol>()
            .Single(method => method.Parameters.Length == 1 &&
                              SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, employee));

        Assert.True(compilation.TryGetSynthesizedMethodBody(copyConstructor, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        Assert.Collection(
            body!.Statements,
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var fieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(assignment.Expression);
                Assert.Equal("<Id>k__BackingField", fieldAssignment.Field.Name);
            },
            statement => Assert.IsType<BoundReturnStatement>(statement));
    }

    [Fact]
    public void RecordGetHashCode_HasSynthesizedBody()
    {
        const string source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var getHashCode = person.GetMembers(nameof(object.GetHashCode)).OfType<IMethodSymbol>()
            .Single(method => method.Parameters.Length == 0);

        Assert.True(compilation.TryGetSynthesizedMethodBody(getHashCode, BoundTreeView.Original, out var body));
        Assert.NotNull(body);
        Assert.Contains(body!.Statements, static statement => statement is BoundLocalDeclarationStatement);
        Assert.IsType<BoundReturnStatement>(body.Statements.Last());
    }

    [Fact]
    public void RecordObjectEquals_HasSynthesizedBody()
    {
        const string source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var objectEquals = person.GetMembers(nameof(object.Equals)).OfType<IMethodSymbol>()
            .Single(method => method.Parameters.Length == 1 &&
                              method.Parameters[0].Type.SpecialType == SpecialType.System_Object);

        Assert.True(compilation.TryGetSynthesizedMethodBody(objectEquals, BoundTreeView.Original, out var objectBody));
        Assert.NotNull(objectBody);
        Assert.Contains(objectBody!.Statements, static statement => statement is BoundLocalDeclarationStatement);
        Assert.Contains(objectBody.Statements, static statement => statement is BoundIfStatement);
    }

    [Fact]
    public void RecordTypedEquals_HasSynthesizedBody()
    {
        const string source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var typedEquals = person.GetMembers(nameof(object.Equals)).OfType<IMethodSymbol>()
            .Single(method => method.Parameters.Length == 1 &&
                              SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, person));

        Assert.True(compilation.TryGetSynthesizedMethodBody(typedEquals, BoundTreeView.Original, out var typedBody));
        Assert.NotNull(typedBody);
        Assert.Contains(typedBody!.Statements, static statement => statement is BoundIfStatement);
    }

    [Fact]
    public void RecordEqualityOperator_HasSynthesizedBody()
    {
        const string source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var equalityOperator = person.GetMembers("op_Equality").OfType<IMethodSymbol>().Single();

        Assert.True(compilation.TryGetSynthesizedMethodBody(equalityOperator, BoundTreeView.Original, out var body));
        Assert.NotNull(body);
        Assert.Contains(body!.Statements, static statement => statement is BoundIfStatement);
        Assert.IsType<BoundReturnStatement>(body.Statements.Last());
    }

    [Fact]
    public void RecordInequalityOperator_HasSynthesizedBody()
    {
        const string source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var recordDeclaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDeclaration));
        var inequalityOperator = person.GetMembers("op_Inequality").OfType<IMethodSymbol>().Single();

        Assert.True(compilation.TryGetSynthesizedMethodBody(inequalityOperator, BoundTreeView.Original, out var body));
        Assert.NotNull(body);
        Assert.Contains(body!.Statements, static statement => statement is BoundIfStatement);
        Assert.IsType<BoundReturnStatement>(body.Statements.Last());
    }

    [Fact]
    public void NominalDeconstructionPattern_BindsPrimaryConstructorProperties()
    {
        var source = """
            val value: object = Person("Ada", 42);

            val result = value match {
                Person(val name, val age) => name
                _ => ""
            };

            record class Person(Name: string, Age: int);
            """;

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        var model = compilation.GetSemanticModel(tree);

        var recordPattern = tree.GetRoot().DescendantNodes().OfType<NominalDeconstructionPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(recordPattern));

        Assert.Equal(2, boundPattern.Arguments.Length);
        Assert.Equal("Deconstruct", boundPattern.DeconstructMethod.Name);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NominalDeconstructionPattern_AllowsNullableObjectInput()
    {
        var source = """
            func Test(value: object?) {
                if value is Person(val name, val age) {
                }
            }

            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var recordPattern = tree.GetRoot().DescendantNodes().OfType<NominalDeconstructionPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(recordPattern));

        Assert.Equal("Person", boundPattern.ReceiverType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Id == CompilerDiagnostics.NominalDeconstructionPatternTypeMismatch.Id);
    }

    [Fact]
    public void PropertyPattern_AllowsNullableObjectInput()
    {
        var source = """
            func Test(candidate: object?) {
                if candidate is Shipment { IsPriority: true } {
                }
            }

            class Shipment {
                var IsPriority: bool

                init(isPriority: bool) {
                    IsPriority = isPriority
                }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var propertyPattern = tree.GetRoot().DescendantNodes().OfType<PropertyPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundPropertyPattern>(model.GetBoundNode(propertyPattern));

        Assert.Equal("Shipment", boundPattern.ReceiverType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Id == CompilerDiagnostics.PropertyPatternTypeMismatch.Id);
    }

    [Fact]
    public void PositionalPattern_UsesDeconstructWhenAvailable()
    {
        var source = """
            val value: Pair = Pair(1, 2);

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
    public void PositionalPattern_WithNamedElements_BindsDeconstructArgumentsByName()
    {
        var source = """
            val value: Person = Person("Ada", 42, ["tea"]);

            val result = value match {
                (Age: 42, Items: val items, Name: val name) => name + items[0]
                _ => ""
            };

            record class Person(Name: string, Age: int, Items: string[]);
            """;

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        var model = compilation.GetSemanticModel(tree);

        var positionalPattern = tree.GetRoot().DescendantNodes().OfType<PositionalPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(positionalPattern));

        Assert.Equal(3, boundPattern.Arguments.Length);
        Assert.IsType<BoundDeclarationPattern>(boundPattern.Arguments[0]);
        Assert.IsType<BoundConstantPattern>(boundPattern.Arguments[1]);
        Assert.IsType<BoundDeclarationPattern>(boundPattern.Arguments[2]);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NominalDeconstructionPattern_WithNamedArguments_BindsDeconstructArgumentsByName()
    {
        var source = """
            val value: object = Person("Ada", 42, ["tea"]);

            val result = value match {
                Person(Items: val items, Name: val name, Age: 42) => name + items[0]
                _ => ""
            };

            record class Person(Name: string, Age: int, Items: string[]);
            """;

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        var model = compilation.GetSemanticModel(tree);

        var nominalPattern = tree.GetRoot().DescendantNodes().OfType<NominalDeconstructionPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(nominalPattern));

        Assert.Equal(3, boundPattern.Arguments.Length);
        Assert.IsType<BoundDeclarationPattern>(boundPattern.Arguments[0]);
        Assert.IsType<BoundConstantPattern>(boundPattern.Arguments[1]);
        Assert.IsType<BoundDeclarationPattern>(boundPattern.Arguments[2]);
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
                case Left(value: L)
                case Right(value: R)
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
