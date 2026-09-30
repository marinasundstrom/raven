using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ConstrainedSealedHierarchyTests : CompilationTestBase
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void FirstUseDoesNotCaptureCallersTypeParameterInDeclarationConstraint(bool functionFirst)
    {
        const string declarations = """
            interface IConstraint<T> { }
            class Box<T> where T: IConstraint<T> { }
            class Container<T> where T: IConstraint<T> {
                var Value: Box<T>? = null
            }
            """;
        const string function = """
            func Accept<T>(value: Box<T>) where T: IConstraint<T> { }
            """;
        var tree = SyntaxTree.ParseText(functionFirst ? function + declarations : declarations + function);
        var compilation = Compilation.Create("ConstraintScope", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));

        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var box = compilation.GetTypeByMetadataName("Box`1")!;
        var parameter = Assert.Single(box.TypeParameters);
        var constraint = Assert.IsAssignableFrom<INamedTypeSymbol>(Assert.Single(parameter.ConstraintTypes));
        Assert.Same(parameter, Assert.Single(constraint.TypeArguments));
    }

    [Fact]
    public void UnconstrainedCallerStillFailsDeclarationConstraint()
    {
        var tree = SyntaxTree.ParseText("""
            func Accept<T>(value: Box<T>) { }
            interface IConstraint<T> { }
            class Box<T> where T: IConstraint<T> { }
            """);
        var compilation = Compilation.Create("InvalidConstraintScope", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));

        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0320");
        var box = compilation.GetTypeByMetadataName("Box`1")!;
        var parameter = Assert.Single(box.TypeParameters);
        var constraint = Assert.IsAssignableFrom<INamedTypeSymbol>(Assert.Single(parameter.ConstraintTypes));
        Assert.Same(parameter, Assert.Single(constraint.TypeArguments));
    }

    [Fact]
    public void NestedGenericSealedCases_UseOwnTypeParameterInBaseInterface()
    {
        const string code = """
        import System.Numerics.*

        sealed interface Expr<T>
            where T: INumber<T> {
            record Literal<T>(Value: T) : Expr<T>
                where T: INumber<T>

            record Add<T>(Left: Expr<T>, Right: Expr<T>) : Expr<T>
                where T: INumber<T>
        }
        """;

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "sealed_hierarchy_constraints_debug",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarations = root.DescendantNodes().OfType<TypeDeclarationSyntax>().ToArray();
        var exprDeclaration = declarations.Single(t => t.Identifier.ValueText == "Expr");
        var addDeclaration = declarations.Single(t => t.Identifier.ValueText == "Add");

        var exprSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(exprDeclaration));
        var addSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(addDeclaration));

        Assert.Single(exprSymbol.TypeParameters);
        Assert.Single(addSymbol.TypeParameters);

        var addTypeParameter = addSymbol.TypeParameters[0];
        Assert.NotEmpty(addTypeParameter.ConstraintTypes);

        var addConstraint = Assert.IsAssignableFrom<INamedTypeSymbol>(addTypeParameter.ConstraintTypes.Single());
        Assert.Equal("INumber", addConstraint.Name);
        Assert.Single(addConstraint.TypeArguments);
        Assert.Same(addTypeParameter, addConstraint.TypeArguments[0]);

        var exprInterface = Assert.Single(addSymbol.Interfaces.Where(i => i.Name == "Expr"));
        Assert.Equal("Expr", exprInterface.Name);
        Assert.Single(exprInterface.TypeArguments);
        Assert.Same(addTypeParameter, exprInterface.TypeArguments[0]);
    }

    [Fact]
    public void NestedGenericSealedCases_ImplementInterfaceMethodsAndBindGenericMath()
    {
        const string code = """
import System.Numerics.*

func Evaluate<T>(expr: Expr<T>) -> T
    where T: INumber<T> {
    return expr.Apply()
}

sealed interface Expr<T>
    where T: INumber<T> {
    func Apply() -> T

    record Literal<T>(Value: T) : Expr<T>
        where T: INumber<T> {
        func Apply() -> T {
            return Value
        }
    }

    record Add<T>(Left: Expr<T>, Right: Expr<T>) : Expr<T>
        where T: INumber<T> {
        func Apply() -> T {
            return Left.Apply() + Right.Apply()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "sealed_hierarchy_interface_contracts",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarations = root.DescendantNodes().OfType<TypeDeclarationSyntax>().ToArray();
        var addDeclaration = declarations.Single(t => t.Identifier.ValueText == "Add");
        var addSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(addDeclaration));
        var addTypeParameter = Assert.Single(addSymbol.TypeParameters);
        var exprInterface = Assert.Single(addSymbol.Interfaces.Where(i => i.Name == "Expr"));

        Assert.Same(addTypeParameter, Assert.Single(exprInterface.TypeArguments));

        var evaluateApplyInvocation = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation =>
                invocation.Expression is MemberAccessExpressionSyntax { Name.Identifier.ValueText: "Apply" } memberAccess &&
                memberAccess.Ancestors().OfType<FunctionStatementSyntax>().Any(function => function.Identifier.ValueText == "Evaluate"));
        var boundEvaluateInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(evaluateApplyInvocation));

        Assert.Equal("Apply", boundEvaluateInvocation.Method.Name);
        Assert.Equal("Expr", boundEvaluateInvocation.Method.ContainingType?.Name);
        Assert.Equal("Expr<T>", boundEvaluateInvocation.Receiver?.Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("T", boundEvaluateInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var addApplyBinary = root.DescendantNodes()
            .OfType<InfixOperatorExpressionSyntax>()
            .Single(binary => binary.OperatorToken.Kind == SyntaxKind.PlusToken);

        Assert.Equal("T", model.GetTypeInfo(addApplyBinary).Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }
}
