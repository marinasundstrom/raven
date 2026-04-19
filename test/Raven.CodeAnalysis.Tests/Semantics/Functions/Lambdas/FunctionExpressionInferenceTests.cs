using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FunctionExpressionInferenceTests : CompilationTestBase
{
    [Fact]
    public void Lambda_WithoutParameterTypes_UsesTargetDelegateSignature()
    {
        const string code = """
import System.*
class Calculator {
    func Transform(value: int, projector: Func<int, int>) -> int {
        return projector(value)
    }

    func Apply() -> int {
        return Transform(5, delta => delta + 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.False(boundLambda.CandidateDelegates.IsDefaultOrEmpty);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, parameter.Type),
            parameter.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.NotNull(boundLambda.Body.Type);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, boundLambda.Body.Type),
            $"{boundLambda.Body.GetType().FullName} - {boundLambda.Body.Type?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType),
            boundLambda.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.True(
            SymbolEqualityComparer.Default.Equals(
                compilation.GetTypeByMetadataName("System.Func`2")?.Construct(intType, intType),
                boundLambda.DelegateType),
            boundLambda.DelegateType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var transformSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .First(f => f.Identifier.Text == "Transform");
        var transformSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(transformSyntax));
        var projectorParameter = Assert.Single(transformSymbol.Parameters, p => p.Name == "projector");
        Assert.True(SymbolEqualityComparer.Default.Equals(projectorParameter.Type, boundLambda.DelegateType));
    }

    [Fact]
    public void Lambda_AssignedToMetadataDelegate_InfersParameterTypes()
    {
        const string code = """
import System.*
import System.ComponentModel.*

class Program {
    static func Run() -> unit {
        val handler: PropertyChangedEventHandler = (sender, args) => {
            Console.WriteLine(args.PropertyName ?? "")
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var parameters = boundLambda.Parameters.ToArray();
        Assert.Equal(2, parameters.Length);

        var propertyChangedArgsType = compilation.GetTypeByMetadataName("System.ComponentModel.PropertyChangedEventArgs");

        Assert.Equal("object?", parameters[0].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.True(SymbolEqualityComparer.Default.Equals(propertyChangedArgsType, parameters[1].Type));
    }

    [Fact]
    public void LambdaParameter_DeclaredSymbol_UsesInferredType_FromEventSubscription()
    {
        const string code = """
import System.*
import System.ComponentModel.*

open class ObservableBase : INotifyPropertyChanged {
    event PropertyChanged: PropertyChangedEventHandler?

    protected func RaisePropertyChanged(propertyName: string) -> unit {
        PropertyChanged?(self, PropertyChangedEventArgs(propertyName))
    }
}

class MyViewModel : ObservableBase {
    var Title: string = ""
}

class Program {
    static func Main() -> unit {
        val viewModel = MyViewModel()
        viewModel.PropertyChanged += (sender, args) => {
            Console.WriteLine(args.PropertyName ?? "")
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();
        var argsParameterSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single()
            .ParameterList
            .Parameters
            .Single(parameter => parameter.Identifier.ValueText == "args");

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var boundArgsParameter = boundLambda.Parameters.ElementAt(1);
        var argsParameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(argsParameterSyntax));
        var propertyChangedArgsType = compilation.GetTypeByMetadataName("System.ComponentModel.PropertyChangedEventArgs");

        Assert.True(
            SymbolEqualityComparer.Default.Equals(propertyChangedArgsType, boundArgsParameter.Type),
            boundArgsParameter.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.True(SymbolEqualityComparer.Default.Equals(propertyChangedArgsType, argsParameter.Type));
    }

    [Fact]
    public void LambdaParameter_DeclaredSymbol_UsesInferredType_FromSourceDefinedDelegateEventSubscription()
    {
        const string code = """
import System.*

class ChangedArgs(var PropertyName: string)

delegate PropertyChangedHandler(sender: object?, e: ChangedArgs) -> unit

open class ObservableBase {
    event PropertyChanged: PropertyChangedHandler?

    protected func RaisePropertyChanged(propertyName: string) -> unit {
        PropertyChanged?(self, ChangedArgs(propertyName))
    }
}

class MyViewModel : ObservableBase {
    var Title: string = ""
}

class Program {
    static func Main() -> unit {
        val viewModel = MyViewModel()
        viewModel.PropertyChanged += (sender, args) => {
            Console.WriteLine(args.PropertyName ?? "")
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();
        var lambdaParameters = lambdaSyntax.ParameterList.Parameters.ToArray();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        boundLambda.Parameters.Count().ShouldBe(2);

        var senderParameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(lambdaParameters[0]));
        var argsParameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(lambdaParameters[1]));
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var changedArgsType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("ChangedArgs"));

        senderParameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("object?");
        SymbolEqualityComparer.Default.Equals(changedArgsType, argsParameter.Type).ShouldBeTrue();
        SymbolEqualityComparer.Default.Equals(objectType.MakeNullable(), boundLambda.Parameters.ElementAt(0).Type).ShouldBeTrue();
        SymbolEqualityComparer.Default.Equals(changedArgsType, boundLambda.Parameters.ElementAt(1).Type).ShouldBeTrue();
    }

    [Fact]
    public void Lambda_WithConflictingDelegateCandidates_SuppressesParameterInferenceDiagnostic()
    {
        const string code = """
import System.*
class Container {
    func Overloaded(pred: Func<int, bool>) -> unit { }
    func Overloaded(pred: Func<string, bool>) -> unit { }

    func Invoke() -> unit {
        Overloaded(value => true)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => ReferenceEquals(
                diagnostic.Descriptor,
                CompilerDiagnostics.LambdaParameterTypeCannotBeInferred));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.NotNull(boundLambda.Unbound);
    }

    [Fact]
    public void Lambda_WithCandidateMissingParameters_StillInfersTypeFromValidDelegate()
    {
        const string code = """
import System.*
class Container {
    func Overloaded(projector: Func<int, int>) -> int {
        return projector(1)
    }

    func Overloaded(callback: Action) -> int {
        callback()
        return 0
    }

    func Invoke() -> int {
        return Overloaded(value => value + 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type));
    }

    [Fact]
    public void Lambda_WithoutReturnType_InferredFromBody()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val lambda = (value: int) => value + 1
        lambda(1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaSymbol = Assert.IsAssignableFrom<IMethodSymbol>(boundLambda.Symbol);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, lambdaSymbol.ReturnType));
    }

    [Fact]
    public void FuncLambda_WithoutReturnType_InferredFromExpressionBody()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val f = func (a: int, b: int) => a + b
        f(1, 2)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
    }

    [Fact]
    public void FuncLambda_WithoutReturnType_InferredFromBlockBody()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val f = func (a: int, b: int) => {
            a + b
        }
        f(1, 2)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
    }

    [Fact]
    public void FuncLambda_WithoutReturnType_InferredFromIteratorBody()
    {
        const string code = """
import System.*
import System.Collections.Generic.*

class Container {
    func Provide() -> unit {
        val values = func () => {
            yield return 1
            yield return 2
        }

        values()
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var enumerableType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundLambda.ReturnType);
        Assert.Equal(SpecialType.System_Collections_Generic_IEnumerable_T, enumerableType.OriginalDefinition.SpecialType);
        Assert.Equal(SpecialType.System_Int32, enumerableType.TypeArguments[0].SpecialType);
    }

    [Fact]
    public void AsyncFuncLambda_WithoutReturnType_InferredFromIteratorBody()
    {
        const string code = """
import System.*
import System.Collections.Generic.*

class Container {
    func Provide() -> unit {
        val values = async func () => {
            yield return 1
            yield return 2
        }

        values()
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var enumerableType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundLambda.ReturnType);
        Assert.Equal("IAsyncEnumerable`1", enumerableType.OriginalDefinition.MetadataName);
        Assert.Equal(SpecialType.System_Int32, enumerableType.TypeArguments[0].SpecialType);
    }

    [Fact]
    public void FuncLambda_BlockBodyWithoutArrow_CapturesOuterVariable()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> int {
        val offset = 2
        val f = func (value: int) {
            value + offset
        }
        f(3)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var captures = model.GetCapturedVariables(lambdaSyntax);
        Assert.Contains(captures, symbol => symbol.Name == "offset");
    }

    [Fact]
    public void FuncLambda_BlockBodyTailIfElse_ContributesImplicitReturn()
    {
        const string code = """
class Container {
    func Provide() -> int {
        val f = func Fib(n: int) -> int {
            if n < 2 {
                n
            } else {
                Fib(n - 1) + Fib(n - 2)
            }
        }

        f(10)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
    }

    [Fact]
    public void FuncLambda_BlockBodyTailIfElseWithoutBraces_ContributesImplicitReturn()
    {
        const string code = """
class Container {
    func Provide() -> int {
        val f = func Fib(n: int) -> int {
            if n < 2
                n
            else
                Fib(n - 1) + Fib(n - 2)
        }

        f(10)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
    }

    [Fact]
    public void FuncLambda_AssignedToFunctionTypeSignature_Binds()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val f: (int, int) -> int = func (a: int, b: int) => a + b
        f(1, 2)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        Assert.Equal("(int, int) -> int", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void FuncLambda_WithTypeParametersAndConstraints_Binds()
    {
        const string code = """
import System.Numerics.*

class Container {
    func Provide() -> int {
        val f = func<T>(a: T, b: T) where T: INumber<T> {
            a
        }

        f(2, 3)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaSymbol = Assert.IsAssignableFrom<IMethodSymbol>(boundLambda.Symbol);

        Assert.True(lambdaSymbol.IsGenericMethod);
        Assert.Single(lambdaSymbol.TypeParameters);
        Assert.NotEmpty(lambdaSymbol.TypeParameters[0].ConstraintTypes);
    }

    [Fact]
    public void FuncLambda_WithIdentifier_BindsSelfReferenceInsideBody()
    {
        const string code = """
class Container {
    func Provide() -> unit {
        val f: (int) -> int = func Fib(n: int) => Fib(n)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var fibInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First(inv => inv.Expression is IdentifierNameSyntax identifier && identifier.Identifier.ValueText == "Fib");

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(fibInvocation).Symbol);
        Assert.Equal(MethodKind.LambdaMethod, symbol.MethodKind);
    }

    [Fact]
    public void FuncLambda_WithIdentifierRecursiveCall_DoesNotCaptureContainingType()
    {
        const string code = """
class Container {
    func Provide() -> int {
        val f: (int) -> int = func Fib(n: int) -> int {
            if n < 2
                n
            else
                Fib(n - 1) + Fib(n - 2)
        }
        f(5)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var captures = model.GetCapturedVariables(lambdaSyntax);

        Assert.DoesNotContain(captures, static symbol => symbol is ITypeSymbol { Name: "Container" });
    }

    [Fact]
    public void FuncLambda_WithIdentifier_IsNotVisibleOutsideBody()
    {
        const string code = """
class Container {
    func Provide() -> unit {
        val f: (int) -> int = func Fib(n: int) => Fib(n)
        Fib(1)
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == "RAV0103");
    }

    [Fact]
    public void FuncLambda_AssignedToDelegateType_BindsAndProjectsBuiltInDelegates()
    {
        const string code = """
import System.*
class Container {
    public delegate MyHandler(value: int) -> unit

    func Provide() -> unit {
        val action: Action<int> = func (x: int) {
        }

        val handler: Container.MyHandler = func (x: int) {
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        Assert.Equal(2, declarators.Length);

        var actionLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators[0]));
        var handlerLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators[1]));

        Assert.Equal("int -> ()", actionLocal.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.StartsWith("delegate MyHandler(", handlerLocal.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void Lambda_ParameterWithDefaultValue_BindsAsOptionalParameter()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val f = (name: string, age: int = 1) => age
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var parameters = boundLambda.Parameters.ToArray();
        Assert.Equal(2, parameters.Length);
        Assert.False(parameters[0].HasExplicitDefaultValue);
        Assert.True(parameters[1].HasExplicitDefaultValue);
        Assert.Equal(1, parameters[1].ExplicitDefaultValue);
    }

    [Fact]
    public void Lambda_LeadingAttribute_BindsToSingleParameter()
    {
        const string code = """
import System.*

class Container {
    func Provide() -> unit {
        val f = [Obsolete](content: string) => content
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var syntaxParameter = Assert.Single(lambdaSyntax.ParameterList.Parameters);
        Assert.Single(syntaxParameter.AttributeLists);

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.Single(boundLambda.Parameters);
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_DoesNotBindAgainstOpenTypeParameter()
    {
        const string code = """
import System.Linq.*

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 { 0 } else { n * 2 })
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_QualifiedUnionCases_ReportsConversionDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

public union Option<T> {
    Some(value: T)
    None
}

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 {
            Option<int>.None
        } else {
            Option<int>.Some(n * 2)
        })
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == "RAV1503");
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_QualifiedResultCases_InfersResultType()
    {
        const string code = """
import System.*
import System.Linq.*

public union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 {
            Result<int, string>.Error("neg")
        } else {
            Result<int, string>.Ok(n * 2)
        })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.Text == "result");

        var resultLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        var resultType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultLocal.Type);
        var elementType = Assert.Single(resultType.TypeArguments);

        Assert.IsNotType<ITypeUnionSymbol>(elementType);

        var expectedResult = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Result`2"))
            .Construct(
                compilation.GetSpecialType(SpecialType.System_Int32),
                compilation.GetSpecialType(SpecialType.System_String));

        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedResult, elementType),
            $"Expected: {expectedResult.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}, Actual: {elementType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_QualifiedCustomUnionCases_InfersCarrierType()
    {
        const string code = """
import System.*
import System.Linq.*

public union Response<T, E> {
    Success(value: T)
    Failure(error: E)
}

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 {
            Response<int, string>.Failure("neg")
        } else {
            Response<int, string>.Success(n * 2)
        })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.Text == "result");

        var resultLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        var resultType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultLocal.Type);
        var elementType = Assert.Single(resultType.TypeArguments);

        Assert.IsNotType<ITypeUnionSymbol>(elementType);

        var expectedResponse = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Response`2"))
            .Construct(
                compilation.GetSpecialType(SpecialType.System_Int32),
                compilation.GetSpecialType(SpecialType.System_String));

        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedResponse, elementType),
            $"Expected: {expectedResponse.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}, Actual: {elementType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_SiblingClasses_InfersCommonBaseType()
    {
        const string code = """
import System.*
import System.Linq.*

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 {
            InvalidOperationException()
        } else {
            ApplicationException()
        })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.Text == "result");

        var resultLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        var resultType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultLocal.Type);
        var elementType = Assert.Single(resultType.TypeArguments);

        var expectedException = compilation.GetTypeByMetadataName("System.Exception");
        Assert.NotNull(expectedException);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedException, elementType),
            $"Expected: {expectedException!.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}, Actual: {elementType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_ImplicitOperatorTargets_InfersBestCommonType()
    {
        const string code = """
import System.*
import System.Linq.*

class A {
    static func implicit(value: A) -> string { return "a" }
}

class B {
    static func implicit(value: B) -> string { return "b" }
}

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 { A() } else { B() })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.Text == "result");

        var resultLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        var resultType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultLocal.Type);
        var elementType = Assert.Single(resultType.TypeArguments);
        Assert.Equal("String", elementType.Name);
    }

    [Fact]
    public void Lambda_InGenericLinqWhere_AllowsMemberAccessOnConcreteRecordType()
    {
        const string code = """
import System.Linq.*
import System.Collections.Generic.*

record class RatePlan(Carrier: string)

class Container {
    func HasCarrier(carrier: string) -> bool {
        val plans = List<RatePlan> { RatePlan("NorthStar") }
        val selected = plans.Where(p => p.Carrier == carrier)
        return selected.Any()
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Lambda_LocalDeclaration_InfersFuncDelegate()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val projector = (value: int) => value + 2
        val result = projector(1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarator = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First(d => d.Identifier.Text == "projector");
        var lambdaSyntax = declarator
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var localSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var funcDefinition = compilation.GetTypeByMetadataName("System.Func`2");
        Assert.NotNull(funcDefinition);
        INamedTypeSymbol expectedDelegateDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(funcDefinition);
        INamedTypeSymbol expectedDelegate = (INamedTypeSymbol)expectedDelegateDefinition.Construct(intType, intType);

        INamedTypeSymbol actualDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(localSymbol.Type);
        var actualArgs = string.Join(", ", actualDelegate.TypeArguments.Select(arg => arg.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)));
        var expectedArgs = string.Join(", ", expectedDelegate.TypeArguments.Select(arg => arg.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)));
        var actualDisplay = actualDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        var expectedDisplay = expectedDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

        Assert.Equal(expectedDisplay, actualDisplay);
        Assert.Equal(expectedArgs, actualArgs);

        var lambdaTypeInfo = model.GetTypeInfo(lambdaSyntax);
        INamedTypeSymbol lambdaType = Assert.IsAssignableFrom<INamedTypeSymbol>(lambdaTypeInfo.Type);
        Assert.Equal(expectedDisplay, lambdaType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        INamedTypeSymbol boundDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(boundLambda.DelegateType);
        Assert.Equal(expectedDisplay, boundDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }

    [Fact]
    public void Lambda_GlobalDeclaration_UsesFuncDelegate()
    {
        const string code = """
import System.*

val projector = (value: int) => value + 2
val result = projector(1)
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarator = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First(d => d.Identifier.Text == "projector");
        var lambdaSyntax = declarator
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var localSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var funcDefinition = compilation.GetTypeByMetadataName("System.Func`2");
        Assert.NotNull(funcDefinition);
        INamedTypeSymbol expectedDelegateDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(funcDefinition);
        INamedTypeSymbol expectedDelegate = (INamedTypeSymbol)expectedDelegateDefinition.Construct(intType, intType);

        INamedTypeSymbol actualDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(localSymbol.Type);
        var actualDisplay = actualDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        var expectedDisplay = expectedDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Equal(expectedDisplay, actualDisplay);

        var lambdaTypeInfo = model.GetTypeInfo(lambdaSyntax);
        INamedTypeSymbol lambdaType = Assert.IsAssignableFrom<INamedTypeSymbol>(lambdaTypeInfo.Type);
        Assert.Equal(expectedDisplay, lambdaType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        INamedTypeSymbol boundDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(boundLambda.DelegateType);
        Assert.Equal(expectedDisplay, boundDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }

    [Fact]
    public void Lambda_WithOverloadSpecificDelegate_RebindsToSelectedMethod()
    {
        const string code = """
import System.*
class Container {
    func Overloaded(projector: Func<int, int>, value: int) -> int {
        return projector(value)
    }

    func Overloaded(projector: Func<string, string>, value: string) -> string {
        return projector(value)
    }

    func Invoke() -> int {
        return Overloaded(value => value + 1, 5)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax { Identifier.ValueText: "Overloaded" });

        var invocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        var method = Assert.IsAssignableFrom<IMethodSymbol>(invocation.Method);

        Assert.Equal("Overloaded", method.Name);

        var funcDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Func`2"));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var expectedDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(funcDefinition.Construct(intType, intType));

        var projectorParameter = Assert.Single(method.Parameters, p => p.Name == "projector");
        Assert.Equal(
            expectedDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat),
            projectorParameter.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var lambdaArgument = Assert.IsAssignableFrom<BoundFunctionExpression>(invocation.Arguments.First());
        Assert.NotNull(lambdaArgument.Unbound);
        Assert.False(lambdaArgument.CandidateDelegates.IsDefaultOrEmpty);
        var lambdaParameter = Assert.Single(lambdaArgument.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, lambdaParameter.Type));
    }

    [Fact]
    public void Lambda_AssignedToLocal_InvocationBindsToDelegateInvoke()
    {
        const string code = """
import System.*
class Calculator {
    func Compute() -> int {
        val add = (left: int, right: int) -> int => left + right
        return add(2, 3)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.Equal("Invoke", boundInvocation.Method.Name);

        Assert.NotNull(boundInvocation.Receiver);
        var receiver = boundInvocation.Receiver!;
        var delegateType = Assert.IsAssignableFrom<INamedTypeSymbol>(receiver.Type);
        Assert.Equal(TypeKind.Delegate, delegateType.TypeKind);
        Assert.Same(delegateType.GetDelegateInvokeMethod(), boundInvocation.Method);
    }

    [Fact]
    public void Lambda_ReturningLambda_WithExplicitReturnType_ComposesSuccessfully()
    {
        const string code = """
import System.*

val makeAdder = (x: int) -> (int -> int) => (a: int) => x + a
""";

        var (compilation, _) = CreateCompilation(code, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Lambda_WithUnionCaseConstructor_ReportsConversionDiagnostic()
    {
        const string code = """
import System.*

public union Option<T> {
    Some(value: T)
    None
}

class Container {
    func Map(value: int, selector: Func<int, Option<int>>) -> Option<int> {
        return selector(value)
    }

    func Test() -> Option<int> {
        return Map(1, n => if n < 0 { Option<int>.None } else { Option<int>.Some(n * 2) })
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == "RAV1503");
    }

    [Fact]
    public void MetadataDelegate_PreservesDelegateTypeKind_WhenConstructed()
    {
        const string code = "class Container { }";

        var (compilation, _) = CreateCompilation(code);

        var definition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Func`2"));
        Assert.Equal(TypeKind.Delegate, definition.TypeKind);

        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(definition.Construct(
            compilation.GetSpecialType(SpecialType.System_Int32),
            compilation.GetSpecialType(SpecialType.System_Int32)));
        Assert.Equal(TypeKind.Delegate, constructed.TypeKind);
    }

    [Fact]
    public void GenericFunction_WithCollectionLiteralAndLambda_InfersTypeParameterFromCollection()
    {
        const string source = """
import System.Collections.Generic.*

ForEach([1, 2], no => {})

func ForEach<T>(source: IEnumerable<T>, callback: T -> ()) -> () { }
""";
        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes().OfType<SimpleFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void GenericFunction_WithMethodGroupArgument_InfersTypeParameterAndResolvesOverload()
    {
        const string source = """
import System.*
import System.Console.*
import System.Collections.Generic.*

val items: List<int> = [1, 2, 3]
ForEach(items, WriteLine)

func ForEach<T>(source: IEnumerable<T>, callback: T -> ()) -> () { }
""";
        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(n => n.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "ForEach");
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsGenericMethod);
        Assert.Equal(SpecialType.System_Int32, boundInvocation.Method.TypeArguments[0].SpecialType);

        var writeLineNode = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(n => n.Identifier.ValueText == "WriteLine");
        var symbolInfo = model.GetSymbolInfo(writeLineNode);
        var selectedMethod = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.Equal("WriteLine", selectedMethod.Name);
        Assert.Equal(SpecialType.System_Int32, selectedMethod.Parameters[0].Type.SpecialType);
    }

    [Fact]
    public void GenericFunction_WithTypedSourceAndLambda_InfersTypeParameter()
    {
        const string source = """
import System.Collections.Generic.*

val items: IEnumerable<int> = [1, 2, 3]
ForEach(items, x => {})

func ForEach<T>(source: IEnumerable<T>, callback: T -> ()) -> () { }
""";
        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes().OfType<SimpleFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void GenericFunction_WithExplicitTypeArguments_AndActionLambda_DoesNotRequireFurtherInference()
    {
        const string source = """
import System.*

class Container {
    static func Create<T>(handler: Action<T>) -> unit { }

    static func Main() -> unit {
        Create<int>(value => {})
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes().OfType<SimpleFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void Lambda_TargetingAction_CanDiscardExpressionBodyResult()
    {
        const string source = """
import System.*

class Builder {
    func Configure() -> int {
        return 1
    }
}

class Container {
    static func Create(handler: Action<Builder>) -> unit { }

    static func Main() -> unit {
        Create(builder => builder.Configure())
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes().OfType<SimpleFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal("Builder", parameter.Type.Name);
        Assert.Equal(SpecialType.System_Unit, boundLambda.ReturnType.SpecialType);
    }

    [Fact]
    public void GenericFunction_ToDictionary_WithTwoLambdas_InfersTypeParameters()
    {
        const string source = """
import System.*
import System.Linq.*

func Main() {
    val o = [1, 2, 3].ToDictionary(x => x, y => y)
}
""";
        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(n => n.Expression is MemberAccessExpressionSyntax m && m.Name.Identifier.Text == "ToDictionary");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundInvocation.Type);
        Assert.Equal("Dictionary", returnType.Name);
        Assert.Equal(2, returnType.TypeArguments.Length);
        Assert.Equal(SpecialType.System_Int32, returnType.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_Int32, returnType.TypeArguments[1].SpecialType);

        var lambdas = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .ToDictionary(lambda => lambda.Parameter.Identifier.ValueText);

        var xLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdas["x"]));
        var xParam = Assert.Single(xLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, xParam.Type.SpecialType);

        var yLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdas["y"]));
        var yParam = Assert.Single(yLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, yParam.Type.SpecialType);
    }

    [Fact]
    public void GenericFunction_WithOpenDelegateInputType_ReportsInferenceDiagnostic()
    {
        const string code = """
class Container {
    static func Build<T>(projector: T -> int) -> int {
        return 0
    }

    func Run() -> int {
        return Build(x => x)
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => ReferenceEquals(
                diagnostic.Descriptor,
                CompilerDiagnostics.LambdaParameterTypeCannotBeInferred));
    }

    [Fact]
    public void StaticFuncExpression_CapturingOuterLocal_ReportsDiagnostic()
    {
        const string code = """
class C {
    func M() -> int {
        val offset = 2
        val f = static func (x: int) {
            x + offset
        }

        return f(1)
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
                diagnostic => ReferenceEquals(
                diagnostic.Descriptor,
                CompilerDiagnostics.StaticFunctionExpressionCannotCapture));
    }

    [Fact]
    public void ContinueWith_FunctionExpressionBody_ResolvesReceiverAndResultMember()
    {
        const string code = """
import System.Console.*
import System.Threading.Tasks.*

async func Main() -> Task {
    val f = async func (a: int, b: int) {
        await Task.FromResult(a + b)
    }

    val x = f(2, 3).ContinueWith(x => {
        x.Result
    })

    WriteLine(await f(2, 3))
}
""";

        var (compilation, tree) = CreateCompilation(code, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Name is IdentifierNameSyntax { Identifier.ValueText: "Result" });

        var resultSymbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetSymbolInfo(memberAccess).Symbol);
        Assert.Equal("Result", resultSymbol.Name);
        Assert.Equal(SpecialType.System_Int32, resultSymbol.Type.SpecialType);

        var receiverIdentifier = Assert.IsType<IdentifierNameSyntax>(memberAccess.Expression);
        var receiverSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetSymbolInfo(receiverIdentifier).Symbol);
        var receiverType = Assert.IsAssignableFrom<INamedTypeSymbol>(receiverSymbol.Type);
        Assert.Equal("Task", receiverType.Name);
        Assert.Single(receiverType.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, receiverType.TypeArguments[0].SpecialType);
    }

    [Fact]
    public void Lambda_WithPositionalDestructuringParameter_UsesInferredTupleType()
    {
        const string code = """
import System.*
class C {
    func Apply(projector: Func<(int, int), int>) -> int {
        return projector((4, 6))
    }

    func Test() -> int {
        return Apply(((a, b)) => a + b)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var tupleParameter = Assert.Single(boundLambda.Parameters);
        Assert.IsAssignableFrom<ITupleTypeSymbol>(tupleParameter.Type);

        var localNames = lambdaSyntax
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Select(id => model.GetSymbolInfo(id).Symbol)
            .OfType<ILocalSymbol>()
            .Select(local => local.Name)
            .ToHashSet(StringComparer.Ordinal);
        Assert.Contains("a", localNames);
        Assert.Contains("b", localNames);
    }

    [Fact]
    public void Lambda_WithSequenceDestructuringParameter_SupportsJsStyleRest()
    {
        const string code = """
import System.*
class C {
    func Apply(projector: Func<int[], int>) -> int {
        return projector([1, 2, 3])
    }

    func Test() -> int {
        return Apply(([a, ...rest]) => a + rest[0] + rest[1])
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var arrayParameter = Assert.Single(boundLambda.Parameters);
        Assert.IsAssignableFrom<IArrayTypeSymbol>(arrayParameter.Type);

        var restSymbols = lambdaSyntax
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(id => id.Identifier.ValueText == "rest")
            .Select(id => model.GetSymbolInfo(id).Symbol)
            .OfType<ILocalSymbol>()
            .Where(local => local.Name == "rest")
            .ToArray();
        Assert.NotEmpty(restSymbols);
        Assert.All(restSymbols, local => Assert.IsAssignableFrom<IArrayTypeSymbol>(local.Type));
    }

    [Fact]
    public void Lambda_WithPositionalDestructuringParameter_SupportsRecordDeconstruct()
    {
        const string code = """
import System.*

record class Pair(Left: int, Right: int)

class C {
    func Apply(projector: Func<Pair, int>) -> int {
        return projector(Pair(4, 6))
    }

    func Test() -> int {
        return Apply(((left, right)) => left + right)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.IsAssignableFrom<INamedTypeSymbol>(parameter.Type);

        var localNames = lambdaSyntax
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Select(id => model.GetSymbolInfo(id).Symbol)
            .OfType<ILocalSymbol>()
            .Select(local => local.Name)
            .ToHashSet(StringComparer.Ordinal);
        Assert.Contains("left", localNames);
        Assert.Contains("right", localNames);
    }
}

public class FunctionExpressionInferenceDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void Lambda_WithErrorTypeArgument_DoesNotReportConversionError()
    {
        const string code = """
func apply(value: int, transform: int -> int) -> int {
    transform(value)
}

val doubled = x => x * 2

val result = apply(5, doubled)
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2200").WithSpan(5, 15, 5, 16).WithArguments("x"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Lambda_WithBodyError_ReportsBodyDiagnostics()
    {
        const string code = """
import System.*

class Container {
    func Map(projector: Func<string, string>) -> unit { }

    func Test() -> unit {
        Map((name: string) => missingValue)
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments("missingValue"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Lambda_WithDestructuredParameterBodyError_DoesNotReportOverloadCascade()
    {
        const string code = """
import System.*
import System.Linq.*

class Container {
    func Test() -> unit {
        val x2: int[][] = [[1, 2], [2, 3, 4]]
        val r2 = x2.Select(([a, ..rest]) => b)
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments("b"),
            ]);

        verifier.Verify();
    }
}
