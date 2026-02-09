using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class InvocationOperatorTests : CompilationTestBase
{
    [Fact]
    public void InvocationOperator_BindsToInvokeMethod()
    {
        var source = """
class Test {
    public self(no: int) -> int {
        return no + 1;
    }
}
val t = Test()
val x = t(2)
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Invoke", symbol.Name);
    }

    [Fact]
    public void InvocationOperator_InterfaceDeclaration_BindsInvokeMethod()
    {
        var source = """
interface ICallable {
    self(value: int) -> int;
}

class Callable : ICallable {
    public self(value: int) -> int {
        return value + 1;
    }
}

val callable: ICallable = Callable()
val result = callable(2)
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Invoke", symbol.Name);
        Assert.Equal(TypeKind.Interface, symbol.ContainingType.TypeKind);
        Assert.Equal("ICallable", symbol.ContainingType.Name);
    }

    [Fact]
    public void InvocationOperator_Modifiers_AreTrackedOnSymbols()
    {
        var source = """
abstract class Base {
    public abstract self(value: int) -> int;
}

class Derived : Base {
    public override self(value: int) -> int {
        return value;
    }
}

open class VirtualBase {
    public virtual self(value: int) -> int {
        return value;
    }
}

class VirtualDerived : VirtualBase {
    public override self(value: int) -> int {
        return value + 2;
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var baseType = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "Base");
        var baseMethod = baseType.Members.OfType<MethodDeclarationSyntax>().Single();
        var baseSymbol = (IMethodSymbol)model.GetDeclaredSymbol(baseMethod)!;
        Assert.True(baseSymbol.IsAbstract);
        Assert.True(baseSymbol.IsVirtual);

        var derivedType = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "Derived");
        var derivedMethod = derivedType.Members.OfType<MethodDeclarationSyntax>().Single();
        var derivedSymbol = (IMethodSymbol)model.GetDeclaredSymbol(derivedMethod)!;
        Assert.True(derivedSymbol.IsOverride);

        var virtualBaseType = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "VirtualBase");
        var virtualBaseMethod = virtualBaseType.Members.OfType<MethodDeclarationSyntax>().Single();
        var virtualBaseSymbol = (IMethodSymbol)model.GetDeclaredSymbol(virtualBaseMethod)!;
        Assert.True(virtualBaseSymbol.IsVirtual);
        Assert.False(virtualBaseSymbol.IsOverride);
    }
}
