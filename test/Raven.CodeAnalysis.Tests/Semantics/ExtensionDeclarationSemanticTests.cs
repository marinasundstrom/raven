using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExtensionDeclarationSemanticTests : CompilationTestBase
{
    [Fact]
    public void ExtensionDeclaration_OpenGenericReceiver_BindsExtensionMethod()
    {
        const string source = """
import System.Collections.Generic.*

extension MyEnumerableExt<T> for IEnumerable<T> {
    CountItems() -> int {
        var total: int = 0
        for each _ in self {
            total = total + 1
        }
        return total
    }
}

func main() {
    let items = List<int>()
    items.Add(1)
    items.Add(2)
    let count = items.CountItems()
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var extensionDecl = tree.GetRoot().DescendantNodes().OfType<ExtensionDeclarationSyntax>().Single();
        var extensionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(extensionDecl));

        Assert.True(extensionSymbol.IsAbstract);
        Assert.True(extensionSymbol.IsSealed);
        Assert.Equal("MyEnumerableExt`1", extensionSymbol.MetadataName);
        var typeParameter = Assert.Single(extensionSymbol.TypeParameters);

        var methodDecl = extensionDecl.Members.OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDecl));

        Assert.True(methodSymbol.IsExtensionMethod);
        Assert.Equal("CountItems", methodSymbol.Name);
        var selfParameter = Assert.Single(methodSymbol.Parameters);
        Assert.Equal("self", selfParameter.Name);

        var receiverType = Assert.IsAssignableFrom<INamedTypeSymbol>(selfParameter.Type);
        Assert.Equal("IEnumerable", receiverType.Name);
        var receiverArgument = Assert.Single(receiverType.TypeArguments);
        Assert.Same(typeParameter, receiverArgument);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.ValueText == "CountItems");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(SymbolEqualityComparer.Default.Equals(methodSymbol, boundInvocation.Method));
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(selfParameter.Type, boundInvocation.ExtensionReceiver!.Type));
    }

    [Fact]
    public void ExtensionDeclaration_ClosedGenericReceiver_InjectsSelfParameter()
    {
        const string source = """
import System.Collections.Generic.*

extension MyIntEnumerableExt for IEnumerable<int> {
    Sum() -> int {
        return 0
    }
}

func main() {
    let items = List<int>()
    items.Add(3)
    items.Add(5)
    let total = items.Sum()
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var extensionDecl = tree.GetRoot().DescendantNodes().OfType<ExtensionDeclarationSyntax>().Single();
        var extensionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(extensionDecl));

        Assert.True(extensionSymbol.IsAbstract);
        Assert.True(extensionSymbol.IsSealed);
        Assert.Equal("MyIntEnumerableExt", extensionSymbol.MetadataName);

        var methodDecl = extensionDecl.Members.OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDecl));

        Assert.True(methodSymbol.IsExtensionMethod);
        var selfParameter = Assert.Single(methodSymbol.Parameters);
        Assert.Equal("self", selfParameter.Name);

        var receiverType = Assert.IsAssignableFrom<INamedTypeSymbol>(selfParameter.Type);
        Assert.Equal("IEnumerable", receiverType.Name);
        var receiverArgument = Assert.Single(receiverType.TypeArguments);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, receiverArgument));

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.ValueText == "Sum");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(SymbolEqualityComparer.Default.Equals(methodSymbol, boundInvocation.Method));
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(selfParameter.Type, boundInvocation.ExtensionReceiver!.Type));
    }

    [Fact]
    public void ExtensionDeclaration_OpenGenericReceiver_AllowsTopLevelInvocation()
    {
        const string source = """
import System.Collections.Generic.*

let items = List<int>()
items.Add(1)
items.Add(2)
let count = items.CountItems()

extension MyEnumerableExt<T> for IEnumerable<T> {
    CountItems() -> int {
        var total: int = 0
        for each x in self {
            total = total + 1
        }
        return total
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }
}
