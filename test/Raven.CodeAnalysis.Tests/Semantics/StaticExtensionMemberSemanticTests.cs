using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StaticExtensionMemberSemanticTests : CompilationTestBase
{
    [Fact]
    public void StaticExtensionMethod_QualifiedLookup_BindsToExtensionContainer()
    {
        const string source = """
class Widget { }

extension WidgetExtensions for Widget {
    public static Build(value: int) -> Widget {
        return Widget()
    }
}

let created = Widget.Build(1)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodDecl = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "Build");
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDecl));

        Assert.False(methodSymbol.IsExtensionMethod);

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.True(SymbolEqualityComparer.Default.Equals(methodSymbol, boundInvocation.Method));
        Assert.Null(boundInvocation.ExtensionReceiver);
    }

    [Fact]
    public void StaticExtensionMethod_WildcardImport_EnablesUnqualifiedAccess()
    {
        const string source = """
import Widget.*

class Widget { }

extension WidgetExtensions for Widget {
    public static Build(value: int) -> Widget {
        return Widget()
    }
}

let created = Build(1)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("WidgetExtensions", boundInvocation.Method.ContainingType?.Name);
    }

    [Fact]
    public void StaticExtensionMethod_PrefersRealStaticMembers()
    {
        const string source = """
class Widget {
    public static Build(value: int) -> Widget {
        return Widget()
    }
}

extension WidgetExtensions for Widget {
    public static Build(value: int) -> Widget {
        return Widget()
    }
}

let created = Widget.Build(1)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("Widget", boundInvocation.Method.ContainingType?.Name);
    }

    [Fact]
    public void StaticExtensionMethod_InfersExtensionContainerTypeArguments()
    {
        const string source = """
class Box<T> { }

extension BoxExtensions<T> for Box<T> {
    public static Wrap(value: T) -> Box<T> {
        return Box<T>()
    }
}

let boxed = Box<int>.Wrap(1)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundInvocation.Method.ContainingType);
        Assert.Equal("BoxExtensions", containingType.Name);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var typeArgument = Assert.Single(containingType.TypeArguments);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, typeArgument));
    }

    [Fact]
    public void StaticExtensionProperty_QualifiedLookup_BindsToExtensionContainer()
    {
        const string source = """
class Counter { }

extension CounterExtensions for Counter {
    public static Total: int {
        get { return 42; }
    }
}

let total = Counter.Total
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.Text == "Total");

        var boundAccess = Assert.IsType<BoundMemberAccessExpression>(model.GetBoundNode(memberAccess));
        var propertySymbol = Assert.IsAssignableFrom<IPropertySymbol>(boundAccess.Member);
        Assert.False(propertySymbol.IsExtensionProperty());
        Assert.Equal("CounterExtensions", propertySymbol.ContainingType?.Name);
    }
}
