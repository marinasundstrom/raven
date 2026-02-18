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
val created = Widget.Build(1)

class Widget { }

extension WidgetExtensions for Widget {
    public static Build(value: int) -> Widget {
        return Widget()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodDecl = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "Build");
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDecl));

        Assert.False(methodSymbol.IsExtensionMethod);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.ValueText == "Build");
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.True(SymbolEqualityComparer.Default.Equals(methodSymbol, boundInvocation.Method));
        Assert.Null(boundInvocation.ExtensionReceiver);
    }

    [Fact]
    public void StaticExtensionMethod_WildcardImport_DoesNotEnableUnqualifiedAccess()
    {
        const string source = """
import Widget.*
val created = Build(1)

class Widget { }

extension WidgetExtensions for Widget {
    public static Build(value: int) -> Widget {
        return Widget()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "Build");
        _ = Assert.IsType<BoundErrorExpression>(model.GetBoundNode(invocation));
    }

    [Fact]
    public void StaticExtensionMethod_PrefersRealStaticMembers()
    {
        const string source = """
val created = Widget.Build(1)

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
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.ValueText == "Build");
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("Widget", boundInvocation.Method.ContainingType?.Name);
    }

    [Fact]
    public void StaticExtensionMethod_InfersExtensionContainerTypeArguments()
    {
        const string source = """
val boxed = Box<int>.Wrap(1)

class Box<T> { }

extension BoxExtensions<T> for Box<T> {
    public static Wrap(value: T) -> Box<T> {
        return Box<T>()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.ValueText == "Wrap");
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundInvocation.Method.ContainingType);
        Assert.Equal("BoxExtensions", containingType.Name);

        var typeArgument = Assert.Single(containingType.TypeArguments);
        Assert.NotEqual(TypeKind.Error, typeArgument.TypeKind);
    }

    [Fact]
    public void StaticExtensionProperty_QualifiedLookup_BindsToExtensionContainer()
    {
        const string source = """
val total = Counter.Total

class Counter { }

extension CounterExtensions for Counter {
    public static Total: int {
        get { return 42; }
    }
}
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
