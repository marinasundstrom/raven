using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class DiscriminatedUnionSemanticTests : CompilationTestBase
{
    [Fact]
    public void GetDeclaredSymbol_ReturnsCaseSymbol()
    {
        const string source = """
union Option {
    None
    Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var caseClause = unionDecl.Cases[0];
        var symbol = model.GetDeclaredSymbol(caseClause);

        var caseSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionCaseSymbol>(symbol);
        Assert.Equal("None", caseSymbol.Name);
    }

    [Fact]
    public void MemberAccess_BindsToUnionCaseType()
    {
        const string source = """
union Option {
    None
    Some(value: int)
}

func create() {
    let option = Option.Some(value: 42)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>().Single();
        var symbol = model.GetSymbolInfo(memberAccess).Symbol;
        var caseType = Assert.IsAssignableFrom<ITypeSymbol>(symbol);
        Assert.Equal("Some", caseType.Name);
    }

    [Fact]
    public void CaseParameters_AreExposedAsGetterOnlyProperties()
    {
        const string source = """
union Option {
    Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = unionSymbol.Cases.Single();

        var property = caseSymbol.GetMembers("value").OfType<IPropertySymbol>().Single();
        Assert.Equal(Accessibility.Public, property.DeclaredAccessibility);
        Assert.NotNull(property.GetMethod);
        Assert.Null(property.SetMethod);
        Assert.Equal(SpecialType.System_Int32, property.Type.SpecialType);

        var backingField = caseSymbol.GetMembers().OfType<IFieldSymbol>()
            .Single(f => f.Name == "<value>k__BackingField");
        Assert.Equal(Accessibility.Private, backingField.DeclaredAccessibility);
    }

    [Fact]
    public void CaseToUnionConversion_ClassifiedAsDiscriminatedUnion()
    {
        const string source = """
union Option<T> {
    None
    Some(value: T)
}

class Container {
    Create() -> Option<int> {
        return Option.Some(value: 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = unionSymbol.Cases.Single(c => c.Name == "Some");

        var constructedUnion = (INamedTypeSymbol)unionSymbol.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
        var conversion = compilation.ClassifyConversion(caseSymbol, constructedUnion);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsDiscriminatedUnion);
    }

    [Fact]
    public void Lowerer_RewritesDiscriminatedUnionConversion()
    {
        const string source = """
union Option {
    None
    Some(value: int)
}

class Container {
    Create() -> Option {
        return Option.Some(value: 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        var boundBody = (BoundBlockStatement)model.GetBoundNode(methodSyntax.Body!)!;
        var returnStatement = boundBody.Statements.OfType<BoundReturnStatement>().Single();
        var castExpression = Assert.IsType<BoundCastExpression>(returnStatement.Expression);
        Assert.True(castExpression.Conversion.IsDiscriminatedUnion);

        var loweredBody = Lowerer.LowerBlock(methodSymbol, boundBody);
        var loweredReturn = loweredBody.Statements.OfType<BoundReturnStatement>().Single();
        var blockExpr = Assert.IsType<BoundBlockExpression>(loweredReturn.Expression);
        var statements = blockExpr.Statements.ToArray();
        Assert.Equal(5, statements.Length);

        var tagAssignment = Assert.IsType<BoundExpressionStatement>(statements[2]);
        var tagFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(tagAssignment.Expression);
        Assert.Equal("<Tag>", tagFieldAssignment.Field.Name);

        var payloadAssignment = Assert.IsType<BoundExpressionStatement>(statements[3]);
        var payloadFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(payloadAssignment.Expression);
        Assert.Equal("<Payload>", payloadFieldAssignment.Field.Name);

        var resultExpression = Assert.IsType<BoundExpressionStatement>(statements[4]);
        Assert.IsType<BoundLocalAccess>(resultExpression.Expression);
    }
}
