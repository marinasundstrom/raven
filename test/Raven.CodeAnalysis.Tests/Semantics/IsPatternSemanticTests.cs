using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class IsPatternSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void IsPattern_WithQualifiedMetadataType_BindsDeclaredType()
    {
        const string source = """
let member: object = 0
let result = member is System.Reflection.MethodInfo method
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var isPattern = tree.GetRoot()
            .DescendantNodes()
            .OfType<IsPatternExpressionSyntax>()
            .Single();

        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));
        var declaration = Assert.IsType<BoundDeclarationPattern>(bound.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);

        var methodInfoType = result.Compilation.GetTypeByMetadataName("System.Reflection.MethodInfo");
        Assert.NotNull(methodInfoType);

        var declaredDisplay = declaration.DeclaredType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        var expectedDisplay = methodInfoType!.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

        Assert.Equal(expectedDisplay, declaredDisplay); // ensure diagnostic clarity
        Assert.Equal("method", designator.Local.Name);
        Assert.False(designator.Local.IsMutable);

        var declaredAssembly = declaration.DeclaredType.ContainingAssembly;
        Assert.NotNull(declaredAssembly);

        var expectedType = declaredAssembly!.GetTypeByMetadataName("System.Reflection.MethodInfo");
        Assert.NotNull(expectedType);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedType, declaration.DeclaredType),
            declaredDisplay);

        var localAssembly = designator.Local.Type.ContainingAssembly;
        Assert.NotNull(localAssembly);
        Assert.Equal(declaredAssembly, localAssembly);
    }

    [Fact]
    public void IsPattern_WithUnionCasePattern_BindsPayload()
    {
        const string code = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

let value: Result<int> = Result<int>.Ok(value: 5)
let isOk = value is .Ok(let payload)
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var isPattern = tree.GetRoot().DescendantNodes().OfType<IsPatternExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));

        var unionPattern = Assert.IsType<BoundUnionCasePattern>(bound.Pattern);
        Assert.Equal("Ok", unionPattern.CaseType.Name);
        var argument = Assert.Single(unionPattern.Arguments);
        var declaration = Assert.IsType<BoundDeclarationPattern>(argument);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);
        Assert.Equal("payload", designator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, designator.Local.Type.SpecialType);
    }
}
