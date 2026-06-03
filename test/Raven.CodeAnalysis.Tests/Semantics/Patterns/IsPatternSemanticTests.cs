using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis.Symbols;
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
val member: object = 0
val result = member is System.Reflection.MethodInfo method
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
    public void IsPattern_WithOpenGenericDeclarationType_InfersTypeArgumentsFromInput()
    {
        const string source = """
class Box<T> {}

func test(maybe: Box<int>) -> bool {
    if maybe is Box box {
        return true
    }

    return false
}
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);
        Assert.DoesNotContain(
            result.Compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var isPattern = tree.GetRoot()
            .DescendantNodes()
            .OfType<IsPatternExpressionSyntax>()
            .First();

        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));
        var declaration = Assert.IsType<BoundDeclarationPattern>(bound.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);

        var declaredDisplay = declaration.DeclaredType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var localDisplay = designator.Local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        Assert.Equal("Box<int>", declaredDisplay);
        Assert.Equal("Box<int>", localDisplay);
        Assert.Equal("box", designator.Local.Name);
        Assert.False(designator.Local.IsMutable);
    }

    [Fact]
    public void IsPattern_WithCompetingNonGenericType_InfersGenericWhenNonGenericIsNotPatternCompatible()
    {
        const string source = """
class Box {}
class Box<T> {}

func test(maybe: Box<int>) -> bool {
    if maybe is Box box {
        return true
    }

    return false
}
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);
        Assert.DoesNotContain(
            result.Compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var isPattern = tree.GetRoot()
            .DescendantNodes()
            .OfType<IsPatternExpressionSyntax>()
            .First();

        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));
        var declaration = Assert.IsType<BoundDeclarationPattern>(bound.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);

        var declaredDisplay = declaration.DeclaredType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var localDisplay = designator.Local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        Assert.Equal("Box<int>", declaredDisplay);
        Assert.Equal("Box<int>", localDisplay);
    }

    [Fact]
    public void IsPatternExpression_WithAndNotPropertyPatterns_BindsBothPropertyPatterns()
    {
        var code = """
class Box {
    var Flag: bool
}

class C {
    func Test(value: object) {
        if value is Box { Flag: true } and not Box { Flag: false } {
            ()
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var isPattern = tree.GetRoot().DescendantNodes().OfType<IsPatternExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));
        var andPattern = Assert.IsType<BoundAndPattern>(bound.Pattern);

        var left = Assert.IsType<BoundPropertyPattern>(andPattern.Left);
        var not = Assert.IsType<BoundNotPattern>(andPattern.Right);
        var right = Assert.IsType<BoundPropertyPattern>(not.Pattern);

        AssertBoxFlagProperty(left);
        AssertBoxFlagProperty(right);
    }

    [Fact]
    public void IsPropertyPattern_WithBareIdentifier_UsesExistingLocal()
    {
        var code = """
record class Person(Name: string)

class C {
    func Test(person: Person, name: string) {
        if person is { Name: name } {
            name.Length
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var parameter = root.DescendantNodes()
            .OfType<ParameterSyntax>()
            .Single(p => p.Identifier.ValueText == "name");
        var isPattern = root.DescendantNodes()
            .OfType<IsPatternExpressionSyntax>()
            .Single();

        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameter));
        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));
        var propertyPattern = Assert.IsType<BoundPropertyPattern>(bound.Pattern);
        var property = Assert.Single(propertyPattern.Properties);
        var constantPattern = Assert.IsType<BoundConstantPattern>(property.Pattern);
        var parameterAccess = Assert.IsType<BoundParameterAccess>(constantPattern.Expression);

        Assert.True(SymbolEqualityComparer.Default.Equals(parameterSymbol, parameterAccess.Parameter));
        Assert.Equal(SpecialType.System_String, parameterAccess.Type.SpecialType);
    }

    [Fact]
    public void IsNotPatternLocal_AfterEarlyReturn_BindsSubsequentReferences()
    {
        var code = """
class GuardedText {
    func Read(value: object) -> string {
        if value is not string text {
            return "not text"
        }

        val suffix = text + "!"
        return suffix
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var designation = root.DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(d => d.Identifier.ValueText == "text");
        var usage = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "text");
        var isPattern = root.DescendantNodes()
            .OfType<IsPatternExpressionSyntax>()
            .Single();

        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));
        var notPattern = Assert.IsType<BoundNotPattern>(bound.Pattern);
        var declarationPattern = Assert.IsType<BoundDeclarationPattern>(notPattern.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declarationPattern.Designator);

        var declared = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designation));
        var referenced = Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(usage).Symbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(declared, designator.Local));
        Assert.True(SymbolEqualityComparer.Default.Equals(declared, referenced));
        Assert.Equal(SpecialType.System_String, declared.Type.SpecialType);
        Assert.False(declared.IsMutable);
    }

    [Fact]
    public void IsPatternLocal_InIfBody_BindsDeclaredLocal()
    {
        var code = """
class C {
    func Test(value: object) {
        if value is int amount {
            amount
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var designation = root.DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(d => d.Identifier.ValueText == "amount");
        var usage = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "amount");
        var isPattern = root.DescendantNodes()
            .OfType<IsPatternExpressionSyntax>()
            .Single();

        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));
        var declarationPattern = Assert.IsType<BoundDeclarationPattern>(bound.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declarationPattern.Designator);
        var declared = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designation));
        var referenced = Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(usage).Symbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(declared, designator.Local));
        Assert.True(SymbolEqualityComparer.Default.Equals(declared, referenced));
        Assert.Equal(SpecialType.System_Int32, declared.Type.SpecialType);
        Assert.False(declared.IsMutable);
    }

    private static void AssertBoxFlagProperty(BoundPropertyPattern pattern)
    {
        Assert.Equal("Box", pattern.ReceiverType.Name);
        var subpattern = Assert.Single(pattern.Properties);
        Assert.Equal("Flag", subpattern.Member.Name);
        Assert.IsAssignableFrom<IPropertySymbol>(subpattern.Member);
        Assert.IsType<BoundConstantPattern>(subpattern.Pattern);
    }
}
