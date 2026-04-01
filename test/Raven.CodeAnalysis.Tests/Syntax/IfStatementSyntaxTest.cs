using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class IfStatementSyntaxTest : DiagnosticTestBase
{
    [Fact]
    public void IfStatement()
    {
        string testCode =
            """
            if(x) {
            
            }
            """;

        var verifier = CreateVerifier(testCode, disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_WithElseClause()
    {
        string testCode =
            """
            if(x) {
            
            } else {
            
            }
            """;

        var verifier = CreateVerifier(testCode, disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_WithElseClause_WithIfStatement()
    {
        string testCode =
            """
            if(x) {
            
            } else if (y) {
            
            }
            """;

        var verifier = CreateVerifier(testCode, disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_WithoutBraces_ParsesWithoutBraceDiagnostic()
    {
        const string testCode = """
if x
    return 0
else
    return 1
""";

        var verifier = CreateVerifier(
            testCode,
            disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfPatternStatement_ParsesAsDedicatedNode()
    {
        const string testCode = """
if val (id, name) = person {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);

        ifBinding.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        ifBinding.Pattern.ShouldBeOfType<PositionalPatternSyntax>();
        ifBinding.Expression.ShouldBeOfType<IdentifierNameSyntax>();
    }

    [Fact]
    public void IfPatternStatement_WithRecursivePattern_ParsesNestedImplicitBindings()
    {
        const string testCode = """
if val Person(1, name, _) = person {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);
        var pattern = Assert.IsType<NominalDeconstructionPatternSyntax>(ifBinding.Pattern);
        var arguments = pattern.ArgumentList!.Arguments;

        arguments.Count.ShouldBe(3);
        arguments[1].ShouldBeOfType<VariablePatternSyntax>();
    }

    [Fact]
    public void IfPatternStatement_WithNestedTypedRecursivePattern_ParsesNestedNominalDeconstruction()
    {
        const string testCode = """
if val Error(ParseIntError(kind, _)) = value {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);
        var outerPattern = Assert.IsType<NominalDeconstructionPatternSyntax>(ifBinding.Pattern);
        var innerPattern = Assert.IsType<NominalDeconstructionPatternSyntax>(Assert.Single(outerPattern.ArgumentList!.Arguments));
        var innerArguments = innerPattern.ArgumentList!.Arguments;

        outerPattern.Type.ToString().ShouldBe("Error");
        innerPattern.Type.ToString().ShouldBe("ParseIntError");
        innerArguments.Count.ShouldBe(2);
        innerArguments[0].ShouldBeOfType<VariablePatternSyntax>();
        innerArguments[1].ShouldBeOfType<DiscardPatternSyntax>();
    }

    [Fact]
    public void IfPatternStatement_WithTypedImplicitBinding_ParsesVariablePattern()
    {
        const string testCode = """
if val x: int = input {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);
        var pattern = Assert.IsType<VariablePatternSyntax>(ifBinding.Pattern);
        var designation = Assert.IsType<TypedVariableDesignationSyntax>(pattern.Designation);
        var single = Assert.IsType<SingleVariableDesignationSyntax>(designation.Designation);

        ifBinding.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        pattern.BindingKeyword.Kind.ShouldBe(SyntaxKind.None);
        single.Identifier.ValueText.ShouldBe("x");
        designation.TypeAnnotation.Type.ToString().ShouldBe("int");
    }

    [Fact]
    public void IfPatternStatement_WithTrailingWholePatternDesignation_Parses()
    {
        const string testCode = """
if val (2, > 0.5) point = value {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);
        var pattern = Assert.IsType<PositionalPatternSyntax>(ifBinding.Pattern);
        var designation = Assert.IsType<SingleVariableDesignationSyntax>(pattern.Designation);

        ifBinding.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        designation.Identifier.ValueText.ShouldBe("point");
    }

    [Fact]
    public void IfPatternStatement_WithPropertyPattern_Parses()
    {
        const string testCode = """
if val Person { Name: "Ada", Age: age } = person {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);
        var pattern = Assert.IsType<PropertyPatternSyntax>(ifBinding.Pattern);
        var properties = pattern.PropertyPatternClause.Properties;

        ifBinding.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        pattern.Type.ShouldBeOfType<IdentifierNameSyntax>().Identifier.ValueText.ShouldBe("Person");
        properties.Count.ShouldBe(2);
        properties[0].NameColon.Name.Identifier.ValueText.ShouldBe("Name");
        properties[0].Pattern.ShouldBeOfType<ConstantPatternSyntax>();
        properties[1].NameColon.Name.Identifier.ValueText.ShouldBe("Age");
        properties[1].Pattern.ShouldBeOfType<VariablePatternSyntax>();
    }

    [Fact]
    public void IfPatternStatement_WithGuardedBinding_Parses()
    {
        const string testCode = """
if val (id, amount when > 100) = order {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);
        var pattern = Assert.IsType<PositionalPatternSyntax>(ifBinding.Pattern);
        var guarded = Assert.IsType<GuardedPatternSyntax>(pattern.Elements[1].Pattern);
        var variable = Assert.IsType<VariablePatternSyntax>(guarded.Pattern);

        ifBinding.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        variable.Designation.ShouldBeOfType<SingleVariableDesignationSyntax>().Identifier.ValueText.ShouldBe("amount");
        guarded.WhenClause.Guard.ShouldBeOfType<ComparisonPatternSyntax>();
    }
}
