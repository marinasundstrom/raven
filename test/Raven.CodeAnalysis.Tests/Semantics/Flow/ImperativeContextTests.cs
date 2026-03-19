using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ImperativeContextTests : CompilationTestBase
{
    [Fact]
    public void NestedStatementBlockExpressionStatement_DoesNotRequireMethodReturnType()
    {
        var code = """
class C {
    func Test(flag: bool, action: () -> ()) -> int {
        if flag {
            action()
        }
        return 1
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Id == "RAV1503");
    }

    [Fact]
    public void NestedStatementBlockInMethodReturningSequence_DoesNotRequireMethodReturnType()
    {
        var code = """
import System.Collections.Generic.*

class C {
    func Test(flag: bool, list: List<int>) -> IEnumerable<int> {
        if flag {
            list.Add(1)
        }
        return list
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Id == "RAV1503");
    }

    [Fact]
    public void IfStatement_BindsAsStatement()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        } else {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ifStmt = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var bound = model.GetBoundNode(ifStmt);

        Assert.IsType<BoundIfStatement>(bound);
    }

    [Fact]
    public void IfStatement_BranchesCanBeExpressions()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        } else ()
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ifStmt = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var bound = (BoundIfStatement)model.GetBoundNode(ifStmt);

        Assert.IsType<BoundBlockStatement>(bound.ThenNode);
        Assert.IsType<BoundExpressionStatement>(bound.ElseNode);
        Assert.IsType<BoundUnitExpression>(((BoundExpressionStatement)bound.ElseNode!).Expression);
    }

    [Fact]
    public void IfStatement_BranchesCanBeStatements()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag return else return
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ifStmt = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var bound = (BoundIfStatement)model.GetBoundNode(ifStmt);

        Assert.IsType<BoundReturnStatement>(bound.ThenNode);
        Assert.IsType<BoundReturnStatement>(bound.ElseNode);
    }

    [Fact]
    public void IfPatternStatement_BindsAsIfStatementWithPatternCondition()
    {
        var code = """
class Person(val Id: int, val Name: string)

class C {
    func Test(person: Person) {
        if val Person(id, name) = person {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ifPatternStmt = tree.GetRoot().DescendantNodes().OfType<IfPatternStatementSyntax>().First();
        var bound = (BoundIfStatement)model.GetBoundNode(ifPatternStmt);

        bound.Condition.ShouldBeOfType<BoundIsPatternExpression>();
        bound.ThenNode.ShouldBeOfType<BoundBlockStatement>();
    }

    [Fact]
    public void IfPatternStatement_PatternLocalShadowing_ReportsConsistentDiagnostic()
    {
        var code = """
class Person(val Id: int, val Name: string)

class C {
    func Test(person: Person) {
        val name = ""
        if val Person(_, name) = person {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        diagnostics.ShouldContain(d => d.Id == CompilerDiagnostics.VariableShadowsPreviousDeclaration.Id);
    }

    [Fact]
    public void IfPatternStatement_WithTypedImplicitBinding_BindsDeclarationPattern()
    {
        var code = """
class C {
    func Test(input: int?) {
        if val x: int = input {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var ifPatternStmt = tree.GetRoot().DescendantNodes().OfType<IfPatternStatementSyntax>().First();
        var bound = Assert.IsType<BoundIfStatement>(model.GetBoundNode(ifPatternStmt));
        var condition = Assert.IsType<BoundIsPatternExpression>(bound.Condition);
        var pattern = Assert.IsType<BoundDeclarationPattern>(condition.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(pattern.Designator);

        pattern.DeclaredType.SpecialType.ShouldBe(SpecialType.System_Int32);
        designator.Local.Name.ShouldBe("x");
        designator.Local.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
        designator.Local.IsMutable.ShouldBeFalse();
    }

    [Fact]
    public void IfPatternStatement_WithOuterValNominalPattern_BindsImplicitCaptures()
    {
        var code = """
union Option<T> {
    Some(value: T)
    None
}

class C {
    func Test(input: Option<string>) {
        if val Some(value) = input {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var ifPatternStmt = tree.GetRoot().DescendantNodes().OfType<IfPatternStatementSyntax>().First();
        var bound = Assert.IsType<BoundIfStatement>(model.GetBoundNode(ifPatternStmt));
        var condition = Assert.IsType<BoundIsPatternExpression>(bound.Condition);
        var pattern = Assert.IsType<BoundCasePattern>(condition.Pattern);
        var argument = Assert.IsType<BoundDeclarationPattern>(pattern.Arguments.Single());
        var designator = Assert.IsType<BoundSingleVariableDesignator>(argument.Designator);

        designator.Local.Name.ShouldBe("value");
        designator.Local.Type.SpecialType.ShouldBe(SpecialType.System_String);
    }

    [Fact]
    public void IfPatternStatement_WithTypedHierarchyBinding_BindsNarrowedType()
    {
        var code = """
open class Animal {}
class Dog : Animal {}

class C {
    func Test(animal: Animal) {
        if val dog: Dog = animal {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var ifPatternStmt = tree.GetRoot().DescendantNodes().OfType<IfPatternStatementSyntax>().First();
        var bound = Assert.IsType<BoundIfStatement>(model.GetBoundNode(ifPatternStmt));
        var condition = Assert.IsType<BoundIsPatternExpression>(bound.Condition);
        var pattern = Assert.IsType<BoundDeclarationPattern>(condition.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(pattern.Designator);

        pattern.DeclaredType.Name.ShouldBe("Dog");
        designator.Local.Name.ShouldBe("dog");
        designator.Local.Type.Name.ShouldBe("Dog");
        designator.Local.IsMutable.ShouldBeFalse();
    }

    [Fact]
    public void IfPatternStatement_WithTrailingWholePatternDesignation_BindsMatchedValue()
    {
        var code = """
class C {
    func Test(point: (int, double)) {
        if val (2, > 0.5) matched = point {
            matched.Item1
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var designation = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(d => d.Identifier.ValueText == "matched");

        var symbol = model.GetDeclaredSymbol(designation).ShouldBeOfType<SourceLocalSymbol>();
        symbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("(int, double)");
        symbol.IsMutable.ShouldBeFalse();
    }

    [Fact]
    public void IfPatternStatement_WithComparisonPatternOfDifferentType_ReportsDiagnostic()
    {
        var code = """
class C {
    func Test(point: (int, int)) {
        if val (2, > 0.5) matched = point {
            matched.Item1
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        diagnostics.ShouldContain(d => d.Id == "RAV1606");
    }

    [Fact]
    public void IfPatternStatement_WithNestedCaseNominalSequenceAndWholeDesignation_BindsAllLocals()
    {
        var code = """
union Option<T> {
    Some(value: T)
    None
}

class C {
    func Test(input: Option<(string, int)>) {
        if val Some((first, >= 18)) whole = input {
            first.Length
            whole
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);

        var first = model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<SingleVariableDesignationSyntax>().Single(d => d.Identifier.ValueText == "first")).ShouldBeOfType<SourceLocalSymbol>();
        var whole = model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<SingleVariableDesignationSyntax>().Single(d => d.Identifier.ValueText == "whole")).ShouldBeOfType<SourceLocalSymbol>();

        first.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
        whole.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("Some<(string, int)>");
    }

    [Fact]
    public void IfPatternStatement_WithPropertyPattern_BindsImplicitCaptures()
    {
        var code = """
record class Person(Name: string, Age: int)

class C {
    func Test(value: object) {
        if val Person { Name: "Ada", Age: age } = value {
            age
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var ifPatternStmt = tree.GetRoot().DescendantNodes().OfType<IfPatternStatementSyntax>().First();
        var bound = Assert.IsType<BoundIfStatement>(model.GetBoundNode(ifPatternStmt));
        var condition = Assert.IsType<BoundIsPatternExpression>(bound.Condition);
        var pattern = Assert.IsType<BoundPropertyPattern>(condition.Pattern);
        var property = Assert.Single(pattern.Properties.Where(p => p.Member.Name == "Age"));
        var capture = Assert.IsType<BoundDeclarationPattern>(property.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(capture.Designator);

        pattern.ReceiverType.Name.ShouldBe("Person");
        designator.Local.Name.ShouldBe("age");
        designator.Local.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
        designator.Local.IsMutable.ShouldBeFalse();
    }

    [Fact]
    public void IfPatternStatement_WithDuplicatePropertyPatternMembers_ReportsDiagnostic()
    {
        var code = """
class Box {
    val Value: int
}

class C {
    func Test(value: object) {
        if val Box { Value: 1, Value: other } = value {
            other
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        diagnostics.ShouldContain(d => d.Id == CompilerDiagnostics.DuplicatePropertyPatternMember.Id);
    }

    [Fact]
    public void WhileStatement_BindsAsStatement()
    {
        var code = """
class C {
    func Test(flag: bool) {
        while flag {
            break
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var whileStmt = tree.GetRoot().DescendantNodes().OfType<WhileStatementSyntax>().First();
        var bound = model.GetBoundNode(whileStmt);

        Assert.IsType<BoundWhileStatement>(bound);
    }

    [Fact]
    public void ForStatement_BindsAsStatement()
    {
        var code = """
class C {
    func Test(items: int[]) {
        for item in items {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var forStmt = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().First();
        var bound = model.GetBoundNode(forStmt);

        Assert.IsType<BoundForStatement>(bound);
    }

    [Fact]
    public void BlockExpression_InExpressionStatement_BindsAsStatement()
    {
        var code = """
class C {
    func Test() {
        {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var blockStmt = tree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().Skip(1).First();
        var bound = model.GetBoundNode(blockStmt);

        Assert.IsType<BoundBlockStatement>(bound);
    }
}
