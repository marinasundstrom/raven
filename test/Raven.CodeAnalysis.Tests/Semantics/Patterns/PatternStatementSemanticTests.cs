using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class PatternStatementSemanticTests : CompilationTestBase
{
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
    case Some(value: T)
    case None
}

class C {
    func Test(input: Option<string>) {
        if val Option<string>.Some(value) = input {
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
    public void IfPatternStatement_WithGuardedBinding_BindsGuardedPatternAndLocal()
    {
        var code = """
class C {
    func Test(order: (int, int)) {
        if val (id, amount when amount > 100) = order {
            amount
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
        var tuplePattern = Assert.IsType<BoundPositionalPattern>(condition.Pattern);
        var guarded = Assert.IsType<BoundGuardedPattern>(tuplePattern.Elements[1]);
        var capture = Assert.IsType<BoundDeclarationPattern>(guarded.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(capture.Designator);

        designator.Local.Name.ShouldBe("amount");
        designator.Local.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
        guarded.GuardExpression.ShouldNotBeNull();
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
import Option.*

union Option<T> {
    case Some(value: T)
    case None
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
    public void WhilePatternStatement_WithNamedTypedTargetWithoutInlineBinding_ReportsDiagnostic()
    {
        var code = """
record class Person(Name: string, Age: int)

class C {
    func Test(person: Person) {
        while val (Name: name: string, Age: age: int) = person {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var allDiagnostics = compilation.GetDiagnostics();
        var diagnostics = allDiagnostics
            .Where(d => d.Id == CompilerDiagnostics.PatternTypedBindingRequiresKeyword.Id)
            .ToArray();

        allDiagnostics.Where(d => d.Id != CompilerDiagnostics.PatternTypedBindingRequiresKeyword.Id).ShouldBeEmpty();
        diagnostics.Length.ShouldBe(2);
        diagnostics.ShouldContain(d => d.GetMessage().Contains("name: string", StringComparison.Ordinal));
        diagnostics.ShouldContain(d => d.GetMessage().Contains("age: int", StringComparison.Ordinal));
    }

    [Fact]
    public void WhilePatternStatement_BindsAsWhileStatementWithPatternCondition()
    {
        var code = """
class C {
    func Test(input: (string, int)) {
        while val ("Ok", value) = input {
            value
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var whilePatternStmt = tree.GetRoot().DescendantNodes().OfType<WhilePatternStatementSyntax>().Single();
        var bound = Assert.IsType<BoundWhileStatement>(model.GetBoundNode(whilePatternStmt));
        var condition = Assert.IsType<BoundIsPatternExpression>(bound.Condition);
        var tuplePattern = Assert.IsType<BoundPositionalPattern>(condition.Pattern);
        var valuePattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[1]);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(valuePattern.Designator);

        bound.Body.ShouldBeOfType<BoundBlockStatement>();
        designator.Local.Name.ShouldBe("value");
        designator.Local.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
        designator.Local.IsMutable.ShouldBeFalse();
    }

    [Fact]
    public void WhilePatternStatement_WithUnionCasePattern_BindsCapturedLocal()
    {
        var code = """
union Result<T> {
    case Ok(value: T)
    case Done
}

class C {
    func Test(input: Result<int>) {
        while val .Ok(value) = input {
            value
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var whilePatternStmt = tree.GetRoot().DescendantNodes().OfType<WhilePatternStatementSyntax>().Single();
        var bound = Assert.IsType<BoundWhileStatement>(model.GetBoundNode(whilePatternStmt));
        var condition = Assert.IsType<BoundIsPatternExpression>(bound.Condition);
        var casePattern = Assert.IsType<BoundCasePattern>(condition.Pattern);
        var valuePattern = Assert.IsType<BoundDeclarationPattern>(casePattern.Arguments.Single());
        var designator = Assert.IsType<BoundSingleVariableDesignator>(valuePattern.Designator);

        casePattern.CaseSymbol.Name.ShouldBe("Ok");
        designator.Local.Name.ShouldBe("value");
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
}
