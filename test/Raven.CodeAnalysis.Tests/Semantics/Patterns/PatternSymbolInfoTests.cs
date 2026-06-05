using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class PatternSymbolInfoTests : CompilationTestBase
{
    [Fact]
    public void GetSymbolInfo_MemberPatternPathInMatchExpression_ResolvesUnionCase()
    {
        const string source = """
union VehicleStatus {
    case Operational(driverName: string)
    case Maintenance(reason: string)
}

func StatusMatches(status: VehicleStatus) -> bool {
    return status match {
        .Operational(_) => true
        _ => false
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var memberPatternPath = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberPatternPathSyntax>()
            .Single(path => path.Identifier.ValueText == "Operational");

        var caseSymbol = Assert.IsAssignableFrom<IUnionCaseTypeSymbol>(model.GetSymbolInfo(memberPatternPath).Symbol);
        Assert.Equal("Operational", caseSymbol.Name);
        Assert.Equal("VehicleStatus", caseSymbol.Union.Name);
    }

    [Fact]
    public void GetSymbolInfo_NominalUnionCasePattern_ProjectsGenericCaseFromScrutineeType()
    {
        const string source = """
import Outcome.*

val result: Outcome<int, string> = Ok(2)

match result {
    Ok(val value) => value
    Error(_) => 0
}

union Outcome<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                          diagnostic.Id != CompilerDiagnostics.MatchExpressionNotExhaustive.Id);

        var model = compilation.GetSemanticModel(tree);
        var incompleteErrorPatternName = tree.GetRoot()
            .DescendantNodes()
            .OfType<NominalDeconstructionPatternSyntax>()
            .Select(pattern => Assert.IsType<IdentifierNameSyntax>(pattern.Type))
            .Last(name => name.Identifier.ValueText == "Error");
        var incompleteErrorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(incompleteErrorPatternName).Symbol);
        Assert.Equal("Error", incompleteErrorCase.Name);
        Assert.Equal(SpecialType.System_String, incompleteErrorCase.TypeArguments.Single().SpecialType);

        var patternNames = tree.GetRoot()
            .DescendantNodes()
            .OfType<NominalDeconstructionPatternSyntax>()
            .Select(pattern => Assert.IsType<IdentifierNameSyntax>(pattern.Type))
            .ToDictionary(name => name.Identifier.ValueText);

        var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(patternNames["Ok"]).Symbol);
        var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(patternNames["Error"]).Symbol);

        Assert.Equal("Ok", okCase.Name);
        Assert.Equal(SpecialType.System_Int32, okCase.TypeArguments.Single().SpecialType);
        Assert.Equal("Error", errorCase.Name);
        Assert.Equal(SpecialType.System_String, errorCase.TypeArguments.Single().SpecialType);
    }

    [Fact]
    public void GetSymbolInfo_MetadataResultCasePattern_ProjectsGenericCaseFromScrutineeType()
    {
        const string source = """
import System.Result.*
import System.*

val ok: Result<int, string> = Ok(2)

match ok {
    Ok(val value) => {}
    Error(_) => {}
}

class C {
    func Test() {
        val localOk: Result<int, string> = Ok(2)

        match localOk {
            Ok(_) => {}
            Error(_) => {}
        }

        match localOk {
            Error(_) => {}
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                          diagnostic.Id != CompilerDiagnostics.MatchExpressionNotExhaustive.Id);

        var model = compilation.GetSemanticModel(tree);
        var patternNames = tree.GetRoot()
            .DescendantNodes()
            .OfType<NominalDeconstructionPatternSyntax>()
            .Select(pattern => Assert.IsType<IdentifierNameSyntax>(pattern.Type))
            .GroupBy(name => name.Identifier.ValueText)
            .ToDictionary(group => group.Key, group => group.ToArray());

        foreach (var patternName in patternNames["Ok"])
        {
            var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(patternName).Symbol);
            Assert.Equal("Ok", okCase.Name);
            Assert.Equal(SpecialType.System_Int32, okCase.TypeArguments.Single().SpecialType);
        }

        foreach (var patternName in patternNames["Error"])
        {
            var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(patternName).Symbol);
            Assert.Equal("Error", errorCase.Name);
            Assert.Equal(SpecialType.System_String, errorCase.TypeArguments.Single().SpecialType);
        }
    }

    [Fact]
    public void GetSymbolInfo_MetadataResultCasePattern_InClassOnlyCompilation_ProjectsGenericCaseFromScrutineeType()
    {
        const string source = """
import System.Result.*
import System.*

class C {
    func Test() {
        val ok: Result<int, string> = Ok(2)

        match ok {
            Error(_) => {}
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                          diagnostic.Id != CompilerDiagnostics.MatchExpressionNotExhaustive.Id);

        var model = compilation.GetSemanticModel(tree);
        var matchStatement = tree.GetRoot()
            .DescendantNodes()
            .OfType<MatchStatementSyntax>()
            .Single();
        var scrutineeSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(matchStatement.Expression).Symbol);
        Assert.Equal("Result<int, string>", scrutineeSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var errorPatternName = tree.GetRoot()
            .DescendantNodes()
            .OfType<NominalDeconstructionPatternSyntax>()
            .Select(pattern => Assert.IsType<IdentifierNameSyntax>(pattern.Type))
            .Single(name => name.Identifier.ValueText == "Error");

        var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(errorPatternName).Symbol);
        Assert.Equal("Error", errorCase.Name);
        Assert.Equal(SpecialType.System_String, errorCase.TypeArguments.Single().SpecialType);
    }

    [Fact]
    public void GetSymbolInfo_OpenGenericTypePattern_ProjectsFromScrutineeType()
    {
        const string source = """
val value: Box<int> = Box<int>()

if value is Box box {
}

class Box<T> {}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var patternType = tree.GetRoot()
            .DescendantNodes()
            .OfType<DeclarationPatternSyntax>()
            .Select(pattern => Assert.IsType<IdentifierNameSyntax>(pattern.Type))
            .Single(identifier => identifier.Identifier.ValueText == "Box");

        var boxType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(patternType).Symbol);
        Assert.Equal("Box", boxType.Name);
        Assert.Equal(SpecialType.System_Int32, boxType.TypeArguments.Single().SpecialType);
    }

    [Fact]
    public void GetSymbolInfo_QualifiedEnumConstantInIsPattern_ResolvesEnumMember()
    {
        const string source = """
import System.Text.Json.*

func Test(element: JsonElement) -> bool {
    return element.ValueKind is JsonValueKind.Array
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error && diagnostic.Id != "RAV1014");

        var model = compilation.GetSemanticModel(tree);
        var memberName = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "Array");

        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(memberName).Symbol);
        Assert.Equal("Array", field.Name);
        Assert.Equal("JsonValueKind", field.ContainingType.Name);
        Assert.Equal(TypeKind.Enum, field.ContainingType.TypeKind);
    }
}
