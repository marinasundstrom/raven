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
func create() {
    val option = Option.Some(value: 42)
}

union Option {
    None
    Some(value: int)
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
    public void MemberBindingInvocation_TargetTypedCase_BindsConstructor()
    {
        const string source = """
func build() {
    val option : Option = .Some(value: 42)
}

union Option {
    None
    Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundNode = model.GetBoundNode(invocation) ?? throw new InvalidOperationException("Missing bound invocation.");
        var creation = boundNode switch
        {
            BoundObjectCreationExpression objectCreation => objectCreation,
            BoundConversionExpression { Expression: BoundObjectCreationExpression innerCreation } => innerCreation,
            _ => throw new InvalidOperationException($"Unexpected bound node '{boundNode.GetType().Name}'.")
        };
        var constructor = creation.Constructor;
        Assert.Equal(MethodKind.Constructor, constructor.MethodKind);
        Assert.Equal("Some", constructor.ContainingType.Name);
        Assert.Equal("Option", constructor.ContainingType.ContainingType?.Name);
    }

    [Fact]
    public void AsyncReturn_TargetTypedCase_BindsUnionCase()
    {
        const string source = """
import System.Threading.Tasks.*

async func fetch() -> Task<Result<string, string>> {
    return .Ok(value: "done")
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberBinding = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<MemberBindingExpressionSyntax>()
            .Single();

        var symbol = model.GetSymbolInfo(memberBinding).Symbol;
        var caseType = Assert.IsAssignableFrom<ITypeSymbol>(symbol);
        Assert.Equal("Ok", caseType.Name);
    }

    [Fact]
    public void Union_DeclaresImplicitConversionPerCase()
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
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var conversionMethods = unionSymbol
            .GetMembers("op_Implicit")
            .OfType<IMethodSymbol>()
            .ToArray();

        Assert.Equal(unionSymbol.Cases.Length, conversionMethods.Length);

        foreach (var caseSymbol in unionSymbol.Cases)
        {
            var matchingConversion = conversionMethods.Single(m =>
                m.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type, caseSymbol));

            Assert.Equal(unionSymbol, matchingConversion.ReturnType);
            Assert.True(matchingConversion.IsStatic);
        }
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
    public void SynthesizedUnionMembers_AreRegisteredOnUnionAndCaseSymbols()
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

        var constructor = Assert.Single(caseSymbol.GetMembers(".ctor").OfType<IMethodSymbol>());
        Assert.Contains(unionSymbol.GetMembers(".ctor"), m => SymbolEqualityComparer.Default.Equals(m, constructor));

        var payloadProperty = Assert.Single(caseSymbol.GetMembers().OfType<IPropertySymbol>());
        Assert.Contains(unionSymbol.GetMembers(payloadProperty.Name), m => SymbolEqualityComparer.Default.Equals(m, payloadProperty));

        var getter = payloadProperty.GetMethod;
        Assert.NotNull(getter);
        Assert.Contains(unionSymbol.GetMembers(getter!.Name), m => SymbolEqualityComparer.Default.Equals(m, getter));

        var caseToString = Assert.Single(caseSymbol.GetMembers("ToString").OfType<IMethodSymbol>());
        Assert.Contains(unionSymbol.GetMembers("ToString"), m => SymbolEqualityComparer.Default.Equals(m, caseToString));

        var conversion = unionSymbol
            .GetMembers("op_Implicit")
            .OfType<IMethodSymbol>()
            .Single(m => SymbolEqualityComparer.Default.Equals(m.Parameters.Single().Type, caseSymbol));

        Assert.Contains(caseSymbol.GetMembers("op_Implicit"), m => SymbolEqualityComparer.Default.Equals(m, conversion));

        var tryGetName = $"TryGet{caseSymbol.Name}";
        var tryGet = unionSymbol.GetMembers(tryGetName).OfType<IMethodSymbol>().Single();
        Assert.Contains(caseSymbol.GetMembers(tryGetName), m => SymbolEqualityComparer.Default.Equals(m, tryGet));
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
        Assert.True(conversion.IsUserDefined);
        var method = Assert.IsAssignableFrom<IMethodSymbol>(conversion.MethodSymbol);
        Assert.Equal("op_Implicit", method.Name);
    }

    [Fact]
    public void Lowerer_PreservesDiscriminatedUnionConversion()
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
        var castExpression = Assert.IsType<BoundConversionExpression>(returnStatement.Expression);
        Assert.True(castExpression.Conversion.IsDiscriminatedUnion);
        Assert.True(castExpression.Conversion.IsUserDefined);
        Assert.NotNull(castExpression.Conversion.MethodSymbol);

        var loweredBody = Lowerer.LowerBlock(methodSymbol, boundBody);
        var loweredReturn = loweredBody.Statements.OfType<BoundReturnStatement>().Single();
        Assert.IsType<BoundConversionExpression>(loweredReturn.Expression);
    }

    [Fact]
    public void CasePattern_BindsPayloadType()
    {
        const string source = """
func format(result: Result<int, string>) -> string {
    return result match {
        .Ok(val payload) => payload.ToString()
        .Error(val message) => message
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var matchSyntax = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(matchSyntax));
        var okPattern = Assert.IsType<BoundCasePattern>(boundMatch.Arms[0].Pattern);
        var payloadPattern = Assert.IsType<BoundDeclarationPattern>(okPattern.Arguments[0]);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(payloadPattern.Designator);

        Assert.Equal(SpecialType.System_Int32, designator.Local.Type.SpecialType);
    }

    [Fact]
    public void CasePattern_ImplicitPayloadDesignations_BindLocals()
    {
        const string source = """
func format(result: Result<int, string>) -> string {
    return result match {
        .Ok(payload) => payload.ToString()
        .Error(message) => message
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var matchSyntax = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(matchSyntax));
        var okPayload = Assert.IsType<BoundDeclarationPattern>(Assert.IsType<BoundCasePattern>(boundMatch.Arms[0].Pattern).Arguments[0]);
        var errorPayload = Assert.IsType<BoundDeclarationPattern>(Assert.IsType<BoundCasePattern>(boundMatch.Arms[1].Pattern).Arguments[0]);

        Assert.Equal(SpecialType.System_Int32, okPayload.Designator.Type.SpecialType);
        Assert.Equal(SpecialType.System_String, errorPayload.Designator.Type.SpecialType);
    }

    [Fact]
    public void CasePattern_ImplicitPayloadDesignations_InMatch_BindLocals()
    {
        const string source = """
func describe(result: Result<int, string>) -> string {
    return result match {
        .Ok(payload) => payload.ToString()
        .Error(message) => message
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var matchSyntax = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(matchSyntax));

        var okPattern = Assert.IsType<BoundCasePattern>(boundMatch.Arms[0].Pattern);
        var okPayload = Assert.IsType<BoundDeclarationPattern>(okPattern.Arguments[0]);
        var okDesignator = Assert.IsType<BoundSingleVariableDesignator>(okPayload.Designator);

        Assert.Equal(SpecialType.System_Int32, okDesignator.Local.Type.SpecialType);
        Assert.Equal("payload", okDesignator.Local.Name);

        var errorPattern = Assert.IsType<BoundCasePattern>(boundMatch.Arms[1].Pattern);
        var errorPayload = Assert.IsType<BoundDeclarationPattern>(errorPattern.Arguments[0]);
        var errorDesignator = Assert.IsType<BoundSingleVariableDesignator>(errorPayload.Designator);

        Assert.Equal(SpecialType.System_String, errorDesignator.Local.Type.SpecialType);
        Assert.Equal("message", errorDesignator.Local.Name);
    }

    [Fact]
    public void CasePattern_NonGenericUnion_BindsTryGetMethods()
    {
        const string source = """
func describe(value: Test) -> string {
    return value match {
        .Something(text) => text
        .Nothing => "none"
    }
}

union Test {
    Something(value: string)
    Nothing
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var matchSyntax = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(matchSyntax));

        var somethingPattern = Assert.IsType<BoundCasePattern>(boundMatch.Arms[0].Pattern);
        Assert.Equal("TryGetSomething", somethingPattern.TryGetMethod.Name);

        var nothingPattern = Assert.IsType<BoundCasePattern>(boundMatch.Arms[1].Pattern);
        Assert.Equal("TryGetNothing", nothingPattern.TryGetMethod.Name);
    }

    [Fact]
    public void CasePattern_MissingArm_ReportsExhaustivenessDiagnostic()
    {
        const string source = """
func describe(result: Result<int, string>) -> string {
    return result match {
        .Ok(payload) => payload.ToString()
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV2100", diagnostic.Id);
        Assert.Contains("Error", diagnostic.GetMessage());

        var model = compilation.GetSemanticModel(tree);
        var matchSyntax = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        Assert.Equal(matchSyntax.MatchKeyword.GetLocation(), diagnostic.Location);
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(matchSyntax));

        var okPattern = Assert.IsType<BoundCasePattern>(boundMatch.Arms[0].Pattern);
        var okPayload = Assert.IsType<BoundDeclarationPattern>(okPattern.Arguments[0]);
        var okDesignator = Assert.IsType<BoundSingleVariableDesignator>(okPayload.Designator);

        Assert.Equal(SpecialType.System_Int32, okDesignator.Local.Type.SpecialType);
        Assert.Equal("payload", okDesignator.Local.Name);
    }

    [Fact]
    public void CasePattern_WithGuard_RemainsInExhaustivenessCheck()
    {
        const string source = """
func format(result: Result<int, string>) -> string {
    return result match {
        .Ok(val payload) => "ok ${payload}" when payload > 1
        .Error(val message) => "error ${message}"
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.MatchExpressionNotExhaustive, diagnostic.Descriptor);
    }

    [Fact]
    public void CasePattern_WithPartialArguments_IsNotExhaustive()
    {
        const string source = """
func area(shape: Shape) -> int {
    return shape match {
        .Circle(val r) => r * r * 3
        .Rectangle(4, val h) => 42
    }
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.MatchExpressionNotExhaustive, diagnostic.Descriptor);
        Assert.Contains("Rectangle", diagnostic.GetMessage());
    }

    [Fact]
    public void CasePattern_ReportsArgumentCountMismatch()
    {
        const string source = """
func format(result: Result<int, string>) -> string {
    return result match {
        .Ok() => "ok"
        _ => "none"
    }
}

union Result<T> {
    Ok(value: T)
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CasePatternArgumentCountMismatch, diagnostic.Descriptor);
    }
}
