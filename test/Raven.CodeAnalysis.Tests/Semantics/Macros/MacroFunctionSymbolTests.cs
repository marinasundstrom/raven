using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class MacroFunctionSymbolTests : CompilationTestBase
{
    [Fact]
    public void AuthoredMacroPosition_UsesSignatureSemanticModelForParameterAndBody()
    {
        const string source = """
            macro func Identity(value: int) {
                expand value
            }

            func Main() -> int => #Identity(42)
            """;
        var authoredTree = SyntaxTree.ParseText(SourceText.From(source), path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroFunctionConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(authoredTree);
        var bodyOffset = source.IndexOf("expand value", StringComparison.Ordinal) + "expand ".Length;

        var model = compilation.GetSemanticModel(authoredTree, bodyOffset);
        var declaration = model.SyntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();
        var parameterSyntax = declaration.ParameterList.Parameters.Single();
        var bodyReference = declaration.Body!.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "value");

        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameterSyntax));
        Assert.Same(parameter, model.GetSymbolInfo(bodyReference).Symbol);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void MacroFunctionDeclaration_DeclaresDistinctMacroFunctionSymbol()
    {
        var (compilation, tree) = CreateCompilation("""
            macro func Identity<T>(value: T) -> T
                where T: System.IDisposable
            {
                return value
            }
            """);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();

        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            model.GetDeclaredSymbol(declaration));

        Assert.Equal(SymbolKind.MacroFunction, symbol.Kind);
        Assert.Equal(MacroKind.FreestandingExpression, symbol.MacroKind);
        Assert.Equal(MacroTarget.None, symbol.Targets);
        Assert.Null(symbol.TargetName);
        Assert.Equal("Identity", symbol.Name);
        Assert.True(symbol.IsStatic);
        Assert.True(symbol.CanBeReferencedByName);
        Assert.False(symbol is IMethodSymbol);

        var typeParameter = Assert.Single(symbol.TypeParameters);
        Assert.Equal(TypeParameterOwnerKind.MacroFunction, typeParameter.OwnerKind);
        Assert.Same(symbol, typeParameter.DeclaringMacroFunctionParameterOwner);
        Assert.Null(typeParameter.DeclaringMethodParameterOwner);
        Assert.Null(typeParameter.DeclaringTypeParameterOwner);
        Assert.Equal(TypeParameterConstraintKind.TypeConstraint, typeParameter.ConstraintKind);
        Assert.Equal("IDisposable", Assert.Single(typeParameter.ConstraintTypes).Name);
        Assert.Same(typeParameter, symbol.ReturnType);

        var parameter = Assert.Single(symbol.Parameters);
        Assert.Equal("value", parameter.Name);
        Assert.Equal(MacroParameterRole.Value, parameter.MacroRole);
        Assert.Same(symbol, parameter.ContainingSymbol);
        Assert.Same(typeParameter, parameter.Type);
        Assert.Same(parameter, model.GetDeclaredSymbol(declaration.ParameterList.Parameters.Single()));

        Assert.Contains(
            compilation.GlobalNamespace.GetMembers("Identity"),
            candidate => ReferenceEquals(candidate, symbol));
        Assert.DoesNotContain(
            compilation.GlobalNamespace.GetMembers("Identity"),
            candidate => candidate is IMethodSymbol);

        var classifications = SemanticClassifier.Classify(tree.GetRoot(), model);
        Assert.Equal(
            SemanticClassification.Method,
            classifications.Tokens[declaration.Identifier]);
        Assert.Equal(
            SemanticClassification.Parameter,
            classifications.Tokens[declaration.ParameterList.Parameters.Single().Identifier]);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroFunctionDeclaration_UsesItsNamespaceAsSemanticContainer()
    {
        var (compilation, tree) = CreateCompilation("""
            namespace Tools {
                macro func Quote(value: int) -> int => value
            }
            """);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();

        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            model.GetDeclaredSymbol(declaration));

        Assert.Equal("Tools", symbol.ContainingNamespace?.ToDisplayString());
        Assert.Same(symbol.ContainingNamespace, symbol.ContainingSymbol);
        Assert.Null(symbol.ContainingType);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroFunctionDeclaration_RejectsAsyncAndAwait()
    {
        var (compilation, _) = CreateCompilation("""
            async macro func Fetch(task: System.Threading.Tasks.Task<int>) -> int {
                return await task
            }
            """);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0925");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0926");
    }

    [Fact]
    public void AttachedMacroFunction_ExposesTargetSemantics()
    {
        var (compilation, tree) = CreateCompilation("""
            macro func Observable() on property: Property {
                replace property
                introduce CreateBackingField(property)
            }
            """);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(MacroKind.AttachedDeclaration, symbol.MacroKind);
        Assert.Equal(MacroTarget.Property, symbol.Targets);
        Assert.Equal("property", symbol.TargetName);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroContribution_ValidatesAgainstAttachmentKind()
    {
        var (compilation, _) = CreateCompilation("""
            macro func Freestanding() {
                replace quote! { 1 }
            }

            macro func Attached() on Type {
                expand quote! { 1 }
            }
            """);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal(2, diagnostics.Count(static diagnostic => diagnostic.Id == "RAV0928"));
    }

    [Fact]
    public void TokenStreamMacroFunction_ExposesTypeDirectedParameterRoles()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            import Raven.CodeAnalysis.Macros.*

            macro func Query(dialect: string, tokens: Raven.CodeAnalysis.Macros.IMacroTokenStream) {
                expand ParseQuery(dialect, tokens)
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location));
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(MacroKind.FreestandingExpression, symbol.MacroKind);
        Assert.Equal(MacroTarget.None, symbol.Targets);
        Assert.Equal(MacroParameterRole.Value, symbol.Parameters[0].MacroRole);
        Assert.Equal(MacroParameterRole.TokenStream, symbol.Parameters[1].MacroRole);
        Assert.Equal("IMacroTokenStream", symbol.Parameters[1].Type.Name);
        Assert.Contains("tokens: IMacroTokenStream", symbol.ToDisplayString());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ExpressionMacroFunction_ExposesSyntaxProjectionRole()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            import Raven.CodeAnalysis.Syntax.*

            macro func Rewrite(value: Raven.CodeAnalysis.Syntax.ExpressionSyntax) {
                expand value
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location));
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        var parameter = Assert.Single(symbol.Parameters);

        Assert.Equal(MacroParameterRole.ExpressionSyntax, parameter.MacroRole);
        Assert.Equal("ExpressionSyntax", parameter.Type.Name);
        Assert.Contains("value: ExpressionSyntax", symbol.ToDisplayString());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroParameterRole_UsesResolvedTypeIdentity()
    {
        var (compilation, tree) = CreateCompilation("""
            class ExpressionSyntax {}

            macro func Custom(value: ExpressionSyntax) {
                expand value
            }
            """);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();
        var parameter = Assert.Single(
            Assert.IsAssignableFrom<IMacroFunctionSymbol>(
                compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration))
            .Parameters);

        Assert.Equal(MacroParameterRole.Value, parameter.MacroRole);
        Assert.Equal("ExpressionSyntax", parameter.Type.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void TokenStreamMacroParameter_ValidatesSpecialBindingRules()
    {
        var (baseCompilation, _) = CreateCompilation("""
            import Raven.CodeAnalysis.Macros.*
            import Raven.CodeAnalysis.Syntax.*

            macro func Defaulted(content: Raven.CodeAnalysis.Macros.IMacroTokenStream = null) {
                expand content
            }

            macro func Duplicate(first: Raven.CodeAnalysis.Macros.IMacroTokenStream, second: Raven.CodeAnalysis.Macros.IMacroTokenStream) {
                expand first
            }

            macro func Attached(tokens: Raven.CodeAnalysis.Macros.IMacroTokenStream) on Type {
                introduce tokens.ReadToken()
            }

            macro func DefaultedExpression(value: Raven.CodeAnalysis.Syntax.ExpressionSyntax = 1) {
                expand value
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location));

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0929");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0930");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0931");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0932");
    }
}
