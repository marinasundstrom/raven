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
    public void AuthoredConsumerPosition_BindsLocalReferenceAfterMacroInvocation()
    {
        const string source = """
            macro func Double(value: int) {
                let doubled = value * 2
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(doubled.ToString())
            }

            func Main() {
                let answer = Double!(21)
                System.Console.WriteLine(answer)
            }
            """;
        var authoredTree = SyntaxTree.ParseText(SourceText.From(source), path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroFunctionConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(authoredTree);
        var referenceOffset = source.LastIndexOf("answer", StringComparison.Ordinal);

        var model = compilation.GetSemanticModel(authoredTree, referenceOffset);
        var answerIdentifiers = model.SyntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(identifier => identifier.Identifier.ValueText == "answer")
            .ToArray();
        var reference = Assert.Single(answerIdentifiers);

        var symbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(reference).Symbol);

        Assert.Equal("answer", symbol.Name);
        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
    }

    [Fact]
    public void AuthoredMacroPosition_BindsParameterLocalsAndMemberInvocations()
    {
        const string source = """
            import Raven.CodeAnalysis.Syntax.*

            macro func Double(value: int) {
                let doubled = value * 2
                let text = doubled.ToString()
                expand SyntaxFactory.ParseExpression(text)
            }

            func Main() -> int => #Double(21)
            """;
        var authoredTree = SyntaxTree.ParseText(SourceText.From(source), path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroFunctionConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(authoredTree);
        var bodyOffset = source.IndexOf("let doubled", StringComparison.Ordinal);

        var model = compilation.GetSemanticModel(authoredTree, bodyOffset);
        var declaration = model.SyntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();
        var parameterSyntax = declaration.ParameterList.Parameters.Single();
        var bodyIdentifiers = declaration.Body!.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .ToArray();
        var parameterReference = bodyIdentifiers
            .Single(identifier => identifier.Identifier.ValueText == "value");
        var doubledReference = bodyIdentifiers
            .Single(identifier => identifier.Identifier.ValueText == "doubled");
        var textReference = bodyIdentifiers
            .Single(identifier => identifier.Identifier.ValueText == "text");
        var toStringReference = bodyIdentifiers
            .Single(identifier => identifier.Identifier.ValueText == "ToString");
        var parseExpressionReference = bodyIdentifiers
            .Single(identifier => identifier.Identifier.ValueText == "ParseExpression");
        var declarators = declaration.Body.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .ToDictionary(declarator => declarator.Identifier.ValueText);

        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameterSyntax));
        Assert.Same(parameter, model.GetSymbolInfo(parameterReference).Symbol);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);

        var doubled = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators["doubled"]));
        Assert.Same(doubled, model.GetSymbolInfo(doubledReference).Symbol);
        Assert.Equal(SpecialType.System_Int32, doubled.Type.SpecialType);

        var text = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators["text"]));
        Assert.Same(text, model.GetSymbolInfo(textReference).Symbol);
        Assert.Equal(SpecialType.System_String, text.Type.SpecialType);

        Assert.Equal("ToString", Assert.IsAssignableFrom<IMethodSymbol>(
            model.GetSymbolInfo(toStringReference).Symbol).Name);
        Assert.Equal("ParseExpression", Assert.IsAssignableFrom<IMethodSymbol>(
            model.GetSymbolInfo(parseExpressionReference).Symbol).Name);
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
        var (baseCompilation, tree) = CreateCompilation("""
            macro func Observable() on property: Property {
                replace property
                introduce property
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(PropertyDeclarationSyntax).Assembly.Location));
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();
        var model = compilation.GetSemanticModel(tree);
        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            model.GetDeclaredSymbol(declaration));

        Assert.Equal(MacroKind.AttachedDeclaration, symbol.MacroKind);
        Assert.Equal(MacroTarget.Property, symbol.Targets);
        Assert.Equal("property", symbol.TargetName);
        var targetParameter = Assert.IsAssignableFrom<IParameterSymbol>(symbol.TargetParameter);
        Assert.Equal("PropertyDeclarationSyntax", targetParameter.Type.Name);
        Assert.Equal(
            declaration.TargetClause!.Identifier.Span,
            Assert.Single(targetParameter.Locations).SourceSpan);
        Assert.All(
            declaration.Body!.DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Where(static identifier => identifier.Identifier.ValueText == "property"),
            identifier => Assert.Same(targetParameter, model.GetSymbolInfo(identifier).Symbol));
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
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("0")
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
    public void ArgumentStyleMacroFunction_ExposesCompilerSuppliedFreestandingContextRole()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            macro func Embed(
                path: string,
                context: Raven.CodeAnalysis.Macros.FreestandingMacroContext
            ) {
                expand path
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

        Assert.Equal(MacroParameterRole.Value, symbol.Parameters[0].MacroRole);
        Assert.Equal(MacroParameterRole.FreestandingContext, symbol.Parameters[1].MacroRole);
        Assert.Equal("FreestandingMacroContext", symbol.Parameters[1].Type.Name);
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
