using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class MacroSymbolTests : CompilationTestBase
{
    [Fact]
    public void AuthoredMacroBody_RejectsBreakFromExpressionBlockWithinLoop()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            macro Broken() {
                loop {
                    let value = {
                        break
                        ()
                    }
                }
            }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroControlTransfer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDocumentDiagnostics(sourceTree);

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.BreakStatementInExpression);
    }

    [Fact]
    public void AuthoredConsumerPosition_BindsLocalReferenceAfterMacroInvocation()
    {
        const string source = """
            macro Double(value: int) {
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
                "MacroConsumer",
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

            macro Double(value: int) {
                let doubled = value * 2
                let text = doubled.ToString()
                expand SyntaxFactory.ParseExpression(text)
            }

            func Main() -> int => Double!(21)
            """;
        var authoredTree = SyntaxTree.ParseText(SourceText.From(source), path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(authoredTree);
        var bodyOffset = source.IndexOf("let doubled", StringComparison.Ordinal);

        var model = compilation.GetSemanticModel(authoredTree, bodyOffset);
        var declaration = model.SyntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
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
    public void MacroDeclaration_DeclaresDistinctMacroSymbol()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            macro Identity<T>(value: T) -> Raven.CodeAnalysis.Syntax.ExpressionSyntax
                where T: System.IDisposable
            {
                expand value
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(SyntaxNode).Assembly.Location));
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();

        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            model.GetDeclaredSymbol(declaration));

        Assert.Equal(SymbolKind.Macro, symbol.Kind);
        Assert.Equal(MacroApplicationKind.Invocable, symbol.ApplicationKind);
        Assert.Equal(MacroInvocationTargets.Expression, symbol.InvocationTargets);
        Assert.Equal(MacroKind.Invocable, symbol.MacroKind);
        Assert.Equal(MacroTarget.None, symbol.Targets);
        Assert.Null(symbol.TargetName);
        Assert.Equal("Identity", symbol.Name);
        Assert.Equal(Accessibility.Internal, symbol.DeclaredAccessibility);
        Assert.True(symbol.IsStatic);
        Assert.True(symbol.CanBeReferencedByName);
        Assert.False(symbol is IMethodSymbol);

        var typeParameter = Assert.Single(symbol.TypeParameters);
        Assert.Equal(TypeParameterOwnerKind.Macro, typeParameter.OwnerKind);
        Assert.Same(symbol, typeParameter.DeclaringMacroParameterOwner);
        Assert.Null(typeParameter.DeclaringMethodParameterOwner);
        Assert.Null(typeParameter.DeclaringTypeParameterOwner);
        Assert.Equal(TypeParameterConstraintKind.TypeConstraint, typeParameter.ConstraintKind);
        Assert.Equal("IDisposable", Assert.Single(typeParameter.ConstraintTypes).Name);
        Assert.Equal("ExpressionSyntax", symbol.ReturnType.Name);

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
            SemanticClassification.Macro,
            classifications.Tokens[declaration.Identifier]);
        Assert.Equal(
            SemanticClassification.Parameter,
            classifications.Tokens[declaration.ParameterList.Parameters.Single().Identifier]);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroDeclaration_UsesDeclaredAccessibility()
    {
        var (compilation, tree) = CreateCompilation("""
            public macro Exported() {
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("42")
            }
            """);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();

        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(Accessibility.Public, symbol.DeclaredAccessibility);
    }

    [Fact]
    public void MacroDeclaration_UsesItsNamespaceAsSemanticContainer()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            namespace Tools {
                macro Quote(value: int) -> Raven.CodeAnalysis.Syntax.ExpressionSyntax {}
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(SyntaxNode).Assembly.Location));
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();

        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            model.GetDeclaredSymbol(declaration));

        Assert.Equal("Tools", symbol.ContainingNamespace?.ToDisplayString());
        Assert.Same(symbol.ContainingNamespace, symbol.ContainingSymbol);
        Assert.Null(symbol.ContainingType);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroDeclaration_RejectsAsyncAndAwait()
    {
        var (compilation, _) = CreateCompilation("""
            async macro Fetch(task: System.Threading.Tasks.Task<int>) -> int {
                return await task
            }
            """);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0925");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0926");
    }

    [Fact]
    public void AttachedMacro_ExposesTargetSemantics()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            macro Observable(on property: Raven.CodeAnalysis.Syntax.PropertyDeclarationSyntax) {
                replace property
                introduce property
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(PropertyDeclarationSyntax).Assembly.Location));
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();
        var model = compilation.GetSemanticModel(tree);
        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            model.GetDeclaredSymbol(declaration));

        Assert.Equal(MacroKind.AttachedDeclaration, symbol.MacroKind);
        Assert.Equal(MacroTarget.Property, symbol.Targets);
        Assert.Equal("property", symbol.TargetName);
        var targetParameter = Assert.IsAssignableFrom<IParameterSymbol>(symbol.TargetParameter);
        Assert.Same(Assert.Single(symbol.Parameters), targetParameter);
        Assert.Equal(MacroParameterRole.AttachedTarget, targetParameter.MacroRole);
        Assert.Equal("PropertyDeclarationSyntax", targetParameter.Type.Name);
        Assert.Equal(
            declaration.ParameterList.Parameters[0].Identifier.Span,
            Assert.Single(targetParameter.Locations).SourceSpan);
        Assert.All(
            declaration.Body!.DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Where(static identifier => identifier.Identifier.ValueText == "property"),
            identifier => Assert.Same(targetParameter, model.GetSymbolInfo(identifier).Symbol));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AttachedMacro_TargetParameterRulesProduceStableDiagnostics()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            macro Duplicate(
                on first: Raven.CodeAnalysis.Syntax.BaseTypeDeclarationSyntax,
                on second: Raven.CodeAnalysis.Syntax.ClassDeclarationSyntax
            ) {
                introduce first
            }

            macro Defaulted(
                on target: Raven.CodeAnalysis.Syntax.BaseTypeDeclarationSyntax = null
            ) {
                introduce target
            }

            macro Unsupported(on target: string) {
                introduce target
            }

            macro Incomplete(on target:) {
                introduce target
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(PropertyDeclarationSyntax).Assembly.Location));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0935");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0936");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0927");

        var declarations = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .ToArray();
        Assert.Equal(4, declarations.Length);
        Assert.All(
            declarations,
            declaration => Assert.NotNull(
                compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration)));
    }

    [Fact]
    public void AttachedMacro_UnionCaseSyntaxTarget_IsATypeTarget()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            macro CaseMetadata(on target: Raven.CodeAnalysis.Syntax.CaseDeclarationSyntax) {
                expand target
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(CaseDeclarationSyntax).Assembly.Location));
        var declaration = tree.GetRoot().DescendantNodes().OfType<MacroDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(MacroTarget.Type, symbol.Targets);
        Assert.Equal("CaseDeclarationSyntax", symbol.TargetParameter!.Type.Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV0927");
    }

    [Fact]
    public void InvocableMacro_ReturnTypeProjectsGrammarTargets()
    {
        var (compilation, tree) = CreateCompilation("""
            import Raven.CodeAnalysis.Syntax.*

            macro Inferred() {}
            macro Expression() -> ExpressionSyntax {}
            macro Literal() -> LiteralExpressionSyntax {}
            macro Statement() -> StatementSyntax {}
            macro Members() -> SyntaxList<MemberDeclarationSyntax> {}
            macro Methods() -> SyntaxList<MethodDeclarationSyntax> {}
            macro InvalidList() -> SyntaxList<ExpressionSyntax> {}
            macro Flexible() -> ExpressionSyntax | StatementSyntax {}
            macro Untyped() -> SyntaxNode {}
            macro Unsupported() -> int {}
            macro AttachedResult(
                on target: BaseTypeDeclarationSyntax
            ) -> ExpressionSyntax {}
            """,
            references:
            [
                .. TestMetadataReferences.DefaultWithRavenCore,
                MetadataReference.CreateFromFile(typeof(SyntaxNode).Assembly.Location)
            ]);
        var symbols = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Select(declaration => Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
                compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration)))
            .ToDictionary(static symbol => symbol.Name);

        Assert.Equal(MacroInvocationTargets.Expression, symbols["Inferred"].InvocationTargets);
        Assert.Equal(MacroInvocationTargets.Expression, symbols["Expression"].InvocationTargets);
        Assert.Equal(MacroInvocationTargets.Expression, symbols["Literal"].InvocationTargets);
        Assert.Equal(MacroInvocationTargets.Statement, symbols["Statement"].InvocationTargets);
        var memberTargets = MacroInvocationTargets.NamespaceMember | MacroInvocationTargets.TypeMember;
        Assert.Equal(memberTargets, symbols["Members"].InvocationTargets);
        Assert.Equal(memberTargets, symbols["Methods"].InvocationTargets);
        Assert.Equal(MacroInvocationTargets.None, symbols["InvalidList"].InvocationTargets);
        var flexibleTargets = symbols["Flexible"].InvocationTargets;
        Assert.True(
            flexibleTargets == (MacroInvocationTargets.Expression | MacroInvocationTargets.Statement),
            $"Expected expression and statement targets, got {flexibleTargets}. " +
            $"Return type: {symbols["Flexible"].ReturnType.ToDisplayString()} " +
            $"({symbols["Flexible"].ReturnType.TypeKind}). Diagnostics: " +
            string.Join(" | ", compilation.GetDiagnostics().Select(static diagnostic => diagnostic.ToString())));
        Assert.Equal(MacroInvocationTargets.AllSingleNode, symbols["Untyped"].InvocationTargets);
        Assert.Equal(MacroInvocationTargets.None, symbols["Unsupported"].InvocationTargets);
        Assert.Equal(MacroInvocationTargets.None, symbols["AttachedResult"].InvocationTargets);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0937");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0938");
    }

    [Fact]
    public void MacroContribution_ValidatesAgainstAttachmentKind()
    {
        var (compilation, _) = CreateCompilation("""
            macro Invocable() {
                replace quote! { 1 }
            }

            macro Attached(on target: Raven.CodeAnalysis.Syntax.BaseTypeDeclarationSyntax) {
                expand quote! { 1 }
            }
            """);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal(1, diagnostics.Count(static diagnostic => diagnostic.Id == "RAV0928"));
    }

    [Fact]
    public void DuplicateMacroSignature_ReportsMacroDiagnostic()
    {
        var (compilation, _) = CreateCompilation("""
            macro Repeat(value: int) {
                expand value
            }

            macro Repeat(value: int) {
                expand value
            }
            """);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAV0934"));
        Assert.Contains("macro named 'Repeat'", diagnostic.GetMessage());
    }

    [Fact]
    public void ExpandContribution_IsAControlFlowReturn()
    {
        var (compilation, tree) = CreateCompilation("""
            macro Choose() {
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("1")
                let unreachable = 2
            }
            """);
        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single()
            .Body!;

        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);

        Assert.False(analysis.EndPointIsReachable);
        Assert.IsType<MacroExpansionStatementSyntax>(Assert.Single(analysis.ReturnStatements));
        Assert.IsType<LocalDeclarationStatementSyntax>(Assert.Single(analysis.UnreachableStatements));
    }

    [Fact]
    public void FragmentContribution_RequiresTokenTreeMacro()
    {
        var (compilation, _) = CreateCompilation("""
            macro ArgumentMacro(value: int) {
                fragment value
                token value
                expand value
            }

            macro AttachedMacro(on target: Raven.CodeAnalysis.Syntax.BaseTypeDeclarationSyntax) {
                fragment target
                token target
            }
            """);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal(4, diagnostics.Count(static diagnostic => diagnostic.Id == "RAV0928"));
    }

    [Fact]
    public void TokenStreamMacro_ExposesTypeDirectedParameterRoles()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            import Raven.CodeAnalysis.Macros.*

            macro Query(dialect: string, tokens: Raven.CodeAnalysis.Macros.IMacroTokenStream) {
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("0")
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location));
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(MacroKind.Invocable, symbol.MacroKind);
        Assert.Equal(MacroTarget.None, symbol.Targets);
        Assert.Equal(MacroParameterRole.Value, symbol.Parameters[0].MacroRole);
        Assert.Equal(MacroParameterRole.TokenBody, symbol.Parameters[1].MacroRole);
        Assert.Equal("IMacroTokenStream", symbol.Parameters[1].Type.Name);
        Assert.Contains("tokens: IMacroTokenStream", symbol.ToDisplayString());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ExpressionMacro_ExposesSyntaxProjectionRole()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            import Raven.CodeAnalysis.Syntax.*

            macro Rewrite(value: Raven.CodeAnalysis.Syntax.ExpressionSyntax) {
                expand value
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location));
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        var parameter = Assert.Single(symbol.Parameters);

        Assert.Equal(MacroParameterRole.SyntaxInput, parameter.MacroRole);
        Assert.Equal("ExpressionSyntax", parameter.Type.Name);
        Assert.Contains("value: ExpressionSyntax", symbol.ToDisplayString());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ArgumentStyleMacro_ExposesCompilerSuppliedInvocableContextRole()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            macro Embed(
                path: string,
                context: Raven.CodeAnalysis.Macros.InvocableMacroContext
            ) {
                expand path
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location));
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(MacroParameterRole.Value, symbol.Parameters[0].MacroRole);
        Assert.Equal(MacroParameterRole.Context, symbol.Parameters[1].MacroRole);
        Assert.Equal("InvocableMacroContext", symbol.Parameters[1].Type.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AttachedMacro_ExposesCompilerSuppliedAttachedContextRole()
    {
        var (baseCompilation, tree) = CreateCompilation("""
            macro Validate(
                context: Raven.CodeAnalysis.Macros.AttachedMacroContext,
                on target: Raven.CodeAnalysis.Syntax.BaseTypeDeclarationSyntax
            ) {
                context.ReportDiagnostic("Invalid type")
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location));
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(2, symbol.Parameters.Length);
        var parameter = symbol.Parameters[0];
        Assert.Equal(MacroApplicationKind.Attached, symbol.ApplicationKind);
        Assert.Equal(MacroInvocationTargets.None, symbol.InvocationTargets);
        Assert.Equal(MacroParameterRole.Context, parameter.MacroRole);
        Assert.Equal("AttachedMacroContext", parameter.Type.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroContextParameter_MustMatchMacroKind()
    {
        var (baseCompilation, _) = CreateCompilation("""
            macro Invocable(context: Raven.CodeAnalysis.Macros.AttachedMacroContext) {
                context.ReportDiagnostic("Invalid")
            }

            macro Attached(
                context: Raven.CodeAnalysis.Macros.InvocableMacroContext,
                on target: Raven.CodeAnalysis.Syntax.BaseTypeDeclarationSyntax
            ) {
                context.ReportDiagnostic("Invalid")
            }
            """);
        var compilation = baseCompilation.AddReferences(
            MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location));

        Assert.Equal(
            2,
            compilation.GetDiagnostics().Count(static diagnostic => diagnostic.Id == "RAV0933"));
    }

    [Fact]
    public void MacroParameterRole_UsesResolvedTypeIdentity()
    {
        var (compilation, tree) = CreateCompilation("""
            class ExpressionSyntax {}

            macro Custom(value: ExpressionSyntax) {
                expand value
            }
            """);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();
        var parameter = Assert.Single(
            Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
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

            macro Defaulted(content: Raven.CodeAnalysis.Macros.IMacroTokenStream = null) {
                expand content
            }

            macro Duplicate(first: Raven.CodeAnalysis.Macros.IMacroTokenStream, second: Raven.CodeAnalysis.Macros.IMacroTokenStream) {
                expand first
            }

            macro Attached(
                tokens: Raven.CodeAnalysis.Macros.IMacroTokenStream,
                on target: Raven.CodeAnalysis.Syntax.BaseTypeDeclarationSyntax
            ) {
                introduce tokens.ReadToken()
            }

            macro DefaultedExpression(value: Raven.CodeAnalysis.Syntax.ExpressionSyntax = 1) {
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
