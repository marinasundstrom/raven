using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroReferenceTests
{
    [Fact]
    public void CompilerPluginMarker_RequiresAssemblyTarget()
    {
        var markedTree = SyntaxTree.ParseText("""
            [assembly: RavenCompilerPlugin]
            """);
        var declarationMarkerTree = SyntaxTree.ParseText("""
            [RavenCompilerPlugin]
            class Plugin {}
            """);
        var unmarkedTree = SyntaxTree.ParseText("""
            class Plugin {}
            """);

        Assert.True(LocalMacroSyntaxClassifier.IsCompilerPluginTree(markedTree));
        Assert.False(LocalMacroSyntaxClassifier.IsCompilerPluginTree(declarationMarkerTree));
        Assert.False(LocalMacroSyntaxClassifier.IsCompilerPluginTree(unmarkedTree));
    }

    [Fact]
    public void CompilerPluginTree_RecognizesCompactDeclarationAsMacroPosition()
    {
        const string source = """
            [assembly: RavenCompilerPlugin]

            public macro Answer() {
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("42")
            }
            """;
        var tree = SyntaxTree.ParseText(source);

        Assert.False(LocalMacroSyntaxClassifier.IsLocalMacroPosition(tree, 0));
        Assert.True(LocalMacroSyntaxClassifier.IsLocalMacroPosition(
            tree,
            source.IndexOf("expand", System.StringComparison.Ordinal)));
    }

    [Fact]
    public void LocalMacroTree_RecognizesExecutorClass()
    {
        var tree = SyntaxTree.ParseText("""
            class AnswerMacro : IMacroExecutor {
                val Name: string => "answer"
                val ApplicationKind: MacroApplicationKind => MacroApplicationKind.Freestanding

                func Expand(context: MacroExecutionContext) -> MacroExecutionResult
                    => MacroExecutionResult.Freestanding(FreestandingMacroExpansionResult.Empty)
            }
            """);

        Assert.True(LocalMacroSyntaxClassifier.IsLocalMacroTree(tree));
    }

    [Fact]
    public void LocalMacroTree_RecognizesMethodShapedClass()
    {
        var tree = SyntaxTree.ParseText("""
            class AnswerMacro : IMacroDefinition {
                func Expand(value: ExpressionSyntax, context: FreestandingMacroContext) -> ExpressionSyntax
                    => value
            }
            """);

        Assert.True(LocalMacroSyntaxClassifier.IsLocalMacroTree(tree));
    }

    [Fact]
    public void MacroReference_FromType_FindsDirectMacro()
    {
        var reference = new MacroReference(typeof(TestAttachedMacro));

        var macro = Assert.Single(reference.Macros.OfType<TestAttachedMacro>());
        Assert.Same(macro, Assert.Single(reference.Macros));

        Assert.Equal("AddEquatable", macro.Name);
        Assert.Equal(MacroApplicationKind.Attached, MacroFacts.GetApplicationKind(macro));
        Assert.Equal(MacroInvocationTargets.None, MacroFacts.GetInvocationTargets(macro));
        Assert.Equal(MacroKind.AttachedDeclaration, MacroFacts.GetKind(macro));
        Assert.Equal(MacroTarget.Type, MacroFacts.GetTargets(macro));

        var descriptor = MacroFacts.GetDescriptor(macro);
        Assert.Same(macro, descriptor.Definition);
        Assert.Equal(MacroApplicationKind.Attached, descriptor.ApplicationKind);
        Assert.Equal(MacroInvocationTargets.None, descriptor.InvocationTargets);
        Assert.Equal(MacroTarget.Type, descriptor.AttachmentTargets);
        Assert.False(descriptor.AcceptsArguments);
        Assert.False(descriptor.HasTokenBody);
    }

    [Fact]
    public void MacroRegistry_NormalizesLegacyProviderToExecutor()
    {
        var macro = new TestTokenTreeMacro();
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.Macros.*

            let value = tokenTree! { }
            """);
        var compilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(macro));

        Assert.True(compilation.GetMacroRegistry().TryResolveFreestandingMacro(
            compilation,
            syntaxTree.GetRoot(),
            macro.Name,
            out var loaded,
            out var isAmbiguous));
        Assert.False(isAmbiguous);
        Assert.Same(macro, loaded.Macro);
        Assert.NotSame(macro, loaded.Executor);
        Assert.True(loaded.Executor.HasTokenBody);
        Assert.Equal(MacroApplicationKind.Freestanding, loaded.Executor.ApplicationKind);
    }

    [Fact]
    public void MacroCarrierAttribute_DeclaresContextOwnedInvocationShape()
    {
        var descriptor = MacroFacts.GetDescriptor(new AttributedCarrierMacro());

        Assert.Equal(
            MacroCarrierKinds.TokenTree | MacroCarrierKinds.ExpressionHeader,
            descriptor.CarrierKinds);
        Assert.Equal(MacroBodyRequirement.Required, descriptor.BodyRequirement);
        Assert.True(descriptor.AcceptsArguments);
        Assert.Empty(descriptor.Parameters);
    }

    [Fact]
    public void MethodShapedClass_UsesExpandSignatureWithoutExecutorBoilerplate()
    {
        var macro = new MethodShapedMacro();
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.Macros.*

            let value = MethodShaped!(2, 40 + 2)
            """);
        var compilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(macro));

        var descriptor = MacroFacts.GetDescriptor(macro);
        Assert.Equal("MethodShaped", ((IMacroDefinition)macro).Name);
        Assert.Equal(["count", "value"], descriptor.Parameters.Select(static parameter => parameter.Name));
        Assert.Equal(
            [MacroParameterRole.Value, MacroParameterRole.SyntaxInput],
            descriptor.Parameters.Select(static parameter => parameter.Role));

        Assert.True(compilation.GetMacroRegistry().TryResolveFreestandingMacro(
            compilation,
            syntaxTree.GetRoot(),
            ((IMacroDefinition)macro).Name,
            out var loaded,
            out var isAmbiguous));
        Assert.False(isAmbiguous);
        Assert.IsType<MethodMacroExecutorAdapter>(loaded.Executor);
        Assert.Equal(3, loaded.Executor.Parameters.Length);
        Assert.Equal(MacroParameterSource.Context, loaded.Executor.Parameters[2].Source);

        var expanded = compilation.GetSemanticModel(syntaxTree).GetExpandedRoot().ToFullString();
        Assert.Contains("let value = 40 + 2", expanded, System.StringComparison.Ordinal);
        Assert.Equal(2, macro.ObservedCount);
        Assert.True(macro.ReceivedContext);
    }

    [Fact]
    public void DeclarationShapedMacro_ReceivesDeclarationAndTokenBody()
    {
        var macro = new DeclarationClassMacro();
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.Macros.*

            component! Greeting() { 42 }

            func Read() -> int => Greeting.Value()
            """);
        var compilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(macro));

        var descriptor = MacroFacts.GetDescriptor(macro);
        Assert.True(descriptor.HasDeclarationInput);
        Assert.True(descriptor.HasTokenBody);
        Assert.False(descriptor.AcceptsArguments);
        Assert.True(MethodMacroFacts.TryGetExpandMethod(typeof(DeclarationClassMacro), out var expandMethod));
        Assert.Equal(
            [MacroParameterSource.DeclarationInput, MacroParameterSource.TokenBody],
            MethodMacroFacts.GetParameters(expandMethod)
                .Select(static parameter => parameter.Source));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var expanded = semanticModel.GetExpandedRoot().ToFullString();
        var errors = compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();
        Assert.True(errors.Length == 0, string.Join(System.Environment.NewLine, errors.Select(static diagnostic => diagnostic.ToString())));
        Assert.Contains("class Greeting", expanded, System.StringComparison.Ordinal);
        Assert.Contains("=> 42", expanded, System.StringComparison.Ordinal);
    }

    [Fact]
    public void DeclarationShapedMacro_ReceivesStructuredHeaderThroughSyntaxAndContext()
    {
        var macro = new RichDeclarationClassMacro();
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.Macros.*

            richComponent! Greeting<T>(value: T)
                : ComponentBase, IRenderable<T>
                where T: Entity
                permits SpecializedGreeting
            { 42 }
            """);
        var compilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(macro));

        _ = compilation.GetSemanticModel(syntaxTree).GetExpandedRoot();

        Assert.Same(macro.DeclarationHeader, macro.ContextHeader);
        Assert.Equal("T", Assert.Single(macro.DeclarationHeader!.TypeParameterList!.Parameters).Identifier.ValueText);
        Assert.Equal(2, Assert.IsType<MacroBaseListSuffixSyntax>(macro.DeclarationHeader.Suffix).BaseList.Types.Count);
        Assert.Single(macro.DeclarationHeader.ConstraintClauses);
        Assert.NotNull(macro.DeclarationHeader.PermitsClause);
    }

    [Fact]
    public void DeclarationShapedMacro_CanExpandMarkerHeaderWithoutTokenBody()
    {
        var macro = new MarkerDeclarationClassMacro();
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.Macros.*

            marker! GeneratedMember
            """);
        var compilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(macro));

        var expanded = compilation.GetSemanticModel(syntaxTree).GetExpandedRoot();

        Assert.Contains(
            expanded.Members,
            static member => member is ClassDeclarationSyntax { Identifier.ValueText: "GeneratedMember" });
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void MacroReference_FromRavenMethodShapedClass_UsesOrdinaryExpandMethod()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*
            import Raven.CodeAnalysis.Syntax.*

            [assembly: RavenCompilerPlugin]

            public class IdentityMacro : IMacroDefinition {
                func Expand(count: int, value: ExpressionSyntax, context: FreestandingMacroContext) -> ExpressionSyntax
                    => value
            }
            """);
        var reference = MacroReference.CreateFromImage(macroImage);
        var macro = Assert.Single(reference.Macros);

        Assert.Equal("Identity", macro.Name);
        Assert.IsAssignableFrom<IMacroExecutor>(macro);
        Assert.Equal(
            ["count", "value"],
            MacroFacts.GetDescriptor(macro).Parameters.Select(static parameter => parameter.Name));

        var consumerTree = SyntaxTree.ParseText("""
            let answer = Identity!(1, 42)
            """);
        var consumerCompilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(consumerTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(reference);

        Assert.Contains(
            "let answer = 42",
            consumerCompilation.GetSemanticModel(consumerTree).GetExpandedRoot().ToFullString(),
            System.StringComparison.Ordinal);
    }

    [Fact]
    public void MacroReference_FromCompactDeclaration_ExpandsDeclarationCarrier()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*
            import Raven.CodeAnalysis.Syntax.*

            [assembly: RavenCompilerPlugin]

            [MacroAlias("component")]
            public macro FunctionComponent(
                declaration: FreestandingMacroDeclarationSyntax,
                body: IMacroTokenStream
            ) -> MemberDeclarationSyntax {
                let value = body.ReadToken().Text
                let source = "class " + declaration.Identifier.ValueText +
                    " { static func Value() -> int => " + value + " }"
                expand SyntaxFactory.ParseSyntaxTree(source).GetRoot().Members[0]
            }
            """);
        var reference = MacroReference.CreateFromImage(macroImage);
        var consumerTree = SyntaxTree.ParseText("""
            component! Greeting() { 42 }

            func Read() -> int => Greeting.Value()
            """);
        var consumerCompilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(consumerTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(reference);

        var expanded = consumerCompilation.GetSemanticModel(consumerTree).GetExpandedRoot().ToFullString();
        Assert.Contains("class Greeting", expanded, System.StringComparison.Ordinal);
        Assert.DoesNotContain(
            consumerCompilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void MacroReference_FromGenericMethodShapedClass_PreservesCanonicalSignature()
    {
        const string source = """
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*
            import Raven.CodeAnalysis.Syntax.*

            [assembly: RavenCompilerPlugin]

            [MacroAlias("alternate")]
            public class IdentityMacro<T> : IMacroDefinition, IMacroKeywordProvider {
                val Name: string => "customIdentity"
                val Alias: string? => "identity"
                val Keywords: ImmutableArray<MacroKeyword> => []

                func Expand(value: T, syntax: ExpressionSyntax, context: FreestandingMacroContext) -> ExpressionSyntax
                    => Select(syntax)

                func Select(syntax: ExpressionSyntax) -> ExpressionSyntax => syntax
            }
            """;
        var sourceTree = SyntaxTree.ParseText(source);
        var sourceClass = sourceTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        Assert.True(LocalMacroSyntaxClassifier.IsMethodShapedMacroClass(sourceClass));

        var macroImage = EmitMacroAssembly(source);
        var reference = MacroReference.CreateFromImage(macroImage);
        var macro = Assert.Single(reference.Macros);
        var executor = Assert.IsAssignableFrom<IMacroExecutor>(macro);

        Assert.Equal("customIdentity", macro.Name);
        Assert.Equal("identity", macro.Alias);
        Assert.IsAssignableFrom<IMacroKeywordProvider>(macro);
        Assert.Contains(
            macro.GetType().GetCustomAttributes(inherit: false),
            static attribute => attribute is MacroAliasAttribute { Alias: "alternate" });
        Assert.Equal(["T"], executor.TypeParameters.ToArray());
        Assert.Equal(["T", "ExpressionSyntax", "FreestandingMacroContext"],
            executor.Parameters.Select(static parameter => parameter.TypeDisplayName));
        Assert.Equal(
            [MacroParameterSource.Value, MacroParameterSource.SyntaxInput, MacroParameterSource.Context],
            executor.Parameters.Select(static parameter => parameter.Source));

        var consumerTree = SyntaxTree.ParseText("""
            let answer = alternate<int>!(1, 42)
            """);
        var consumerCompilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(consumerTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(reference);

        Assert.Contains(
            "let answer = 42",
            consumerCompilation.GetSemanticModel(consumerTree).GetExpandedRoot().ToFullString(),
            System.StringComparison.Ordinal);
    }

    [Fact]
    public void MacroReference_FromType_RejectsNonMacroExportTypes()
    {
        var ex = Assert.Throws<System.ArgumentException>(() => new MacroReference(typeof(MacroReferenceTests)));
        Assert.Contains("one supported Expand contract", ex.Message);
    }

    [Fact]
    public void MacroReference_FromInMemoryRavenAssembly_ExpandsMacro()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*
            import Raven.Macros.*

            [assembly: RavenCompilerPlugin(typeof(AnswerMacro))]

            class AnswerMacro : IMacroDefinition {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    FreestandingMacroExpansionResult {
                        Expression = quote! { 42 }
                    }
                }
            }

            class UnselectedMacro : IMacroDefinition {
                val Name: string => "unselected"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }
            """);
        var reference = MacroReference.CreateFromImage(
            macroImage,
            display: "same-project macro partition");

        var macro = Assert.Single(reference.Macros);
        Assert.Equal("answer", macro.Name);

        var consumerTree = SyntaxTree.ParseText("func Main() -> int => answer!{ }");
        var consumerCompilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(consumerTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(reference);

        Assert.DoesNotContain(
            consumerCompilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void MacroReference_BareCompilerPluginMarker_UsesFallbackDiscovery()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin]

            public class FirstMacro : IMacroDefinition {
                val Name: string => "first"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }

            public class SecondMacro : IMacroDefinition {
                val Name: string => "second"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }

            class HiddenMacro : IMacroDefinition {
                val Name: string => "hidden"

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }
            """);

        var macros = MacroReference.CreateFromImage(macroImage)
            .Macros
            .Select(static macro => macro.Name)
            .Order()
            .ToArray();

        Assert.Equal(["first", "second"], macros);
    }

    [Fact]
    public void MacroLibrary_EmitsReusableCompilerPluginFromSingleSourceTree()
    {
        var instrumentation = new PerformanceInstrumentation();
        var macroTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin]

            namespace Example.Macros

            /// Expands to `42`; `$value` remains documentation text.
            [Raven.CodeAnalysis.Macros.MacroAlias("answer")]
            public macro Answer(context: Raven.CodeAnalysis.Macros.TokenTreeMacroContext) {
                expand Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult.FromExpression(
                    Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("42"))
            }

            macro Hidden(context: Raven.CodeAnalysis.Macros.TokenTreeMacroContext) {
                expand Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult.FromExpression(
                    Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("0"))
            }
            """,
            path: "Answer.rvn");
        var macroCompilation = Compilation.Create(
                "Example.Macros",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                    .WithPerformanceInstrumentation(instrumentation))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(macroTree);

        using var image = new MemoryStream();
        var emitResult = macroCompilation.Emit(image);
        Assert.True(
            emitResult.Success,
            string.Join(System.Environment.NewLine, emitResult.Diagnostics));
        Assert.Equal(0, instrumentation.Macros.LocalPartitionCompilations);

        var macroReference = MacroReference.CreateFromImage(
            image.ToArray(),
            display: "Raven macro declaration library");
        var macro = Assert.Single(macroReference.Macros);
        Assert.Equal("Example.Macros", macro.Namespace);
        Assert.Equal("Answer", macro.Name);
        Assert.Equal("answer", macro.Alias);
        Assert.Equal("Expands to `42`; `$value` remains documentation text.", macro.Documentation);
        Assert.Equal(DocumentationFormat.Markdown, macro.DocumentationFormat);

        var consumer = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(macroReference)
            .AddSyntaxTrees(SyntaxTree.ParseText(
                """
                import Example.Macros.*

                func Main() -> int => answer! { ignored }
                """));

        Assert.DoesNotContain(
            consumer.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var consumerTree = Assert.Single(consumer.SyntaxTrees);
        var invocation = Assert.Single(
            consumerTree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
        var macroSymbol = Assert.IsAssignableFrom<IMacroSymbol>(
            consumer.GetSemanticModel(consumerTree).GetSymbolInfo(invocation).Symbol);
        var documentation = macroSymbol.GetDocumentationComment();
        Assert.NotNull(documentation);
        Assert.Equal(DocumentationFormat.Markdown, documentation!.Format);
        Assert.Contains("Expands to `42`", documentation.Content, StringComparison.Ordinal);
    }

    [Fact]
    public void MacroReference_ExplicitManifestSupportsMultipleEntryPoints()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(FirstMacro))]
            [assembly: RavenCompilerPlugin(typeof(SecondMacro))]

            class FirstMacro : IMacroDefinition {
                val Name: string => "first"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }

            class UnselectedMacro : IMacroDefinition {
                val Name: string => "unselected"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }

            class SecondMacro : IMacroDefinition {
                val Name: string => "second"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }
            """);

        var macros = MacroReference.CreateFromImage(macroImage)
            .Macros
            .Select(static macro => macro.Name)
            .ToArray();

        Assert.Equal(["first", "second"], macros);
    }

    [Fact]
    public void MacroReference_FromFile_UsesExplicitEntryPointManifest()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(SelectedMacro))]

            class SelectedMacro : IMacroDefinition {
                val Name: string => "selected"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }

            class UnselectedMacro : IMacroDefinition {
                val Name: string => "unselected"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }
            """);
        var assemblyPath = Path.Combine(
            Path.GetTempPath(),
            $"RavenMacroReference_{System.Guid.NewGuid():N}.dll");

        try
        {
            File.WriteAllBytes(assemblyPath, macroImage);

            var macro = Assert.Single(MacroReference.CreateFromFile(assemblyPath).Macros);

            Assert.Equal("selected", macro.Name);
        }
        finally
        {
            File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void MacroReference_FromUnchangedFile_ReusesLoadedAssemblyAcrossReferences()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(SharedMacro))]

            class SharedMacro : IMacroDefinition {
                val Name: string => "shared"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }
            """);
        var assemblyPath = Path.Combine(Path.GetTempPath(), $"RavenSharedMacro_{System.Guid.NewGuid():N}.dll");

        try
        {
            File.WriteAllBytes(assemblyPath, macroImage);

            var firstReference = MacroReference.CreateFromFile(assemblyPath);
            var first = Assert.Single(firstReference.Macros);
            System.GC.Collect();
            System.GC.WaitForPendingFinalizers();
            System.GC.Collect();
            var second = Assert.Single(MacroReference.CreateFromFile(assemblyPath).Macros);

            Assert.NotSame(first, second);
            Assert.Same(first.GetType(), second.GetType());
            Assert.Same(first.GetType().Assembly, second.GetType().Assembly);
            System.GC.KeepAlive(firstReference);
        }
        finally
        {
            File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void MacroReference_ConcurrentFileLoads_ShareOneAssembly()
    {
        var macroImage = EmitNamedMacroAssembly("concurrent");
        var assemblyPath = Path.Combine(Path.GetTempPath(), $"RavenConcurrentMacro_{System.Guid.NewGuid():N}.dll");

        try
        {
            File.WriteAllBytes(assemblyPath, macroImage);
            var loads = Enumerable.Range(0, 8)
                .Select(_ => System.Threading.Tasks.Task.Run(
                    () => Assert.Single(MacroReference.CreateFromFile(assemblyPath).Macros).GetType().Assembly))
                .ToArray();

            var assemblies = System.Threading.Tasks.Task.WhenAll(loads).GetAwaiter().GetResult();

            Assert.All(assemblies, assembly => Assert.Same(assemblies[0], assembly));
        }
        finally
        {
            File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void MacroReference_ChangedFile_LoadsNewAssemblyIdentity()
    {
        var firstImage = EmitNamedMacroAssembly("stable");
        var secondImage = EmitNamedMacroAssembly("stable");
        var assemblyPath = Path.Combine(Path.GetTempPath(), $"RavenChangedMacro_{System.Guid.NewGuid():N}.dll");

        try
        {
            File.WriteAllBytes(assemblyPath, firstImage);
            var first = Assert.Single(MacroReference.CreateFromFile(assemblyPath).Macros);

            var replacementPath = assemblyPath + ".replacement";
            File.WriteAllBytes(replacementPath, secondImage);
            File.Delete(assemblyPath);
            File.Move(replacementPath, assemblyPath);
            var second = Assert.Single(MacroReference.CreateFromFile(assemblyPath).Macros);

            Assert.Equal("stable", first.Name);
            Assert.Equal("stable", second.Name);
            Assert.NotSame(first.GetType().Assembly, second.GetType().Assembly);
        }
        finally
        {
            File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void MacroReference_ChangedExplicitDependency_LoadsNewAssemblyIdentity()
    {
        var macroImage = EmitNamedMacroAssembly("dependency-sensitive");
        var assemblyPath = Path.Combine(Path.GetTempPath(), $"RavenDependencyMacro_{System.Guid.NewGuid():N}.dll");
        var dependencyPath = Path.Combine(Path.GetTempPath(), $"RavenDependency_{System.Guid.NewGuid():N}.dll");

        try
        {
            File.WriteAllBytes(assemblyPath, macroImage);
            File.WriteAllText(dependencyPath, "first dependency state");
            var first = Assert.Single(MacroReference.CreateFromFile(assemblyPath, null, [dependencyPath]).Macros);

            File.WriteAllText(dependencyPath, "second dependency state with different content");
            var second = Assert.Single(MacroReference.CreateFromFile(assemblyPath, null, [dependencyPath]).Macros);

            Assert.NotSame(first.GetType().Assembly, second.GetType().Assembly);
        }
        finally
        {
            File.Delete(assemblyPath);
            File.Delete(dependencyPath);
        }
    }

    [Fact]
    public void MacroReference_FailedFileLoad_DoesNotPoisonReusableExports()
    {
        var macroImage = EmitNamedMacroAssembly("recovered");
        var assemblyPath = Path.Combine(Path.GetTempPath(), $"RavenRecoveredMacro_{System.Guid.NewGuid():N}.dll");

        try
        {
            File.WriteAllText(assemblyPath, "not an assembly");
            var invalidReference = MacroReference.CreateFromFile(assemblyPath);
            Assert.Throws<System.BadImageFormatException>(() => invalidReference.Macros);

            File.WriteAllBytes(assemblyPath, macroImage);
            var recovered = Assert.Single(MacroReference.CreateFromFile(assemblyPath).Macros);

            Assert.Equal("recovered", recovered.Name);
        }
        finally
        {
            File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void Compilation_MarkedMetadataReference_AutomaticallyActivatesMacro()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(AnswerMacro))]

            class AnswerMacro : IMacroDefinition {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.FromExpression(
                        Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("42"))
            }
            """);
        var assemblyPath = Path.Combine(
            Path.GetTempPath(),
            $"RavenReferencedMacro_{System.Guid.NewGuid():N}.dll");

        try
        {
            File.WriteAllBytes(assemblyPath, macroImage);
            var sourceTree = SyntaxTree.ParseText("func Main() -> int => answer!{ }");
            var baseCompilation = Compilation.Create(
                    "Consumer",
                    new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddSyntaxTrees(sourceTree)
                .AddReferences(TestMetadataReferences.Default);
            var compilation = baseCompilation.AddReferences(
                MetadataReference.CreateFromFile(assemblyPath));

            Assert.Contains(
                baseCompilation.GetDiagnostics(),
                static diagnostic => diagnostic.Id == "RAVM010");
            Assert.DoesNotContain(
                compilation.GetDiagnostics(),
                static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
            var reference = Assert.Single(compilation.MacroReferences);
            Assert.Equal(Path.GetFullPath(assemblyPath), reference.Display);
        }
        finally
        {
            File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void Compilation_UnmarkedMetadataReference_DoesNotExportMacro()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            class PrivateAnswerMacro : IMacroDefinition {
                val Name: string => "privateAnswer"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.FromExpression(
                        Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("42"))
            }
            """);
        var assemblyPath = Path.Combine(
            Path.GetTempPath(),
            $"RavenUnexportedMacro_{System.Guid.NewGuid():N}.dll");

        try
        {
            File.WriteAllBytes(assemblyPath, macroImage);
            var compilation = Compilation.Create(
                    "Consumer",
                    new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddSyntaxTrees(SyntaxTree.ParseText(
                    "func Main() -> int => privateAnswer!{ }"))
                .AddReferences(TestMetadataReferences.Default)
                .AddReferences(MetadataReference.CreateFromFile(assemblyPath));

            Assert.Contains(
                compilation.GetDiagnostics(),
                static diagnostic => diagnostic.Id == "RAVM010");
            Assert.Empty(compilation.MacroReferences);
        }
        finally
        {
            File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void MacroReference_InvalidDeclaredExport_ReportsLoadDiagnostic()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(NotAPlugin))]

            class NotAPlugin {}
            """);
        var compilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText("func Main() -> unit {}"))
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(MacroReference.CreateFromImage(
                macroImage,
                display: "invalid manifest"));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Id == "RAVM001");

        Assert.Contains("NotAPlugin", diagnostic.GetMessage());
        Assert.Contains("one supported Expand contract", diagnostic.GetMessage());
    }

    [Fact]
    public void MacroReference_ThrowingProvider_ReportsStableLoadDiagnosticAndKeepsOtherReferencesActive()
    {
        var macroImage = EmitMacroAssembly("""
            import System.*
            import Raven.CodeAnalysis.Macros.*
            import Raven.CodeAnalysis.Syntax.*

            [assembly: RavenCompilerPlugin(typeof(ThrowingMacro))]

            class ThrowingMacro : IMacroDefinition {
                init() {
                    throw InvalidOperationException("provider construction failed")
                }

                val Name: string => "Throwing"
                func Expand(target: BaseTypeDeclarationSyntax, context: AttachedMacroContext) -> MacroExpansionResult
                    => MacroExpansionResult.Empty
            }
            """);
        var compilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText("""
                import Raven.CodeAnalysis.Tests.Macros.*

                #[AddEquatable]
                class Customer {}
                """))
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(
                MacroReference.CreateFromImage(macroImage, "throwing provider assembly"),
                new MacroReference(typeof(TestAttachedMacro)));

        var firstDiagnostics = compilation.GetDiagnostics();
        var secondDiagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(
            firstDiagnostics,
            static diagnostic => diagnostic.Id == "RAVM001");

        Assert.Contains("ThrowingMacro", diagnostic.GetMessage());
        Assert.Contains("provider construction failed", diagnostic.GetMessage());
        Assert.DoesNotContain(firstDiagnostics, static diagnostic => diagnostic.Id == "RAVM010");
        Assert.Equal(
            firstDiagnostics.Select(static diagnostic => diagnostic.ToString()),
            secondDiagnostics.Select(static diagnostic => diagnostic.ToString()));
    }

    [Fact]
    public void MacroReference_FromImage_RejectsEmptyAssembly()
    {
        var ex = Assert.Throws<System.ArgumentException>(() => MacroReference.CreateFromImage([]));
        Assert.Contains("must not be empty", ex.Message);
    }

    [Fact]
    public void MethodShapedMacroDefinition_DoesNotExposeParameterObjectContract()
    {
        var macro = new TypedParameterAttachedMacro();

        Assert.DoesNotContain(
            macro.GetType().GetInterfaces(),
            static contract => contract.IsGenericType &&
                contract.GetGenericTypeDefinition().Name == "IMacroDefinition`1");
    }

    [Fact]
    public void MacroFacts_DescribesTypedPositionalAndNamedParameters()
    {
        var macro = new TypedParameterAttachedMacro();

        Assert.Collection(
            MacroFacts.GetParameters(macro),
            parameter =>
            {
                Assert.Equal("name", parameter.Name);
                Assert.Equal(typeof(string), parameter.ParameterType);
                Assert.Equal(MacroParameterRole.Value, parameter.Role);
                Assert.Equal(0, parameter.Ordinal);
                Assert.True(parameter.IsRequired);
            },
            parameter =>
            {
                Assert.Equal("count", parameter.Name);
                Assert.Equal(typeof(int), parameter.ParameterType);
                Assert.Equal(MacroParameterRole.Value, parameter.Role);
                Assert.Equal(1, parameter.Ordinal);
                Assert.False(parameter.IsRequired);
                Assert.Equal(1, parameter.DefaultValue);
            },
            parameter =>
            {
                Assert.Equal("Notify", parameter.Name);
                Assert.Equal(typeof(bool), parameter.ParameterType);
                Assert.Equal(MacroParameterRole.Value, parameter.Role);
                Assert.Equal(2, parameter.Ordinal);
                Assert.False(parameter.IsRequired);
            });
    }

    [Fact]
    public void MacroFacts_DescribesExpressionSyntaxProjection()
    {
        var parameter = Assert.Single(
            MacroFacts.GetParameters(new ExpressionParameterMacro()));

        Assert.Equal(typeof(ExpressionSyntax), parameter.ParameterType);
        Assert.Equal("ExpressionSyntax", parameter.TypeDisplayName);
        Assert.Equal(MacroParameterRole.SyntaxInput, parameter.Role);
    }

    [Fact]
    public void MacroFacts_RequiresExactlyOneCategoryInterface()
    {
        Assert.False(MacroFacts.TryGetKind(new UnclassifiedMacro(), out _));
        Assert.False(MacroFacts.TryGetKind(new AmbiguousMacro(), out _));
        Assert.Equal(MacroKind.AttachedDeclaration, MacroFacts.GetKind(new MisleadingKindMacro()));
        Assert.Throws<System.ArgumentException>(() => new MacroReference(typeof(UnclassifiedMacro)));
        Assert.Throws<System.ArgumentException>(() => new MacroReference(typeof(AmbiguousMacro)));
    }

    [Fact]
    public void MacroFacts_ReturnsNoDeclarationTargetsForFreestandingMacro()
    {
        var macro = new TestTokenTreeMacro();

        Assert.Equal(MacroApplicationKind.Freestanding, MacroFacts.GetApplicationKind(macro));
        Assert.Equal(MacroInvocationTargets.Expression, MacroFacts.GetInvocationTargets(macro));
        Assert.Equal(MacroTarget.None, MacroFacts.GetTargets(macro));

        var descriptor = MacroFacts.GetDescriptor(macro);
        Assert.Same(macro, descriptor.Definition);
        Assert.Equal(MacroApplicationKind.Freestanding, descriptor.ApplicationKind);
        Assert.Equal(MacroInvocationTargets.Expression, descriptor.InvocationTargets);
        Assert.Equal(MacroTarget.None, descriptor.AttachmentTargets);
        Assert.False(descriptor.AcceptsArguments);
        Assert.True(descriptor.HasTokenBody);
    }

    [Fact]
    public void MacroFacts_PreservesDeclaredFreestandingTargets()
    {
        var macro = new MemberTargetMacro();
        var expected = MacroInvocationTargets.NamespaceMember | MacroInvocationTargets.TypeMember;

        Assert.Equal(expected, MacroFacts.GetInvocationTargets(macro));
        Assert.Equal(expected, MacroFacts.GetDescriptor(macro).InvocationTargets);
    }

    public sealed class TestAttachedMacro : IMacroDefinition
    {
        public string Name => "AddEquatable";

        public MacroExpansionResult Expand(
            BaseTypeDeclarationSyntax target,
            AttachedMacroContext context)
            => MacroExpansionResult.Empty;
    }

    public sealed class TypedParameterAttachedMacro : IMacroDefinition
    {
        public string Name => "Observable";

        public MacroExpansionResult Expand(
            PropertyDeclarationSyntax target,
            string name,
            AttachedMacroContext context,
            int count = 1,
            bool Notify = true)
            => MacroExpansionResult.Empty;
    }

    public sealed class ExpressionParameterMacro : IMacroDefinition
    {
        public string Name => "expression";

        public FreestandingMacroExpansionResult Expand(ExpressionSyntax expression)
            => FreestandingMacroExpansionResult.Empty;
    }

    public sealed class UnclassifiedMacro : IMacroDefinition
    {
        public string Name => "unclassified";
    }

    public sealed class MethodShapedMacro : IMacroDefinition
    {
        public int ObservedCount { get; private set; }

        public bool ReceivedContext { get; private set; }

        public ExpressionSyntax Expand(
            int count,
            ExpressionSyntax value,
            FreestandingMacroContext context)
        {
            ObservedCount = count;
            ReceivedContext = context is not null;
            return value;
        }
    }

    public sealed class DeclarationClassMacro : IMacroDefinition
    {
        public string Name => "FunctionComponent";
        public string? Alias => "component";

        public MemberDeclarationSyntax Expand(
            FreestandingMacroDeclarationSyntax declaration,
            IMacroTokenStream body)
        {
            var value = body.ReadToken().Text;
            return SyntaxFactory.ParseSyntaxTree($$"""
                class {{declaration.Identifier.ValueText}} {
                    static func Value() -> int => {{value}}
                }
                """).GetRoot().Members.Single();
        }
    }

    public sealed class RichDeclarationClassMacro : IMacroDefinition
    {
        public string Name => "RichDeclaration";
        public string? Alias => "richComponent";

        public MacroDeclarationHeaderSyntax? DeclarationHeader { get; private set; }
        public MacroDeclarationHeaderSyntax? ContextHeader { get; private set; }

        public MemberDeclarationSyntax Expand(
            FreestandingMacroDeclarationSyntax declaration,
            TokenTreeMacroContext context)
        {
            DeclarationHeader = declaration.Header;
            ContextHeader = context.DeclarationHeader;
            return SyntaxFactory.ParseSyntaxTree($$"""
                class {{declaration.Identifier.ValueText}} {}
                """).GetRoot().Members.Single();
        }
    }

    public sealed class MarkerDeclarationClassMacro : IMacroDefinition
    {
        public string Name => "MarkerDeclaration";
        public string? Alias => "marker";

        public MemberDeclarationSyntax Expand(FreestandingMacroDeclarationSyntax declaration)
            => SyntaxFactory.ParseSyntaxTree($$"""
                class {{declaration.Identifier.ValueText}} {}
                """).GetRoot().Members.Single();
    }

    public sealed class TestTokenTreeMacro : IMacroDefinition
    {
        public string Name => "tokenTree";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    [MacroCarrier(
        MacroCarrierKinds.TokenTree | MacroCarrierKinds.ExpressionHeader,
        MacroBodyRequirement.Required)]
    public sealed class AttributedCarrierMacro : IMacroDefinition
    {
        public string Name => "attributedCarrier";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    public sealed class MemberTargetMacro : IMacroDefinition
    {
        public string Name => "members";
        public MacroInvocationTargets InvocationTargets =>
            MacroInvocationTargets.NamespaceMember | MacroInvocationTargets.TypeMember;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.FromMembers(
                SyntaxFactory.List<MemberDeclarationSyntax>());
    }

    public sealed class AmbiguousMacro : IMacroDefinition
    {
        public string Name => "ambiguous";
        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => MacroExpansionResult.Empty;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    public sealed class MisleadingKindMacro : IMacroDefinition
    {
        public string Name => "misleading";
        public MacroKind Kind => MacroKind.Freestanding;
        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => MacroExpansionResult.Empty;
    }

    private static byte[] EmitMacroAssembly(string source)
    {
        var macroTree = SyntaxTree.ParseText(source);
        var codeAnalysisReference = MetadataReference.CreateFromFile(typeof(IMacroDefinition).Assembly.Location);
        var macroCompilation = Compilation.Create(
                $"InMemoryMacros_{System.Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTreesWithLocalMacros(macroTree)
            .AddReferences([
                .. TestMetadataReferences.DefaultWithRavenMacros,
                codeAnalysisReference
            ])
            .AddMacroReferences(MacroReference.CreateFromFile(
                ((PortableExecutableReference)TestMetadataReferences.RavenMacros).FilePath!));

        using var macroImage = new MemoryStream();
        var macroEmit = macroCompilation.Emit(macroImage);
        Assert.True(macroEmit.Success, string.Join(System.Environment.NewLine, macroEmit.Diagnostics));
        return macroImage.ToArray();
    }

    private static byte[] EmitNamedMacroAssembly(string name)
        => EmitMacroAssembly($$"""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin]

            public class NamedMacro : IMacroDefinition {
                val Name: string => "{{name}}"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }
            """);
}
