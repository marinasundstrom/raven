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
    public void MacroReference_FromType_RejectsNonMacroExportTypes()
    {
        var ex = Assert.Throws<System.ArgumentException>(() => new MacroReference(typeof(MacroReferenceTests)));
        Assert.Contains("exactly one supported macro category interface", ex.Message);
    }

    [Fact]
    public void MacroReference_FromInMemoryRavenAssembly_ExpandsMacro()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*
            import Raven.Macros.*

            [assembly: RavenCompilerPlugin(typeof(AnswerMacro))]

            class AnswerMacro : ITokenTreeMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult {
                    InvocableMacroExpansionResult {
                        Expression = quote! { 42 }
                    }
                }
            }

            class UnselectedMacro : ITokenTreeMacro {
                val Name: string => "unselected"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
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

            public class FirstMacro : ITokenTreeMacro {
                val Name: string => "first"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
            }

            public class SecondMacro : ITokenTreeMacro {
                val Name: string => "second"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
            }

            class HiddenMacro : ITokenTreeMacro {
                val Name: string => "hidden"

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
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
        var macroTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin]

            namespace Example.Macros

            /// Expands to `42`; `$value` remains documentation text.
            [Raven.CodeAnalysis.Macros.MacroAlias("answer")]
            public macro Answer(context: Raven.CodeAnalysis.Macros.TokenTreeMacroContext) {
                expand Raven.CodeAnalysis.Macros.InvocableMacroExpansionResult.FromExpression(
                    Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("42"))
            }

            macro Hidden(context: Raven.CodeAnalysis.Macros.TokenTreeMacroContext) {
                expand Raven.CodeAnalysis.Macros.InvocableMacroExpansionResult.FromExpression(
                    Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("0"))
            }
            """,
            path: "Answer.rvn");
        var macroCompilation = Compilation.Create(
                "Example.Macros",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(macroTree);

        using var image = new MemoryStream();
        var emitResult = macroCompilation.Emit(image);
        Assert.True(
            emitResult.Success,
            string.Join(System.Environment.NewLine, emitResult.Diagnostics));

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
    }

    [Fact]
    public void MacroReference_ExplicitManifestSupportsMultipleEntryPoints()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(FirstMacro))]
            [assembly: RavenCompilerPlugin(typeof(SecondMacro))]

            class FirstMacro : ITokenTreeMacro {
                val Name: string => "first"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
            }

            class UnselectedMacro : ITokenTreeMacro {
                val Name: string => "unselected"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
            }

            class SecondMacro : ITokenTreeMacro {
                val Name: string => "second"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
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

            class SelectedMacro : ITokenTreeMacro {
                val Name: string => "selected"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
            }

            class UnselectedMacro : ITokenTreeMacro {
                val Name: string => "unselected"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.Empty
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
    public void Compilation_MarkedMetadataReference_AutomaticallyActivatesMacro()
    {
        var macroImage = EmitMacroAssembly("""
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(AnswerMacro))]

            class AnswerMacro : ITokenTreeMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.FromExpression(
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

            class PrivateAnswerMacro : ITokenTreeMacro {
                val Name: string => "privateAnswer"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult
                    => InvocableMacroExpansionResult.FromExpression(
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
        Assert.Contains("exactly one supported macro category interface", diagnostic.GetMessage());
    }

    [Fact]
    public void MacroReference_ThrowingProvider_ReportsStableLoadDiagnosticAndKeepsOtherReferencesActive()
    {
        var macroImage = EmitMacroAssembly("""
            import System.*
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(ThrowingMacro))]

            class ThrowingMacro : IAttachedDeclarationMacro {
                init() {
                    throw InvalidOperationException("provider construction failed")
                }

                val Name: string => "Throwing"
                val Targets: MacroTarget => MacroTarget.Type

                func Expand(context: AttachedMacroContext) -> MacroExpansionResult
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
    public void GenericMacroDefinition_ExposesTypedParameterObject()
    {
        var macro = new TypedParameterAttachedMacro();

        Assert.Equal(typeof(ObservableMacroParameters), ((IMacroDefinition<ObservableMacroParameters>)macro).ParametersType);
    }

    [Fact]
    public void MacroFacts_DescribesTypedPositionalAndNamedParameters()
    {
        var macro = new TypedParameterAttachedMacro();

        Assert.Equal(typeof(ObservableMacroParameters), MacroFacts.GetParametersType(macro));
        Assert.Collection(
            MacroFacts.GetParameters(macro),
            parameter =>
            {
                Assert.Equal("name", parameter.Name);
                Assert.Equal(typeof(string), parameter.ParameterType);
                Assert.Equal(MacroParameterKind.Positional, parameter.Kind);
                Assert.Equal(MacroParameterRole.Value, parameter.Role);
                Assert.Equal(0, parameter.Ordinal);
                Assert.True(parameter.IsRequired);
            },
            parameter =>
            {
                Assert.Equal("count", parameter.Name);
                Assert.Equal(typeof(int), parameter.ParameterType);
                Assert.Equal(MacroParameterKind.Positional, parameter.Kind);
                Assert.Equal(MacroParameterRole.Value, parameter.Role);
                Assert.Equal(1, parameter.Ordinal);
                Assert.False(parameter.IsRequired);
                Assert.Equal(1, parameter.DefaultValue);
            },
            parameter =>
            {
                Assert.Equal("Notify", parameter.Name);
                Assert.Equal(typeof(bool), parameter.ParameterType);
                Assert.Equal(MacroParameterKind.Named, parameter.Kind);
                Assert.Equal(MacroParameterRole.Value, parameter.Role);
                Assert.Equal(-1, parameter.Ordinal);
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
    public void MacroFacts_ReturnsNoDeclarationTargetsForInvocableMacro()
    {
        var macro = new TestTokenTreeMacro();

        Assert.Equal(MacroApplicationKind.Invocable, MacroFacts.GetApplicationKind(macro));
        Assert.Equal(MacroInvocationTargets.Expression, MacroFacts.GetInvocationTargets(macro));
        Assert.Equal(MacroTarget.None, MacroFacts.GetTargets(macro));

        var descriptor = MacroFacts.GetDescriptor(macro);
        Assert.Same(macro, descriptor.Definition);
        Assert.Equal(MacroApplicationKind.Invocable, descriptor.ApplicationKind);
        Assert.Equal(MacroInvocationTargets.Expression, descriptor.InvocationTargets);
        Assert.Equal(MacroTarget.None, descriptor.AttachmentTargets);
        Assert.False(descriptor.AcceptsArguments);
        Assert.True(descriptor.HasTokenBody);
    }

    [Fact]
    public void MacroFacts_PreservesDeclaredInvocableTargets()
    {
        var macro = new MemberTargetMacro();
        var expected = MacroInvocationTargets.NamespaceMember | MacroInvocationTargets.TypeMember;

        Assert.Equal(expected, MacroFacts.GetInvocationTargets(macro));
        Assert.Equal(expected, MacroFacts.GetDescriptor(macro).InvocationTargets);
    }

    public sealed class TestAttachedMacro : IAttachedDeclarationMacro
    {
        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => MacroExpansionResult.Empty;
    }

    public sealed class ObservableMacroParameters
    {
        public ObservableMacroParameters(string name, int count = 1)
        {
            Name = name;
            Count = count;
        }

        public string Name { get; }

        public int Count { get; }

        public bool Notify { get; init; } = true;
    }

    public sealed class TypedParameterAttachedMacro : IAttachedDeclarationMacro, IMacroDefinition<ObservableMacroParameters>
    {
        public string Name => "Observable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => MacroExpansionResult.Empty;
    }

    public sealed class ExpressionMacroParameters
    {
        public ExpressionMacroParameters(ExpressionSyntax expression)
        {
            Expression = expression;
        }

        public ExpressionSyntax Expression { get; }
    }

    public sealed class ExpressionParameterMacro : IInvocableMacro<ExpressionMacroParameters>
    {
        public string Name => "expression";

        public InvocableMacroExpansionResult Expand(
            InvocableMacroContext<ExpressionMacroParameters> context)
            => InvocableMacroExpansionResult.Empty;
    }

    public sealed class UnclassifiedMacro : IMacroDefinition
    {
        public string Name => "unclassified";
    }

    public sealed class TestTokenTreeMacro : ITokenTreeMacro
    {
        public string Name => "tokenTree";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.Empty;
    }

    public sealed class MemberTargetMacro : ITokenTreeMacro
    {
        public string Name => "members";
        public MacroInvocationTargets InvocationTargets =>
            MacroInvocationTargets.NamespaceMember | MacroInvocationTargets.TypeMember;

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.FromMembers(
                SyntaxFactory.List<MemberDeclarationSyntax>());
    }

    public sealed class AmbiguousMacro : IAttachedDeclarationMacro, ITokenTreeMacro
    {
        public string Name => "ambiguous";
        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => MacroExpansionResult.Empty;

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.Empty;
    }

    public sealed class MisleadingKindMacro : IAttachedDeclarationMacro
    {
        public string Name => "misleading";
        public MacroKind Kind => MacroKind.Invocable;
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
            .AddSyntaxTrees(macroTree)
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
}
