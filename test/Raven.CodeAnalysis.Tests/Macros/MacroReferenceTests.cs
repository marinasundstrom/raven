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
    public void MacroReference_FromAssembly_FindsMacroPlugin()
    {
        var reference = new MacroReference(typeof(TestMacroPlugin).Assembly);

        var plugin = Assert.Single(reference.GetPlugins().OfType<TestMacroPlugin>());
        var macro = Assert.Single(plugin.GetMacros().OfType<TestAttachedMacro>());

        Assert.Equal("TestMacros", plugin.Name);
        Assert.Equal("AddEquatable", macro.Name);
        Assert.Equal(MacroKind.AttachedDeclaration, macro.Kind);
        Assert.Equal(MacroTarget.Type, macro.Targets);
    }

    [Fact]
    public void MacroReference_FromType_RejectsNonPluginTypes()
    {
        var ex = Assert.Throws<System.ArgumentException>(() => new MacroReference(typeof(MacroReferenceTests)));
        Assert.Contains("IRavenMacroPlugin", ex.Message);
    }

    [Fact]
    public void MacroReference_FromInMemoryRavenAssembly_ExpandsMacro()
    {
        var macroImage = EmitMacroAssembly("""
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(InMemoryMacroPlugin))]

            class InMemoryMacroPlugin : IRavenMacroPlugin {
                val Name: string => "InMemory"

                func GetMacros() -> ImmutableArray<IMacroDefinition>
                    => [AnswerMacro()]
            }

            class UnselectedMacroPlugin : IRavenMacroPlugin {
                val Name: string => "Unselected"

                func GetMacros() -> ImmutableArray<IMacroDefinition>
                    => [AnswerMacro()]
            }

            class AnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Targets: MacroTarget => MacroTarget.None

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    FreestandingMacroExpansionResult {
                        Expression = #quote { 42 }
                    }
                }
            }
            """);
        var reference = MacroReference.CreateFromImage(
            macroImage,
            display: "same-project macro partition");

        var plugin = Assert.Single(reference.GetPlugins());
        Assert.Equal("InMemory", plugin.Name);

        var consumerTree = SyntaxTree.ParseText("func Main() -> int => #answer { }");
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
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin]

            class FirstPlugin : IRavenMacroPlugin {
                val Name: string => "First"
                func GetMacros() -> ImmutableArray<IMacroDefinition> => [EmptyMacro()]
            }

            class SecondPlugin : IRavenMacroPlugin {
                val Name: string => "Second"
                func GetMacros() -> ImmutableArray<IMacroDefinition> => [EmptyMacro()]
            }

            class EmptyMacro : ITokenTreeExpressionMacro {
                val Name: string => "empty"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Targets: MacroTarget => MacroTarget.None

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }
            """);

        var plugins = MacroReference.CreateFromImage(macroImage)
            .GetPlugins()
            .Select(static plugin => plugin.Name)
            .Order()
            .ToArray();

        Assert.Equal(["First", "Second"], plugins);
    }

    [Fact]
    public void MacroReference_ExplicitManifestSupportsMultipleEntryPoints()
    {
        var macroImage = EmitMacroAssembly("""
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(FirstPlugin))]
            [assembly: RavenCompilerPlugin(typeof(SecondPlugin))]

            class FirstPlugin : IRavenMacroPlugin {
                val Name: string => "First"
                func GetMacros() -> ImmutableArray<IMacroDefinition> => [EmptyMacro()]
            }

            class UnselectedPlugin : IRavenMacroPlugin {
                val Name: string => "Unselected"
                func GetMacros() -> ImmutableArray<IMacroDefinition> => [EmptyMacro()]
            }

            class SecondPlugin : IRavenMacroPlugin {
                val Name: string => "Second"
                func GetMacros() -> ImmutableArray<IMacroDefinition> => [EmptyMacro()]
            }

            class EmptyMacro : ITokenTreeExpressionMacro {
                val Name: string => "empty"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Targets: MacroTarget => MacroTarget.None

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult
                    => FreestandingMacroExpansionResult.Empty
            }
            """);

        var plugins = MacroReference.CreateFromImage(macroImage)
            .GetPlugins()
            .Select(static plugin => plugin.Name)
            .ToArray();

        Assert.Equal(["First", "Second"], plugins);
    }

    [Fact]
    public void MacroReference_FromFile_UsesExplicitEntryPointManifest()
    {
        var macroImage = EmitMacroAssembly("""
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            [assembly: RavenCompilerPlugin(typeof(SelectedPlugin))]

            class SelectedPlugin : IRavenMacroPlugin {
                val Name: string => "Selected"
                func GetMacros() -> ImmutableArray<IMacroDefinition> => [EmptyMacro()]
            }

            class UnselectedPlugin : IRavenMacroPlugin {
                val Name: string => "Unselected"
                func GetMacros() -> ImmutableArray<IMacroDefinition> => [EmptyMacro()]
            }

            class EmptyMacro : ITokenTreeExpressionMacro {
                val Name: string => "empty"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Targets: MacroTarget => MacroTarget.None

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

            var plugin = Assert.Single(MacroReference.CreateFromFile(assemblyPath).GetPlugins());

            Assert.Equal("Selected", plugin.Name);
        }
        finally
        {
            File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void MacroReference_InvalidDeclaredEntryPoint_ReportsLoadDiagnostic()
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
        Assert.Contains(nameof(IRavenMacroPlugin), diagnostic.GetMessage());
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

    public sealed class TestMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "TestMacros";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new TestAttachedMacro()];
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

    private static byte[] EmitMacroAssembly(string source)
    {
        var macroTree = SyntaxTree.ParseText(source);
        var codeAnalysisReference = MetadataReference.CreateFromFile(typeof(IRavenMacroPlugin).Assembly.Location);
        var macroCompilation = Compilation.Create(
                $"InMemoryMacros_{System.Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(macroTree)
            .AddReferences([.. TestMetadataReferences.Default, codeAnalysisReference]);

        using var macroImage = new MemoryStream();
        var macroEmit = macroCompilation.Emit(macroImage);
        Assert.True(macroEmit.Success, string.Join(System.Environment.NewLine, macroEmit.Diagnostics));
        return macroImage.ToArray();
    }
}
