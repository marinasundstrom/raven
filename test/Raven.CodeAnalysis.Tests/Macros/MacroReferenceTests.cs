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
        var macroTree = SyntaxTree.ParseText("""
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            class InMemoryMacroPlugin : IRavenMacroPlugin {
                val Name: string => "InMemory"

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
        var codeAnalysisReference = MetadataReference.CreateFromFile(typeof(IRavenMacroPlugin).Assembly.Location);
        var macroCompilation = Compilation.Create(
                "InMemoryMacros",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(macroTree)
            .AddReferences([.. TestMetadataReferences.Default, codeAnalysisReference]);

        using var macroImage = new MemoryStream();
        var macroEmit = macroCompilation.Emit(macroImage);
        Assert.True(macroEmit.Success, string.Join(System.Environment.NewLine, macroEmit.Diagnostics));

        var consumerTree = SyntaxTree.ParseText("func Main() -> int => #answer { }");
        var consumerCompilation = Compilation.Create(
                "Consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(consumerTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(MacroReference.CreateFromImage(
                macroImage.ToArray(),
                display: "same-project macro partition"));

        Assert.DoesNotContain(
            consumerCompilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
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
}
