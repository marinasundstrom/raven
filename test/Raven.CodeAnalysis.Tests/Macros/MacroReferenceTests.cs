using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroReferenceTests
{
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
