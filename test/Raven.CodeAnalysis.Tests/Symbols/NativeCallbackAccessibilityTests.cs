using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public sealed class NativeCallbackAccessibilityTests
{
    [Theory]
    [InlineData(false, false, true)]
    [InlineData(true, false, false)]
    [InlineData(true, true, true)]
    public void CallbackTransportChecksComponentAccessibility(bool native, bool hidden, bool expected)
    {
        var options = native ? new CompilationOptions().WithTargetPlatform(TargetPlatform.NeoCLR) : new CompilationOptions();
        var compilation = Compilation.Create("Callbacks", [], TestMetadataReferences.Default, options);
        var component = new SourceNamedTypeSymbol("Item", null!, TypeKind.Class, compilation.Assembly,
            null, compilation.SourceGlobalNamespace, [], [], declaredAccessibility: hidden ? Accessibility.Internal : Accessibility.Public);
        var callback = new SynthesizedDelegateTypeSymbol(compilation, "Callback", [component], [RefKind.None], component, compilation.SourceGlobalNamespace);
        Assert.Equal(expected, AccessibilityUtilities.IsTypeLessAccessibleThan(callback, Accessibility.Public));
    }
}
