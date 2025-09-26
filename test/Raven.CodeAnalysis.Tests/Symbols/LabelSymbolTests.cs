using System;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests;

public class LabelSymbolTests : CompilationTestBase
{
    [Fact]
    public void LabelSymbol_MetadataNameMatchesName()
    {
        var compilation = CreateCompilation();

        var labelSymbol = CreateLabelSymbol(
            name: "start",
            containingSymbol: compilation.Module,
            containingNamespace: compilation.GlobalNamespace);

        Assert.Equal(SymbolKind.Label, labelSymbol.Kind);
        Assert.Equal("start", labelSymbol.Name);
        Assert.Equal("start", labelSymbol.MetadataName);
        Assert.True(labelSymbol.CanBeReferencedByName);
    }

    [Fact]
    public void LabelSymbol_ToDisplayString_UsesName()
    {
        var compilation = CreateCompilation();

        var labelSymbol = CreateLabelSymbol(
            name: "loop",
            containingSymbol: compilation.Module,
            containingNamespace: compilation.GlobalNamespace);

        var display = labelSymbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        Assert.Equal("loop", display);
    }

    private static ILabelSymbol CreateLabelSymbol(
        string name,
        ISymbol containingSymbol,
        INamespaceSymbol? containingNamespace)
    {
        var type = typeof(Compilation).Assembly.GetType(
            "Raven.CodeAnalysis.Symbols.LabelSymbol",
            throwOnError: true)!;

        var instance = Activator.CreateInstance(
            type,
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
            binder: null,
            args: new object?[]
            {
                name,
                containingSymbol,
                null,
                containingNamespace,
                Array.Empty<Location>(),
                Array.Empty<SyntaxReference>()
            },
            culture: null);

        return Assert.IsAssignableFrom<ILabelSymbol>(instance);
    }
}
