using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests.Symbols;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroParameterRoleContractTests
{
    [Theory]
    [InlineData(typeof(int), MacroParameterRole.Value)]
    [InlineData(typeof(SyntaxNode), MacroParameterRole.Value)]
    [InlineData(typeof(IdentifierNameSyntax), MacroParameterRole.SyntaxInput)]
    [InlineData(typeof(ExpressionSyntax<int>), MacroParameterRole.SyntaxInput)]
    [InlineData(typeof(FreestandingMacroDeclarationSyntax), MacroParameterRole.DeclarationInput)]
    [InlineData(typeof(IMacroTokenStream), MacroParameterRole.TokenBody)]
    [InlineData(typeof(AttachedMacroContext), MacroParameterRole.Context)]
    [InlineData(typeof(FreestandingMacroContext), MacroParameterRole.Context)]
    [InlineData(typeof(TokenTreeMacroContext), MacroParameterRole.Context)]
    public void ReflectionRolesPreserveInputCategories(Type type, MacroParameterRole expected)
        => Assert.Equal(expected, MacroParameterRoleFacts.GetRole(type));

    [Fact]
    public void ContextKindsAgreeForReflectedAndSymbolTypes()
    {
        foreach (var (type, kind) in new[] {
            (typeof(AttachedMacroContext), MacroContextKind.Attached),
            (typeof(FreestandingMacroContext), MacroContextKind.Freestanding),
            (typeof(TokenTreeMacroContext), MacroContextKind.TokenTree) })
        {
            var symbol = Named(type.Namespace!, type.Name);
            var derived = Named("Example", "Derived", symbol);
            Assert.Equal(kind, MacroParameterRoleFacts.GetContextKind(type));
            Assert.Equal(kind, MacroParameterRoleFacts.GetContextKind(symbol));
            Assert.Equal(kind, MacroParameterRoleFacts.GetContextKind(derived));
            Assert.Equal(MacroParameterRole.Context, MacroParameterRoleFacts.GetRole(derived));
        }
        Assert.Equal(MacroContextKind.None, MacroParameterRoleFacts.GetContextKind(typeof(object)));
    }

    [Fact]
    public void SymbolRolesRespectInheritanceInterfacesAndFacadeArity()
    {
        var syntax = Named("Raven.CodeAnalysis.Syntax", "SyntaxNode");
        var expression = Named("Raven.CodeAnalysis.Syntax", "ExpressionSyntax", syntax);
        var derived = Named("Example", "Expression", expression);
        Assert.Equal(MacroParameterRole.SyntaxInput, MacroParameterRoleFacts.GetRole(derived));
        Assert.False(MacroParameterRoleFacts.IsAttachedTargetType(derived));
        Assert.True(MacroParameterRoleFacts.IsAttachedTargetType(Named("Example", "Declaration", syntax)));
        Assert.Equal(MacroParameterRole.DeclarationInput,
            MacroParameterRoleFacts.GetRole(Named("Raven.CodeAnalysis.Syntax", "FreestandingMacroDeclarationSyntax")));
        var stream = Named("Raven.CodeAnalysis.Macros", "IMacroTokenStream");
        var implementation = Named("Example", "Stream");
        MetadataSymbolFixture.Set(implementation, "get_AllInterfaces", ImmutableArray.Create(stream));
        Assert.Equal(MacroParameterRole.TokenBody, MacroParameterRoleFacts.GetRole(stream));
        Assert.Equal(MacroParameterRole.TokenBody, MacroParameterRoleFacts.GetRole(implementation));
        var facade = Named("Raven.CodeAnalysis.Macros", "ExpressionSyntax");
        Assert.False(MacroParameterRoleFacts.IsExpressionSyntaxFacade(facade));
        MetadataSymbolFixture.Set(facade, "get_Arity", 1);
        Assert.True(MacroParameterRoleFacts.IsExpressionSyntaxFacade(facade));
        Assert.Equal(MacroParameterRole.SyntaxInput, MacroParameterRoleFacts.GetRole(facade));
    }

    [Theory]
    [InlineData("Other.Raven.CodeAnalysis.Macros")]
    [InlineData("CodeAnalysis.Macros")]
    [InlineData("Raven.CodeAnalysis.macros")]
    [InlineData("")]
    public void NamespaceMatchesAreExact(string namespaceName)
    {
        var type = Named(namespaceName, "AttachedMacroContext");
        Assert.Equal(MacroContextKind.None, MacroParameterRoleFacts.GetContextKind(type));
        Assert.Equal(MacroParameterRole.Value, MacroParameterRoleFacts.GetRole(type));
    }

    [Theory]
    [InlineData(MacroParameterRole.SyntaxInput, "Raven.CodeAnalysis.Syntax.ExpressionSyntax")]
    [InlineData(MacroParameterRole.Context, "Raven.CodeAnalysis.Macros.MacroContext")]
    [InlineData(MacroParameterRole.Value, "object")]
    [InlineData(MacroParameterRole.TokenBody, "Raven.CodeAnalysis.Macros.IMacroTokenStream")]
    [InlineData(MacroParameterRole.DeclarationInput, "Raven.CodeAnalysis.Syntax.FreestandingMacroDeclarationSyntax")]
    public void LoweredNamesRetainAuthoredAnnotationsAndRoleDefaults(MacroParameterRole role, string fallback)
    {
        ParameterSyntax Parameter(string text) => SyntaxTree.ParseText($"func F({text}) {{}}")
            .GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        Assert.Equal(fallback, MacroParameterRoleFacts.GetLoweredTypeName(Parameter("value"), role));
        var preservesAnnotation = role is MacroParameterRole.SyntaxInput or MacroParameterRole.Context or MacroParameterRole.Value;
        Assert.Equal(preservesAnnotation ? "Custom" : fallback,
            MacroParameterRoleFacts.GetLoweredTypeName(Parameter("value: Custom"), role));
    }

    private static NamedTypeSymbol Named(string namespaceName, string name, NamedTypeSymbol? baseType = null)
    {
        var global = MetadataSymbolFixture.Make<NamespaceSymbol>(SymbolKind.Namespace, "");
        MetadataSymbolFixture.Set(global, "get_IsGlobalNamespace", true);
        NamespaceSymbol parent = global;
        foreach (var segment in namespaceName.Split('.', StringSplitOptions.RemoveEmptyEntries))
        {
            var next = MetadataSymbolFixture.Make<NamespaceSymbol>(SymbolKind.Namespace, segment);
            MetadataSymbolFixture.Set(next, "get_IsGlobalNamespace", false);
            MetadataSymbolFixture.Set(next, "get_ContainingNamespace", parent);
            parent = next;
        }
        var type = MetadataSymbolFixture.Make<NamedTypeSymbol>(SymbolKind.Type, name, TypeKind.Class);
        MetadataSymbolFixture.Set(type, "get_ContainingNamespace", parent);
        MetadataSymbolFixture.Set(type, "get_BaseType", baseType);
        MetadataSymbolFixture.Set(type, "get_Arity", 0);
        MetadataSymbolFixture.Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return type;
    }
}
