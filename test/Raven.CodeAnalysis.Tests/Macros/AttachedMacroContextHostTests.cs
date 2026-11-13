using System;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "DotNet")]
public sealed class AttachedMacroContextHostTests
{
    [Fact]
    public void PublicConstructorRetainsHostAndResolvesArgumentType()
    {
        var tree = SyntaxTree.ParseText("[Example(42)] class Model {}");
        var compilation = Compilation.Create("AttachedContext", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default).AddSyntaxTrees(tree);
        var model = compilation.GetSemanticModel(tree);
        var attribute = Assert.Single(tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>());
        var target = tree.GetRoot().Members[0];
        var context = new AttachedMacroContext(compilation, model, attribute, target, target);
        Assert.Same(compilation, context.Compilation);
        Assert.Same(model, context.SemanticModel);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(context.Arguments).SemanticType!.SpecialType);
        Assert.Equal("syntax", Assert.Throws<ArgumentNullException>(() => new AttachedMacroContext(null!, null!, null!, null!, null!)).ParamName);
        Assert.Equal("compilation", Assert.Throws<ArgumentNullException>(() => new AttachedMacroContext(null!, model, attribute, target, target)).ParamName);
        Assert.Equal("semanticModel", Assert.Throws<ArgumentNullException>(() => new AttachedMacroContext(compilation, null!, attribute, target, target)).ParamName);
    }
}
