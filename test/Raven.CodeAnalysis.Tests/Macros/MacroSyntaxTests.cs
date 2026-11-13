using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Macros.Tests;

[Trait("Target", "Shared")]
public sealed class MacroSyntaxTests
{
    [Fact]
    public void MacroSyntax_ProvidesStructuralAndFactoryForms()
    {
        var syntax = SyntaxFactory.ParseExpression("left + right")!;

        var structure = MacroSyntax.GetStructure(syntax);
        var factory = MacroSyntax.GetFactoryForm(syntax);

        Assert.Contains("AddExpression", structure);
        Assert.Contains("SyntaxFactory", factory);
        Assert.Contains("IdentifierName", factory);
    }
}
