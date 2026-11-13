using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Macros.Tests;

[Trait("Target", "Shared")]
public sealed class MacroArgumentSemanticModelTests
{
    [Fact]
    public void PublicConstructorsRetainNullValidationOrderAndParameterNames()
    {
        Assert.Equal("syntax", Assert.Throws<ArgumentNullException>(() => new MacroArgument((ExpressionSyntax)null!, (SemanticModel)null!)).ParamName);
        Assert.Equal("syntax", Assert.Throws<ArgumentNullException>(() => new MacroArgument((ArgumentSyntax)null!, (SemanticModel)null!)).ParamName);
        Assert.Equal("semanticModel", Assert.Throws<ArgumentNullException>(() => new MacroArgument(SyntaxFactory.ParseExpression("42"), (SemanticModel)null!)).ParamName);
    }

    [Fact]
    public void ServicesRemainInternalAndPublicConstructorsKeepSemanticModelParameters()
    {
        var constructors = typeof(MacroArgument).GetConstructors();
        Assert.Equal(2, constructors.Length);
        Assert.All(constructors, constructor => Assert.Equal(typeof(SemanticModel), constructor.GetParameters()[1].ParameterType));
        Assert.Null(typeof(MacroArgument).GetProperty("Services"));
    }
}
