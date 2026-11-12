using Raven.CodeAnalysis.Symbols;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SymbolRegistrationIntegrationTests
{
    [Fact]
    public void SourceNamespace_PreservesConstructorRegistration()
    {
        var compilation = Compilation.Create("Registration");
        var owner = new SourceNamespaceSymbol("Owner", compilation.Assembly, null, [], []);
        var child = new SourceNamespaceSymbol("Child", owner, owner, [], []);

        Assert.Same(child, Assert.Single(owner.GetMembers()));
    }
}
