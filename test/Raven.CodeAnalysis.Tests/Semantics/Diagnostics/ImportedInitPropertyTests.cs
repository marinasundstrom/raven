using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

[Trait("Target", "DotNet")]
public sealed class ImportedInitPropertyTests
{
    [Fact]
    public void MetadataInitSetter_PreservesAccessorKind()
    {
        var compilation = Create("");
        var type = compilation.GetTypeByMetadataName(typeof(ImportedInitPropertyFixture).FullName!)!;
        var property = Assert.Single(type.GetMembers("Value").OfType<IPropertySymbol>());
        Assert.Equal(MethodKind.InitOnly, property.SetMethod!.MethodKind);
        Assert.Equal(MethodKind.PropertySet,
            Assert.Single(type.GetMembers("Mutable").OfType<IPropertySymbol>()).SetMethod!.MethodKind);
    }

    [Fact]
    public void ImportedInitProperty_IsWritableInObjectInitializer()
    {
        var compilation = Create("""
            func Build() -> ImportedInitPropertyFixture => ImportedInitPropertyFixture() { Value = 42 }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("value.Value = 1", true)]
    [InlineData("value.Value += 1", true)]
    [InlineData("value.Value++", true)]
    [InlineData("++value.Value", true)]
    [InlineData("1 |> value.Value", true)]
    [InlineData("value.Mutable = 1", false)]
    [InlineData("value.Mutable += 1", false)]
    [InlineData("value.Mutable++", false)]
    public void ImportedInitWrites_RequireConstruction(string statement, bool rejected)
    {
        var compilation = Create($$"""
            func Update(value: ImportedInitPropertyFixture) {
                {{statement}}
            }
            """);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (rejected)
            Assert.Equal("RAV0200", Assert.Single(errors).Id);
        else
            Assert.Empty(errors);
    }

    [Theory]
    [InlineData("self.Value = 1", false)]
    [InlineData("base.Value = 1", false)]
    [InlineData("other.Value = 1", true)]
    public void DerivedConstructor_PreservesReceiverRestriction(string assignment, bool rejected)
    {
        var compilation = Create($$"""
            class Child : ImportedInitPropertyFixture {
                init(other: ImportedInitPropertyFixture) { {{assignment}} }
            }
            """);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (rejected)
            Assert.Equal("RAV0200", Assert.Single(errors).Id);
        else
            Assert.Empty(errors);
    }

    private static Compilation Create(string source)
        => Compilation.Create("ImportedInitConsumer",
            [SyntaxTree.ParseText("import Raven.CodeAnalysis.Tests.Semantics.Diagnostics.*\n" + source)],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(ImportedInitPropertyFixture).Assembly.Location)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
}

public class ImportedInitPropertyFixture
{
    public int Value { get; init; }
    public int Mutable { get; set; }
}
