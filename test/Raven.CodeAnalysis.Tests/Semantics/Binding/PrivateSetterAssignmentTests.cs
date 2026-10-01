using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics;

public class PrivateSetterAssignmentTests
{
    private const string Declaration = """
        class Gauge {
            private var amount: int
            init() { amount = 0 }
            val Amount: int {
                get => amount
                private set => amount = value
            }
            func Reset() {
                Amount = 1
                self.Amount = 2
                Amount += 1
                self.Amount += 1
                Amount++
                self.Amount++
            }
        }
        """;

    [Fact]
    public void ValWithAccessibleSetterAllowsWritesWithoutChangingPublicMutability()
    {
        var compilation = Create(Declaration);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var gauge = compilation.GetTypeByMetadataName("Gauge")!;
        var property = (IPropertySymbol)gauge.GetMembers("Amount").Single();
        Assert.False(property.IsMutable);
        Assert.Equal(Accessibility.Private, property.SetMethod!.DeclaredAccessibility);
    }

    [Theory]
    [InlineData("gauge.Amount = 1")]
    [InlineData("gauge.Amount += 1")]
    [InlineData("gauge.Amount++")]
    public void PrivateSetterDoesNotAllowOutsideWrites(string assignment)
    {
        var compilation = Create(Declaration + "\nfunc Change() { let gauge = Gauge()\n" + assignment + "\n}");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0200" && d.Severity == DiagnosticSeverity.Error);
    }

    private static Compilation Create(string source) => Compilation.Create("PrivateSetter", [SyntaxTree.ParseText(source)],
        TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
}
