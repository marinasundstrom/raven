using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class FieldAddressCapabilityTests
{
    [Fact]
    public void NestedValueMutationRequiresFieldAddressCapability()
    {
        var app = Compilation.Create("FieldAddressAdmission", [SyntaxTree.ParseText("""
            struct Counter {
                public field Value: int
                public init(value: int) {
                    self.Value = value
                }
                func Increment() -> int {
                    Value = Value + 1
                    return Value
                }
            }
            class Holder<T> {
                public field Value: T
                public init(value: T) {
                    self.Value = value
                }
            }
            static class Consumer {
                static func Run() -> int {
                    let holder = Holder<Counter>(Counter(40))
                    let alias = holder
                    holder.Value.Increment()
                    alias.Value.Increment()
                    return holder.Value.Value
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        EmissionCapabilities Capabilities(bool addresses) => new(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>().Where(kind => addresses || kind != LinearInstructionKind.FieldAddress),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsRootClassLocals: true, allowsManagedReferences: true,
            allowsGenericClassOwners: true, allowsGenericInstanceMethods: true, allowsConstructedFieldReferences: true);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Last();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
    }
}
