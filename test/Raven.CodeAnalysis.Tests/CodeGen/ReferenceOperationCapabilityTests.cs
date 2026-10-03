using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ReferenceOperationCapabilityTests
{
    [Theory]
    [InlineData("ToString", true)]
    [InlineData("GetHashCode", false)]
    public void ObjectDisplayDispatchRequiresExplicitCapability(string member, bool admitted)
    {
        var result = member == "ToString" ? "string?" : "int";
        var app = Compilation.Create("ObjectDispatchAdmission", [SyntaxTree.ParseText($"static class Consumer {{ static func Read(value: object) -> {result} => value.{member}() }}")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        EmissionCapabilities Capabilities(bool dispatch) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true, allowsExternalInstanceCalls: true, allowsObjectDisplayDispatch: dispatch);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.Equal(admitted, plan.TryLowerBody(app, _ => false, out _, out _, Capabilities(true)));
    }

    [Fact]
    public void NullComparisonAndDiscardTypeTestRequireCapabilities()
    {
        var app = Compilation.Create("ReferenceAdmission", [SyntaxTree.ParseText("""
            static class Consumer {
                static func Read(value: object?) -> int {
                    if value == null {
                        return 3
                    }
                    if value is string {
                        return 42
                    }
                    return 4
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        EmissionCapabilities Capabilities(LinearInstructionKind? excluded = null) => new(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>().Where(kind => kind != excluded),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities()));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(LinearInstructionKind.ReferenceIsNull)));
        Assert.False(plan.TryLowerBody(app, _ => false, out _, out _, Capabilities(LinearInstructionKind.TypeTest)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities()), failure?.Detail);
    }
}
