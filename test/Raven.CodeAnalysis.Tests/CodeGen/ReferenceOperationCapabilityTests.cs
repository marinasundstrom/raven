using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ReferenceOperationCapabilityTests
{
    [Fact]
    public void ConfiguredUnitStorageUsesNominalDefault()
    {
        var app = Compilation.Create("UnitStorageAdmission", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Fill(out value: System.ValueTuple) -> bool {
                    value = ()
                    return true
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                .WithTargetCoreAssemblyName("System.Runtime")
                .WithRuntimeUnitContract(new RuntimeUnitContract("System.Runtime", "System.ValueTuple")));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        EmissionCapabilities Capabilities(bool defaults) => new(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>().Where(kind => defaults || kind != LinearInstructionKind.DefaultValue),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsManagedReferences: true, allowsExternalValueSignatures: true);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new System.Runtime.Loader.AssemblyLoadContext("UnitStorageAdmission", isCollectible: true);
        try
        {
            image.Position = 0;
            var assembly = context.LoadFromStream(image);
            object?[] arguments = [null];
            Assert.Equal(true, assembly.GetType("Consumer")!.GetMethod("Fill")!.Invoke(null, arguments));
            Assert.IsType<ValueTuple>(arguments[0]);
        }
        finally { context.Unload(); }
    }

    [Theory]
    [InlineData("object?")]
    [InlineData("string?")]
    public void NullReturnUsesSupportedReferenceDefault(string type)
    {
        var app = Compilation.Create("NullAdmission", [SyntaxTree.ParseText($"static class Consumer {{ static func Empty() -> {type} => null }}")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        EmissionCapabilities Capabilities(bool defaults) => new(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>().Where(kind => defaults || kind != LinearInstructionKind.DefaultValue),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
    }

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
