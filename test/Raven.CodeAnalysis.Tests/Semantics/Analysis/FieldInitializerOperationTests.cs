using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class FieldInitializerOperationTests
{
    [Theory]
    [InlineData(false, false, "val")]
    [InlineData(false, true, "val")]
    [InlineData(true, false, "val")]
    [InlineData(true, true, "val")]
    [InlineData(false, false, "field")]
    [InlineData(false, true, "field")]
    [InlineData(true, false, "field")]
    [InlineData(true, true, "field")]
    [Trait("Target", "DotNet")]
    public void DotNet_InitializerOperationAgreesWithSymbolAndType(bool isStatic, bool diagnosticsFirst, string keyword) => Check(TargetPlatform.DotNet, isStatic, diagnosticsFirst, keyword);

    [Theory]
    [InlineData(false, false, "val")]
    [InlineData(false, true, "val")]
    [InlineData(true, false, "val")]
    [InlineData(true, true, "val")]
    [InlineData(false, false, "field")]
    [InlineData(false, true, "field")]
    [InlineData(true, false, "field")]
    [InlineData(true, true, "field")]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_InitializerOperationAgreesWithSymbolAndType(bool isStatic, bool diagnosticsFirst, string keyword) => Check(TargetPlatform.NeoCLR, isStatic, diagnosticsFirst, keyword);

    [Theory]
    [InlineData("int", "System.Math.Abs(System.Math.Min(-1,-2))")]
    [InlineData("System.Func<int>", "func() => System.Math.Abs(-1)")]
    [Trait("Target", "DotNet")]
    public void DotNet_NestedInitializerQueriesRetainContext(string type, string expression) => CheckNested(TargetPlatform.DotNet, type, expression);

    [Theory]
    [InlineData("int", "System.Math.Abs(System.Math.Min(-1,-2))")]
    [InlineData("System.Func<int>", "func() => System.Math.Abs(-1)")]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_NestedInitializerQueriesRetainContext(string type, string expression) => CheckNested(TargetPlatform.NeoCLR, type, expression);

    private static void CheckNested(TargetPlatform target, string type, string expression)
    {
        var tree = SyntaxTree.ParseText("class Probe { static val Value: " + type + " = " + expression + " }");
        var compilation = Compilation.Create("NestedFieldOperation", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>())
        {
            var operation = Assert.IsAssignableFrom<IInvocationOperation>(model.GetOperation(syntax));
            Assert.Equal(SpecialType.System_Int32, operation.Type?.SpecialType);
            Assert.Equal(model.GetSymbolInfo(syntax).Symbol, operation.TargetMethod);
            Assert.Same(operation, model.GetOperation(syntax));
        }
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void EditingInitializerPreservesOldSnapshotOperation()
    {
        var before = SyntaxTree.ParseText("class Probe { val Value: int = System.Math.Abs(-1) }", path: "main.rvn");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var previous = Compilation.Create("Edit", [before], TestMetadataReferences.Default, options);
        var oldSyntax = before.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var oldOperation = Assert.IsAssignableFrom<IInvocationOperation>(previous.GetSemanticModel(before).GetOperation(oldSyntax));
        var after = SyntaxTree.ParseText("class Probe { val Value: int = System.Math.Max(2,3) }", path: "main.rvn");
        var current = Compilation.Create("Edit", [after], TestMetadataReferences.Default, options);
        current.AdoptIncrementalReuseFrom(previous);
        var newSyntax = after.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var newOperation = Assert.IsAssignableFrom<IInvocationOperation>(current.GetSemanticModel(after).GetOperation(newSyntax));
        Assert.Equal("Max", newOperation.TargetMethod.Name);
        Assert.Equal("Abs", oldOperation.TargetMethod.Name);
        Assert.Same(oldOperation, previous.GetSemanticModel(before).GetOperation(oldSyntax));
        Assert.NotSame(oldOperation, newOperation);
    }

    [Theory]
    [InlineData("val")]
    [InlineData("field")]
    [Trait("Target", "DotNet")]
    public void OperationQueryDoesNotSuppressInitializerErrors(string keyword)
    {
        var tree = SyntaxTree.ParseText("class Probe { " + keyword + " Value: int = \"bad\" }");
        var compilation = Compilation.Create("InvalidInitializer", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().Single();
        Assert.NotNull(model.GetOperation(syntax));
        Assert.Contains(model.GetDiagnostics(), d => d.Id == "RAV1504");
        Assert.Contains(model.GetDiagnostics(), d => d.Id == "RAV1504");
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_InvalidFieldInitializerReportsAssignmentConversion()
    {
        var tree = SyntaxTree.ParseText("class Probe { field Value: int = \"bad\" }");
        var compilation = Compilation.Create("InvalidNativeField", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        Assert.Contains(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Id == "RAV1504");
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void ReferencedReturnTypeEditInvalidatesInitializerOperation()
    {
        var tree = SyntaxTree.ParseText("class Probe { val Value = Factory.Make() }", path: "probe.rvn");
        var beforeFactory = SyntaxTree.ParseText("class Factory { static func Make()->int => 1 }", path: "factory.rvn");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var previous = Compilation.Create("ContextEdit", [tree, beforeFactory], TestMetadataReferences.Default, options);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var oldOperation = Assert.IsAssignableFrom<IInvocationOperation>(previous.GetSemanticModel(tree).GetOperation(syntax));
        Assert.Equal(SpecialType.System_Int32, oldOperation.Type?.SpecialType);
        var afterFactory = SyntaxTree.ParseText("class Factory { static func Make()->string => \"new\" }", path: "factory.rvn");
        var current = Compilation.Create("ContextEdit", [tree, afterFactory], TestMetadataReferences.Default, options);
        current.AdoptIncrementalReuseFrom(previous);
        var newOperation = Assert.IsAssignableFrom<IInvocationOperation>(current.GetSemanticModel(tree).GetOperation(syntax));
        Assert.Equal(SpecialType.System_String, newOperation.Type?.SpecialType);
        Assert.Equal(SpecialType.System_Int32, oldOperation.Type?.SpecialType);
        Assert.NotSame(oldOperation, newOperation);
        Assert.DoesNotContain(current.GetSemanticModel(tree).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    private static void Check(TargetPlatform target, bool isStatic, bool diagnosticsFirst, string keyword)
    {
        var tree = SyntaxTree.ParseText("class Probe { " + (isStatic ? "static " : "") + keyword + " Value: int = System.Math.Abs(-1) }");
        var compilation = Compilation.Create("FieldOperation", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var firstOperation = Assert.IsAssignableFrom<IInvocationOperation>(model.GetOperation(syntax));
        Assert.Equal("Abs", Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(syntax).Symbol).Name);
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(syntax).Type?.SpecialType);
        var operation = Assert.IsAssignableFrom<IInvocationOperation>(model.GetOperation(syntax));
        Assert.Equal("Abs", operation.TargetMethod.Name);
        Assert.Same(firstOperation, operation);
        Assert.Same(operation, model.GetOperation(syntax));
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
