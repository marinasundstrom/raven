using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ConstructorParameterIdentityTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    [Trait("Target", "DotNet")]
    public void DotNet_ConstructorParameterRetainsDeclaredIdentity(bool constructorFirst, bool diagnosticsFirst)
        => Check(constructorFirst, diagnosticsFirst, TargetPlatform.DotNet);

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_ConstructorParameterRetainsDeclaredIdentity(bool constructorFirst, bool diagnosticsFirst)
        => Check(constructorFirst, diagnosticsFirst, TargetPlatform.NeoCLR);

    private static void Check(bool constructorFirst, bool diagnosticsFirst, TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("class Probe { init(value:int=42) {} }");
        var compilation = Compilation.Create("ParameterIdentity", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = compilation.GetSemanticModel(tree);
        var constructor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        MethodSymbol? method = constructorFirst ? Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(constructor)) : null;
        var parameter = Assert.IsAssignableFrom<ParameterSymbol>(model.GetDeclaredSymbol(constructor.ParameterList.Parameters.Single()));
        method ??= Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(constructor));
        Assert.Same(Assert.Single(method.Parameters), parameter);
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal(42, parameter.ExplicitDefaultValue);
        Assert.Same(method, parameter.ContainingSymbol);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Same(parameter, model.GetDeclaredSymbol(constructor.ParameterList.Parameters.Single()));
        Assert.Same(parameter, Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(constructor)).Parameters.Single());
    }
}

public sealed class ParameterSignatureReuseTests
{
    [Theory]
    [InlineData("class Probe { init(value:string?=null) {} }")]
    [InlineData("class Probe { init(values:int[]) {} }")]
    [InlineData("class Probe<T> { init(value:T) {} }")]
    [InlineData("class Probe { init(value:(one: int, two: int)) {} }")]
    [InlineData("class Probe { init(callback: (int) -> int) {} }")]
    [InlineData("class Probe { init(value: int | string) {} }")]
    [InlineData("class Probe { init(value:System.Collections.Generic.List<int>) {} }")]
    [InlineData("class Probe { func M(value:int=42) {} }")]
    [InlineData("class Probe { func M<T>(value:T) {} }")]
    [InlineData("class Probe { static func +(left:Probe,right:Probe)->Probe => left }")]
    [InlineData("class Probe { static func explicit(value:Probe)->int => 0 }")]
    [InlineData("class Probe { init(ref value:int) {} }")]
    [Trait("Target", "DotNet")]
    public void DotNet_UnchangedSignatureKeepsParametersAcrossDiagnostics(string source) => CheckSignature(source, TargetPlatform.DotNet);

    [Theory]
    [InlineData("class Probe { init(value:string?=null) {} }")]
    [InlineData("class Probe { init(values:int[]) {} }")]
    [InlineData("class Probe<T> { init(value:T) {} }")]
    [InlineData("class Probe { init(value:(one: int, two: int)) {} }")]
    [InlineData("class Probe { init(callback: (int) -> int) {} }")]
    [InlineData("class Probe { init(value: int | string) {} }")]
    [InlineData("class Probe { init(value:System.Collections.Generic.List<int>) {} }")]
    [InlineData("class Probe { func M(value:int=42) {} }")]
    [InlineData("class Probe { func M<T>(value:T) {} }")]
    [InlineData("class Probe { static func +(left:Probe,right:Probe)->Probe => left }")]
    [InlineData("class Probe { static func explicit(value:Probe)->int => 0 }")]
    [InlineData("class Probe { init(ref value:int) {} }")]
    [InlineData("interface A {} interface B {} class Probe { init(value: A & B) {} }")]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_UnchangedSignatureKeepsParametersAcrossDiagnostics(string source) => CheckSignature(source, TargetPlatform.NeoCLR);

    private static void CheckSignature(string source, TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("SignatureReuse", [tree], TestMetadataReferences.DefaultWithRavenCore, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<BaseMethodDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(declaration));
        var parameters = declaration.ParameterList.Parameters.Select(p => Assert.IsAssignableFrom<ParameterSymbol>(model.GetDeclaredSymbol(p))).ToArray();
        for (var i = 0; i < parameters.Length; i++)
            Assert.Same(method.Parameters[i], parameters[i]);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var current = Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(declaration));
        Assert.Same(method, current);
        Assert.Single(current.ContainingType!.GetMembers(current.MetadataName).OfType<MethodSymbol>(),
            candidate => candidate.DeclaringSyntaxReferences.Any(reference =>
                ReferenceEquals(reference.SyntaxTree, tree) && reference.Span == declaration.Span));
        for (var i = 0; i < parameters.Length; i++)
        {
            Assert.Same(parameters[i], current.Parameters[i]);
            Assert.Same(parameters[i], model.GetDeclaredSymbol(declaration.ParameterList.Parameters[i]));
        }
    }

    [Theory]
    [InlineData("value:int=43", 43)]
    [InlineData("value:bool=true", true)]
    [InlineData("value:string?=null", null)]
    [Trait("Target", "DotNet")]
    public void SignatureEditDoesNotReuseOldParameter(string editedParameter, object? expectedDefault)
    {
        var oldTree = SyntaxTree.ParseText("class Probe { init(value:int=42) {} }", path: "main.rvn");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var previous = Compilation.Create("Edit", [oldTree], TestMetadataReferences.Default, options);
        var oldModel = previous.GetSemanticModel(oldTree);
        var oldSyntax = oldTree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var oldParameter = Assert.IsAssignableFrom<ParameterSymbol>(oldModel.GetDeclaredSymbol(oldSyntax));
        Assert.DoesNotContain(previous.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var newTree = SyntaxTree.ParseText("class Probe { init(" + editedParameter + ") {} }", path: "main.rvn");
        var current = Compilation.Create("Edit", [newTree], TestMetadataReferences.Default, options);
        current.AdoptIncrementalReuseFrom(previous);
        var newModel = current.GetSemanticModel(newTree);
        var newSyntax = newTree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var newParameter = Assert.IsAssignableFrom<ParameterSymbol>(newModel.GetDeclaredSymbol(newSyntax));
        Assert.NotSame(oldParameter, newParameter);
        Assert.True(newParameter.HasExplicitDefaultValue);
        Assert.Equal(expectedDefault, newParameter.ExplicitDefaultValue);
        Assert.Equal(42, oldParameter.ExplicitDefaultValue);
        Assert.DoesNotContain(current.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Same(newParameter, newModel.GetDeclaredSymbol(newSyntax));
        Assert.Same(oldParameter, oldModel.GetDeclaredSymbol(oldSyntax));
    }
}

public sealed class ParameterContextChangeTests
{
    [Fact, Trait("Target", "DotNet")]
    public void ChangedReferencedTypeDoesNotReuseParameterFromPreviousSnapshot()
    {
        var declaration = SyntaxTree.ParseText("class Probe { init(value:Entity) {} }", path: "probe.rvn");
        var beforeType = SyntaxTree.ParseText("class Entity {}", path: "entity.rvn");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var previous = Compilation.Create("Context", [declaration, beforeType], TestMetadataReferences.Default, options);
        var parameterSyntax = declaration.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var oldParameter = Assert.IsAssignableFrom<ParameterSymbol>(previous.GetSemanticModel(declaration).GetDeclaredSymbol(parameterSyntax));
        Assert.DoesNotContain(previous.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.False(oldParameter.Type.IsValueType);
        var afterType = SyntaxTree.ParseText("struct Entity {}", path: "entity.rvn");
        var current = Compilation.Create("Context", [declaration, afterType], TestMetadataReferences.Default, options);
        current.AdoptIncrementalReuseFrom(previous);
        var newParameter = Assert.IsAssignableFrom<ParameterSymbol>(current.GetSemanticModel(declaration).GetDeclaredSymbol(parameterSyntax));
        Assert.DoesNotContain(current.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.NotSame(oldParameter, newParameter);
        Assert.True(newParameter.Type.IsValueType);
        Assert.False(oldParameter.Type.IsValueType);
    }
}

public sealed class TupleParameterRefreshTests
{
    [Theory]
    [InlineData("tuple")]
    [InlineData("array")]
    [InlineData("generic")]
    [Trait("Target", "DotNet")]
    public void DotNet_ParameterRefreshRetainsNewTupleLabels(string shape) => CheckLabels(shape, TargetPlatform.DotNet);

    [Theory]
    [InlineData("tuple")]
    [InlineData("array")]
    [InlineData("generic")]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_ParameterRefreshRetainsNewTupleLabels(string shape) => CheckLabels(shape, TargetPlatform.NeoCLR);

    private static void CheckLabels(string shape, TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("class Probe { init(value: int) {} }");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target);
        var compilation = Compilation.Create("TupleParameter", [tree], TestMetadataReferences.Default, options);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<SourceMethodSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        var original = Assert.IsType<SourceParameterSymbol>(Assert.Single(method.Parameters));
        var integer = original.Type;

        SourceParameterSymbol WithLabel(string label)
        {
            TypeSymbol type = compilation.CreateTupleTypeSymbol([(label, integer), ("other", integer)]);
            type = shape switch
            {
                "array" => compilation.CreateArrayTypeSymbol(type),
                "generic" => compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!.Construct(type),
                _ => type
            };
            return new SourceParameterSymbol(original.Name, type, method, method.ContainingType,
                method.ContainingNamespace, original.Locations.ToArray(), original.DeclaringSyntaxReferences.ToArray());
        }

        method.SetParameters([WithLabel("before")]);
        var replacement = WithLabel("after");
        method.SetParameters([replacement]);
        Assert.Same(replacement, Assert.Single(method.Parameters));
    }
}
