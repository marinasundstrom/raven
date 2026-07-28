using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class MacroFunctionSymbolTests : CompilationTestBase
{
    [Fact]
    public void MacroFunctionDeclaration_DeclaresDistinctMacroFunctionSymbol()
    {
        var (compilation, tree) = CreateCompilation("""
            macro func Identity<T>(value: T) -> T
                where T: System.IDisposable
            {
                return value
            }
            """);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();

        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            model.GetDeclaredSymbol(declaration));

        Assert.Equal(SymbolKind.MacroFunction, symbol.Kind);
        Assert.Equal(MacroKind.FreestandingExpression, symbol.MacroKind);
        Assert.Equal(MacroTarget.None, symbol.Targets);
        Assert.Null(symbol.TargetName);
        Assert.Equal("Identity", symbol.Name);
        Assert.True(symbol.IsStatic);
        Assert.True(symbol.CanBeReferencedByName);
        Assert.False(symbol is IMethodSymbol);

        var typeParameter = Assert.Single(symbol.TypeParameters);
        Assert.Equal(TypeParameterOwnerKind.MacroFunction, typeParameter.OwnerKind);
        Assert.Same(symbol, typeParameter.DeclaringMacroFunctionParameterOwner);
        Assert.Null(typeParameter.DeclaringMethodParameterOwner);
        Assert.Null(typeParameter.DeclaringTypeParameterOwner);
        Assert.Equal(TypeParameterConstraintKind.TypeConstraint, typeParameter.ConstraintKind);
        Assert.Equal("IDisposable", Assert.Single(typeParameter.ConstraintTypes).Name);
        Assert.Same(typeParameter, symbol.ReturnType);

        var parameter = Assert.Single(symbol.Parameters);
        Assert.Equal("value", parameter.Name);
        Assert.Same(symbol, parameter.ContainingSymbol);
        Assert.Same(typeParameter, parameter.Type);
        Assert.Same(parameter, model.GetDeclaredSymbol(declaration.ParameterList.Parameters.Single()));

        Assert.Contains(
            compilation.GlobalNamespace.GetMembers("Identity"),
            candidate => ReferenceEquals(candidate, symbol));
        Assert.DoesNotContain(
            compilation.GlobalNamespace.GetMembers("Identity"),
            candidate => candidate is IMethodSymbol);

        var classifications = SemanticClassifier.Classify(tree.GetRoot(), model);
        Assert.Equal(
            SemanticClassification.Method,
            classifications.Tokens[declaration.Identifier]);
        Assert.Equal(
            SemanticClassification.Parameter,
            classifications.Tokens[declaration.ParameterList.Parameters.Single().Identifier]);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroFunctionDeclaration_UsesItsNamespaceAsSemanticContainer()
    {
        var (compilation, tree) = CreateCompilation("""
            namespace Tools {
                macro func Quote(value: int) -> int => value
            }
            """);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();

        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            model.GetDeclaredSymbol(declaration));

        Assert.Equal("Tools", symbol.ContainingNamespace?.ToDisplayString());
        Assert.Same(symbol.ContainingNamespace, symbol.ContainingSymbol);
        Assert.Null(symbol.ContainingType);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroFunctionDeclaration_RejectsAsyncAndAwait()
    {
        var (compilation, _) = CreateCompilation("""
            async macro func Fetch(task: System.Threading.Tasks.Task<int>) -> int {
                return await task
            }
            """);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0925");
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV0926");
    }

    [Fact]
    public void AttachedMacroFunction_ExposesTargetSemantics()
    {
        var (compilation, tree) = CreateCompilation("""
            macro func Observable() on property: Property {
                replace property
                introduce CreateBackingField(property)
            }
            """);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IMacroFunctionSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal(MacroKind.AttachedDeclaration, symbol.MacroKind);
        Assert.Equal(MacroTarget.Property, symbol.Targets);
        Assert.Equal("property", symbol.TargetName);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MacroContribution_ValidatesAgainstAttachmentKind()
    {
        var (compilation, _) = CreateCompilation("""
            macro func Freestanding() {
                replace quote! { 1 }
            }

            macro func Attached() on Type {
                expand quote! { 1 }
            }
            """);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal(2, diagnostics.Count(static diagnostic => diagnostic.Id == "RAV0928"));
    }
}
