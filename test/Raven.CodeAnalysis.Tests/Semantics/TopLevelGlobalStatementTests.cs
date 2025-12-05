using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class TopLevelGlobalStatementTests : CompilationTestBase
{
    [Fact]
    public void GlobalStatements_CanReferenceTopLevelTypes()
    {
const string source = """
let greeter: IGreeter = Greeter();
greeter.Greet();
let shade = Shade.Green;

interface IGreeter {
    public Greet() -> unit;
};

class Greeter : IGreeter {
    public Greet() -> unit => ();
};

enum Shade {
    Red,
    Green,
};
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var classDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var interfaceDeclaration = root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var enumDeclaration = root.DescendantNodes().OfType<EnumDeclarationSyntax>().Single();

        var classSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDeclaration));
        var interfaceSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(interfaceDeclaration));
        var enumSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(enumDeclaration));

        var greeterCreation = root
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "Greeter");
        var creationType = model.GetTypeInfo(greeterCreation);
        Assert.Same(classSymbol, creationType.Type);

        var annotationType = root
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(id => id.Identifier.ValueText == "IGreeter");
        var annotatedInfo = model.GetTypeInfo(annotationType);
        Assert.Same(interfaceSymbol, annotatedInfo.Type);

        var enumAccess = root
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "Shade");
        var enumMember = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(enumAccess).Symbol);
        Assert.Same(enumSymbol, enumMember.ContainingType);
    }

    [Fact]
    public void EmptyTopLevelProgram_EmitsEmptyMain()
    {
        var tree = SyntaxTree.ParseText(string.Empty);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        var entryPoint = compilation.GetEntryPoint();
        Assert.NotNull(entryPoint);
        Assert.Equal("Main", entryPoint!.Name);
        Assert.Equal("Program", entryPoint.ContainingType?.Name);
        Assert.Equal(SpecialType.System_Unit, entryPoint.ReturnType.SpecialType);
    }
}
