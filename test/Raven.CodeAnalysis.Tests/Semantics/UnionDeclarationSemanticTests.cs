using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics;

public class UnionDeclarationSemanticTests : CompilationTestBase
{
    [Fact]
    public void UnionDeclaration_DefinesStructWithCases()
    {
        const string source = @"union Result<T> {
    Ok(value: T)
    Error(message: string)
}";

        var (compilation, tree) = CreateCompilation(source);
        var root = tree.GetRoot();
        var unionDecl = root.DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);

        var unionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(unionDecl));
        Assert.Equal(TypeKind.Struct, unionSymbol.TypeKind);
        Assert.True(unionSymbol.IsValueType);

        var okCase = unionSymbol.GetMembers().OfType<INamedTypeSymbol>().Single(n => n.Name == "Ok");
        Assert.True(okCase.IsValueType);

        var conversion = unionSymbol.GetMembers()
            .OfType<IMethodSymbol>()
            .Single(m => m.Name == "op_Implicit" && m.Parameters.Length == 1 && m.Parameters[0].Type.Name == "Ok");
        Assert.Equal(unionSymbol, conversion.ReturnType);

        var tryGet = unionSymbol.GetMembers()
            .OfType<IMethodSymbol>()
            .Single(m => m.Name == "TryGetOk");
        Assert.Equal(SpecialType.System_Boolean, tryGet.ReturnType.SpecialType);
        Assert.Equal(RefKind.Ref, tryGet.Parameters[0].RefKind);
        Assert.Equal("Result<T>.Ok", tryGet.Parameters[0].Type.ToDisplayString());
    }

    [Fact]
    public void UnionCase_WithExtraTypeParameters_GeneratesGenericMembers()
    {
        const string source = @"union Result<T> {
    Deferred<TResult>(factory: func() -> TResult)
}";

        var (compilation, tree) = CreateCompilation(source);
        var root = tree.GetRoot();
        var unionDecl = root.DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);

        var unionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(unionDecl));
        var conversion = unionSymbol.GetMembers()
            .OfType<IMethodSymbol>()
            .Single(m => m.Name == "op_Implicit");

        Assert.Equal(1, conversion.TypeParameters.Length);
        Assert.Equal("TResult", conversion.TypeParameters[0].Name);
        Assert.Equal("Result<T>", conversion.ReturnType.ToDisplayString());
        Assert.Equal("Result<T>.Deferred<TResult>", conversion.Parameters[0].Type.ToDisplayString());

        var tryGet = unionSymbol.GetMembers()
            .OfType<IMethodSymbol>()
            .Single(m => m.Name == "TryGetDeferred");
        Assert.Equal(1, tryGet.TypeParameters.Length);
        Assert.Equal("Result<T>.Deferred<TResult>", tryGet.Parameters[0].Type.ToDisplayString());
    }

    [Fact]
    public void UnionDeclaration_WithNoCases_ReportsDiagnostic()
    {
        const string source = "union Empty {}";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var diagnostics = model.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnionMustDeclareCase);
    }

    [Fact]
    public void UnionCase_WithByRefParameter_ReportsDiagnostic()
    {
        const string source = @"union Token {
    Identifier(ref text: string)
}";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var diagnostics = model.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnionCaseParameterCannotBeByRef);
    }

    [Fact]
    public void UnionDeclaration_EmitsUnionAttribute()
    {
        const string source = @"union Token {
    Identifier(text: string)
}";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "uni");

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var unionType = assembly.GetType("Token");
        Assert.NotNull(unionType);

        Assert.Contains(unionType!.GetCustomAttributesData(), data => data.AttributeType.Name == "UnionAttribute");
        var caseType = unionType.GetNestedType("Identifier");
        Assert.NotNull(caseType);
        Assert.DoesNotContain(caseType!.GetCustomAttributesData(), data => data.AttributeType.Name == "UnionAttribute");
        Assert.NotNull(assembly.GetType("UnionAttribute"));
    }
}
