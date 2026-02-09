using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class LiteralTypeFlowTests : DiagnosticTestBase
{
    [Fact]
    public void VariableDeclaration_WithLiteral_InferredUnderlyingType()
    {
        var code = "var i = 0";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
    }

    [Fact]
    public void LetDeclaration_WithLiteral_InferredUnderlyingType()
    {
        var code = "val i = 0";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
    }

    [Fact]
    public void VariableDeclaration_WithFloatSuffix_InferredFloat()
    {
        var code = "var f = 3.14f"; // explicit float literal
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Single, local.Type.SpecialType);
    }

    [Fact]
    public void VariableDeclaration_WithDoubleLiteral_InferredDouble()
    {
        var code = "var d = 3.14"; // default double
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Double, local.Type.SpecialType);
    }

    [Fact]
    public void VariableDeclaration_WithLargeInteger_InferredLong()
    {
        var code = "var l = 4_000_000_000"; // exceeds Int32 range
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Int64, local.Type.SpecialType);
    }

    [Fact]
    public void Literal_ImplicitlyConvertsToUnderlyingType()
    {
        var code = "val x: bool = true";
        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MemberAccess_OnLiteralTypeVariable_UsesUnderlyingMembers()
    {
        var code = """
import System.*;

val literal: "foo" = "foo";
val length = literal.Length;
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MemberAccess_OnLiteralExpression_UsesUnderlyingMembers()
    {
        var code = """
import System.*;

val length = 10.ToString().Length;
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MemberAccess_OnLiteralExpression_MethodLookup_ResolvesToString()
    {
        var code = """
import System.*;

val length = 10.ToString().Length;
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .First(ma => ma.Name.Identifier.Text == "ToString");

        var binder = (BlockBinder)model.GetBinder(memberAccess)!;
        var receiver = binder.BindExpression(memberAccess.Expression);
        var literalType = Assert.IsType<LiteralTypeSymbol>(receiver.Type);
        Assert.NotEmpty(literalType.UnderlyingType.GetMembers("ToString"));

        var methods = new SymbolQuery("ToString", receiver.Type, IsStatic: false)
            .LookupMethods(binder)
            .ToImmutableArray();

        Assert.NotEmpty(methods);
    }

    [Fact]
    public void Int32_ProvidesToString()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.Contains(intType.GetMembers("ToString"), m => m is IMethodSymbol);
    }

    [Fact]
    public void LiteralType_Long_UsesUnderlyingInt64()
    {
        var code = "val x: 4_000_000_000 = 4_000_000_000";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var literalType = Assert.IsType<LiteralTypeSymbol>(local.Type);

        Assert.Equal(4_000_000_000L, literalType.ConstantValue);
        Assert.Equal(SpecialType.System_Int64, literalType.UnderlyingType.SpecialType);
    }

    [Fact]
    public void LiteralType_Float_UsesUnderlyingSingle()
    {
        var code = "val x: 3.14f = 3.14f";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var literalType = Assert.IsType<LiteralTypeSymbol>(local.Type);

        Assert.Equal(3.14f, literalType.ConstantValue);
        Assert.Equal(SpecialType.System_Single, literalType.UnderlyingType.SpecialType);
    }

    [Fact]
    public void LiteralType_Double_UsesUnderlyingDouble()
    {
        var code = "val x: 3.14 = 3.14";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var literalType = Assert.IsType<LiteralTypeSymbol>(local.Type);

        Assert.Equal(3.14d, literalType.ConstantValue);
        Assert.Equal(SpecialType.System_Double, literalType.UnderlyingType.SpecialType);
    }

    [Fact]
    public void LiteralType_Bool_UsesUnderlyingBoolean()
    {
        var code = "val x: true = true";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var literalType = Assert.IsType<LiteralTypeSymbol>(local.Type);

        Assert.True((bool)literalType.ConstantValue);
        Assert.Equal(SpecialType.System_Boolean, literalType.UnderlyingType.SpecialType);
    }

    [Fact]
    public void LiteralType_Char_UsesUnderlyingChar()
    {
        var code = "val x: 'a' = 'a'";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var literalType = Assert.IsType<LiteralTypeSymbol>(local.Type);

        Assert.Equal('a', literalType.ConstantValue);
        Assert.Equal(SpecialType.System_Char, literalType.UnderlyingType.SpecialType);
    }

    [Fact]
    public void IfExpression_InferredLiteralUnion()
    {
        var code = """
val x = if true { "true" } else { 1 }
""";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(local.Type);

        Assert.Contains(union.Types, t => t is LiteralTypeSymbol lt && Equals(lt.ConstantValue, "true"));
        Assert.Contains(union.Types, t => t is LiteralTypeSymbol lt && Equals(lt.ConstantValue, 1));
    }

    [Fact]
    public void IfExpression_WithMixedNumericBranches_InferredUnderlyingType()
    {
        var code = """
val other = 0
val value = if true { other } else { 42 }
""";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarators[1])!;

        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
    }

    [Fact]
    public void IfExpression_WithNullBranch_InferredNullableValueType()
    {
        var code = """
val other = 0
val value = if true { other } else { null }
""";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarators[1])!;
        var nullable = Assert.IsType<NullableTypeSymbol>(local.Type);

        Assert.Equal(SpecialType.System_Int32, nullable.UnderlyingType.SpecialType);
    }

    [Fact]
    public void IfExpression_WithNullReferenceBranch_InferredNullableReferenceType()
    {
        var code = """
val other = "hi"
val value = if true { other } else { null }
""";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarators[1])!;
        var nullable = Assert.IsType<NullableTypeSymbol>(local.Type);

        Assert.Equal(SpecialType.System_String, nullable.UnderlyingType.SpecialType);
    }

    [Fact]
    public void BinaryExpression_StringLiteralConcatenation_ReturnsLiteralType()
    {
        var code = "val greeting: \"Hello, World!\" = \"Hello\" + \", World!\"";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var literalType = Assert.IsType<LiteralTypeSymbol>(local.Type);
        Assert.Equal("Hello, World!", literalType.ConstantValue);
    }

    [Fact]
    public void BinaryExpression_StringLiteralAndNumericLiteralConcatenation_ReturnsLiteralType()
    {
        var code = "val result: \"Hello1\" = \"Hello\" + 1";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var literalType = Assert.IsType<LiteralTypeSymbol>(local.Type);
        Assert.Equal("Hello1", literalType.ConstantValue);
    }

    [Fact]
    public void BinaryExpression_NumericLiteralAndStringLiteralConcatenation_ReturnsLiteralType()
    {
        var code = "val result: \"1Hello\" = 1 + \"Hello\"";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var literalType = Assert.IsType<LiteralTypeSymbol>(local.Type);
        Assert.Equal("1Hello", literalType.ConstantValue);
    }

}
