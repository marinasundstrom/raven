using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ConstBindingSemanticTests : CompilationTestBase
{
    [Fact]
    public void ConstLocal_InferredStringConstant()
    {
        const string source = """
class C {
    static Run() {
        const greeting = "Hello"
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.True(local.IsConst);
        Assert.False(local.IsMutable);
        Assert.Equal("Hello", local.ConstantValue);
        Assert.Equal(SpecialType.System_String, local.Type.SpecialType);
    }

    [Fact]
    public void ConstLocal_ExplicitNumericTypeConverts()
    {
        const string source = """
import System.*

class C {
    static Run() {
        const total: Int64 = 1
        const small: Byte = 5
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();

        var total = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators[0]));
        Assert.True(total.IsConst);
        Assert.Equal(SpecialType.System_Int64, total.Type.SpecialType);
        Assert.Equal(1L, total.ConstantValue);

        var small = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators[1]));
        Assert.True(small.IsConst);
        Assert.Equal(SpecialType.System_Byte, small.Type.SpecialType);
        Assert.Equal((byte)5, small.ConstantValue);
    }

    [Fact]
    public void ConstLocal_StringNullAllowed()
    {
        const string source = """
class C {
    static Run() {
        const text: string = null
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.True(local.IsConst);
        Assert.Null(local.ConstantValue);
        Assert.Equal(SpecialType.System_String, local.Type.SpecialType);
    }

    [Fact]
    public void ConstLocal_NonConstantInitializerReportsDiagnostic()
    {
        const string source = """
import System.*

class C {
    static Run() {
        const today = DateTime.Now
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.ConstLocalMustBeConstant.Id, diagnostic.Id);
    }

    [Fact]
    public void ConstLocal_ReassignmentIsRejected()
    {
        const string source = """
class C {
    static Run() {
        const value = "Hi"
        value = "Bye"
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.ThisValueIsNotMutable.Id, diagnostic.Id);
    }

    [Fact]
    public void ConstField_InferredStringConstant()
    {
        const string source = """
class C {
    const greeting = "Hello"
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.True(field.IsLiteral);
        Assert.True(field.IsStatic);
        Assert.Equal("Hello", field.GetConstantValue());
        Assert.Equal(SpecialType.System_String, field.Type.SpecialType);
    }

    [Fact]
    public void ConstField_ExplicitNumericTypeConverts()
    {
        const string source = """
import System.*

class C {
    public const total: Int64 = 1
    public const small: Byte = 5
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();

        var total = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(declarators[0]));
        Assert.True(total.IsLiteral);
        Assert.True(total.IsStatic);
        Assert.Equal(SpecialType.System_Int64, total.Type.SpecialType);
        Assert.Equal(1L, total.GetConstantValue());

        var small = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(declarators[1]));
        Assert.True(small.IsLiteral);
        Assert.True(small.IsStatic);
        Assert.Equal(SpecialType.System_Byte, small.Type.SpecialType);
        Assert.Equal((byte)5, small.GetConstantValue());
    }

    [Fact]
    public void ConstField_MissingInitializerReportsDiagnostic()
    {
        const string source = """
class C {
    const value: int
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.ConstFieldRequiresInitializer.Id, diagnostic.Id);
    }

    [Fact]
    public void ConstField_NonConstantInitializerReportsDiagnostic()
    {
        const string source = """
import System.*

class C {
    const today = DateTime.Now
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.ConstFieldMustBeConstant.Id, diagnostic.Id);
    }

    [Fact]
    public void ConstField_AssignmentReportsDiagnostic()
    {
        const string source = """
class C {
    const greeting = "Hello"

    init() {
        greeting = "Hi"
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.ThisValueIsNotMutable.Id, diagnostic.Id);
    }
}
