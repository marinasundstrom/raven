using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class EnumDeclarationSemanticTests : CompilationTestBase
{
    [Fact]
    public void EnumMembers_UseUnderlyingTypeAndAutoIncrement()
    {
        const string source = """
enum Status : byte { Ok = 1, Error }
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var enumDeclaration = tree.GetRoot().DescendantNodes().OfType<EnumDeclarationSyntax>().Single();
        var enumSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(enumDeclaration));

        Assert.Equal(SpecialType.System_Byte, enumSymbol.EnumUnderlyingType.SpecialType);

        var members = enumSymbol.GetMembers().OfType<IFieldSymbol>().ToArray();
        var ok = members.Single(member => member.Name == "Ok");
        var error = members.Single(member => member.Name == "Error");

        Assert.Equal((byte)1, ok.GetConstantValue());
        Assert.Equal((byte)2, error.GetConstantValue());
    }

    [Fact]
    public void EnumUnderlyingType_MustBeIntegral()
    {
        const string source = """
enum Bad : string { Value }
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.EnumUnderlyingTypeMustBeIntegral);
    }

    [Fact]
    public void EnumMemberValue_MustBeConstant()
    {
        const string source = """
enum Bad { Value = 1 + 1 }
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.EnumMemberValueMustBeConstant);
    }

    [Fact]
    public void EnumMemberValue_MustFitUnderlyingType()
    {
        const string source = """
enum Bad : byte { Value = 300 }
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.EnumMemberValueCannotConvert);
    }
}
