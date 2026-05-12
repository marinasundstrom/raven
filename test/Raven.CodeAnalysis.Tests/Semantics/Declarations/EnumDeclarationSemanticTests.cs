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

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
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

    [Fact]
    public void TargetTypedEnumMember_BindsToEnumField()
    {
        const string source = """
enum DeviceType {
    Harddrive
    Monitor
    CPU
}

func Identity(value: DeviceType) -> DeviceType {
    return value
}

func Pick() -> DeviceType {
    return .CPU
}

func Use() {
    val annotated: DeviceType = .Monitor
    val argument = Identity(.Harddrive)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var enumDeclaration = tree.GetRoot().DescendantNodes().OfType<EnumDeclarationSyntax>().Single();
        var enumSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(enumDeclaration));

        var memberBindings = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberBindingExpressionSyntax>()
            .ToArray();

        Assert.Collection(
            memberBindings,
            binding => AssertEnumMemberBinding("CPU", binding),
            binding => AssertEnumMemberBinding("Monitor", binding),
            binding => AssertEnumMemberBinding("Harddrive", binding));

        void AssertEnumMemberBinding(string expectedName, MemberBindingExpressionSyntax binding)
        {
            var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(binding).Symbol);
            Assert.Equal(expectedName, field.Name);
            Assert.Same(enumSymbol, field.ContainingType);
        }
    }

    [Fact]
    public void WildcardImportFromSourceEnum_BindsMembersBySimpleName()
    {
        const string source = """
import DeviceType.*

enum DeviceType {
    Harddrive
    Monitor
    CPU
}

func Pick() -> DeviceType {
    return Monitor
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var enumDeclaration = tree.GetRoot().DescendantNodes().OfType<EnumDeclarationSyntax>().Single();
        var enumSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(enumDeclaration));

        var monitorReference = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .Single()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "Monitor");

        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(monitorReference).Symbol);
        Assert.Equal("Monitor", field.Name);
        Assert.Same(enumSymbol, field.ContainingType);
    }

    [Fact]
    public void DirectImportFromSourceEnum_BindsMemberBySimpleName()
    {
        const string source = """
import DeviceType.Monitor

enum DeviceType {
    Harddrive
    Monitor
    CPU
}

func Pick() -> DeviceType {
    return Monitor
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var enumDeclaration = tree.GetRoot().DescendantNodes().OfType<EnumDeclarationSyntax>().Single();
        var enumSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(enumDeclaration));

        var monitorReference = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .Single()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "Monitor");

        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(monitorReference).Symbol);
        Assert.Equal("Monitor", field.Name);
        Assert.Same(enumSymbol, field.ContainingType);
    }
}
