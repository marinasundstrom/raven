using System.Linq;
using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class MacroAttributeSemanticTests : CompilationTestBase
{
    [Fact]
    public void UnknownMacroAttribute_ReportsUnknownMacroDiagnostic_AndDoesNotBindAsClrAttribute()
    {
        var (compilation, tree) = CreateCompilation("""
            #[AddEquatable]
            class Widget {}
            """);

        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static diagnostic => diagnostic.Id == "RAVM010"));
        Assert.Contains("AddEquatable", diagnostic.GetMessage());

        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single();

        var symbol = (INamedTypeSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration)!;
        Assert.Empty(symbol.GetAttributes());
    }

    [Fact]
    public void KnownMacroAttribute_FromMacroReference_DoesNotReportUnknownMacroDiagnostic()
    {
        var (compilation, _) = CreateCompilation("""
            #[AddEquatable]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TestMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Id == "RAVM010");
    }

    [Fact]
    public void MacroAttribute_OnInvalidTarget_ReportsDiagnostic()
    {
        var (compilation, _) = CreateCompilation("""
            class Widget {
                #[AddEquatable]
                func Render() -> () {}
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TestMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVM011"));
        Assert.Contains("methods", diagnostic.GetMessage());
    }

    [Fact]
    public void GetMacroExpansion_ReturnsPluginExpansionResult()
    {
        var (compilation, tree) = CreateCompilation("""
            #[AddEquatable]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ExpandingMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        var expansion = model.GetMacroExpansion(attribute);

        Assert.NotNull(expansion);
        var member = Assert.Single(expansion!.IntroducedMembers);
        Assert.IsType<MethodDeclarationSyntax>(member);
        Assert.Equal("GeneratedMarker", ((MethodDeclarationSyntax)member).Identifier.ValueText);
    }

    [Fact]
    public void TypeMacro_IntroducedMembers_AppearOnDeclaredType()
    {
        var (compilation, tree) = CreateCompilation("""
            #[AddEquatable]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ExpandingMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        var generatedMethod = Assert.Single(symbol.GetMembers("GeneratedMarker").OfType<IMethodSymbol>());
        Assert.Equal(SpecialType.System_Boolean, generatedMethod.ReturnType.SpecialType);
    }

    [Fact]
    public void GetMacroExpansion_ReturnsReplacementDeclaration()
    {
        var (compilation, tree) = CreateCompilation("""
            class MyViewModel: INotifyPropertyChanged {
                #[Observable]
                var Title: string
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ReplacingMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        var expansion = model.GetMacroExpansion(attribute);

        Assert.NotNull(expansion);
        var property = Assert.IsType<PropertyDeclarationSyntax>(expansion!.ReplacementDeclaration);
        Assert.Equal("Title", property.Identifier.ValueText);
    }

    [Fact]
    public void PropertyMacro_ReplacementAndIntroducedMembers_ParticipateInDeclaredSymbols()
    {
        var (compilation, tree) = CreateCompilation("""
            class MyViewModel: INotifyPropertyChanged {
                #[Observable]
                var Title: string
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ReplacingMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var propertyDeclaration = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var propertySymbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(propertyDeclaration));

        Assert.NotNull(propertySymbol.GetMethod);
        Assert.NotNull(propertySymbol.SetMethod);

        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(propertySymbol.ContainingType);
        Assert.Single(containingType.GetMembers("_Title").OfType<IFieldSymbol>());

        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV0911");
    }

    [Fact]
    public void MacroExpansionDiagnostics_AreReportedBySemanticModel()
    {
        var (compilation, _) = CreateCompilation("""
            #[AddEquatable]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(DiagnosticMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVTEST001"));
        Assert.Contains("macro diagnostic", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void MacroExpansionFailure_ReportsDiagnostic()
    {
        var (compilation, _) = CreateCompilation("""
            #[AddEquatable]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ThrowingMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVM020"));
        Assert.Contains("AddEquatable", diagnostic.GetMessage());
    }

    public sealed class TestMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "TestMacroPlugin";

        public System.Collections.Immutable.ImmutableArray<IMacroDefinition> GetMacros()
            => [new TestAttachedMacro()];
    }

    public sealed class TestAttachedMacro : IAttachedDeclarationMacro
    {
        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context) => MacroExpansionResult.Empty;
    }

    public sealed class ExpandingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ExpandingMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ExpandingAttachedMacro()];
    }

    public sealed class ExpandingAttachedMacro : IAttachedDeclarationMacro
    {
        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var tree = SyntaxFactory.ParseSyntaxTree("""
                class __GeneratedContainer {
                    func GeneratedMarker() -> bool { return true }
                }
                """);

            var method = Assert.IsType<MethodDeclarationSyntax>(
                Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single()).Members.Single());

            return new MacroExpansionResult
            {
                IntroducedMembers = [method]
            };
        }
    }

    public sealed class DiagnosticMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "DiagnosticMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new DiagnosticAttachedMacro()];
    }

    public sealed class DiagnosticAttachedMacro : IAttachedDeclarationMacro
    {
        private static readonly DiagnosticDescriptor s_macroDiagnostic = DiagnosticDescriptor.Create(
            "RAVTEST001",
            "Macro test diagnostic",
            "",
            "",
            "Macro diagnostic from plugin",
            "tests",
            DiagnosticSeverity.Warning,
            true);

        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => new()
            {
                Diagnostics = [Diagnostic.Create(s_macroDiagnostic, context.Syntax.Name.GetLocation())]
            };
    }

    public sealed class ThrowingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ThrowingMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ThrowingAttachedMacro()];
    }

    public sealed class ThrowingAttachedMacro : IAttachedDeclarationMacro
    {
        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => throw new InvalidOperationException("plugin boom");
    }

    public sealed class ReplacingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ReplacingMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ReplacingAttachedMacro()];
    }

    public sealed class ReplacingAttachedMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var tree = SyntaxFactory.ParseSyntaxTree("""
                class __GeneratedContainer {
                    private field _Title: string

                    var Title: string {
                        get => _Title
                        set {
                            _Title = value
                        }
                    }
                }
                """);

            var container = tree.GetRoot().Members.SingleOrDefault() as ClassDeclarationSyntax;
            if (container is null)
                return MacroExpansionResult.Empty;

            var backingField = container.Members.ElementAtOrDefault(0) as FieldDeclarationSyntax;
            var property = container.Members.ElementAtOrDefault(1) as PropertyDeclarationSyntax;
            if (backingField is null || property is null)
                return MacroExpansionResult.Empty;

            return new MacroExpansionResult
            {
                ReplacementDeclaration = property,
                IntroducedMembers = [backingField]
            };
        }
    }
}
