using System;
using System.Collections.Immutable;
using System.Linq;

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
    public void AttachedMacroContext_ExposesArgumentList()
    {
        ArgumentCapturingAttachedMacro.LastCapturedArguments = null;
        ArgumentCapturingAttachedMacro.LastParsedArguments = default;

        var (compilation, tree) = CreateCompilation("""
            #[AddEquatable("Widget", Notify: true)]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ArgumentCapturingMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        _ = model.GetMacroExpansion(attribute);

        var capturedArguments = Assert.IsType<ArgumentListSyntax>(ArgumentCapturingAttachedMacro.LastCapturedArguments);
        Assert.Equal(2, capturedArguments.Arguments.Count);
        Assert.Equal("\"Widget\"", capturedArguments.Arguments[0].Expression.ToString());
        Assert.Equal("Notify", capturedArguments.Arguments[1].NameColon?.Name.Identifier.ValueText);
        Assert.Equal("true", capturedArguments.Arguments[1].Expression.ToString());

        Assert.Equal(2, ArgumentCapturingAttachedMacro.LastParsedArguments.Length);
        Assert.Null(ArgumentCapturingAttachedMacro.LastParsedArguments[0].Name);
        Assert.Equal("\"Widget\"", ArgumentCapturingAttachedMacro.LastParsedArguments[0].Expression.ToString());
        Assert.True(ArgumentCapturingAttachedMacro.LastParsedArguments[0].HasValue);
        Assert.Equal(TypedConstantKind.Primitive, ArgumentCapturingAttachedMacro.LastParsedArguments[0].ValueKind);
        Assert.Equal("Widget", ArgumentCapturingAttachedMacro.LastParsedArguments[0].Value);
        Assert.True(ArgumentCapturingAttachedMacro.LastParsedArguments[1].IsNamed);
        Assert.Equal("Notify", ArgumentCapturingAttachedMacro.LastParsedArguments[1].Name);
        Assert.Equal("true", ArgumentCapturingAttachedMacro.LastParsedArguments[1].Expression.ToString());
        Assert.True(ArgumentCapturingAttachedMacro.LastParsedArguments[1].HasValue);
        Assert.Equal(TypedConstantKind.Primitive, ArgumentCapturingAttachedMacro.LastParsedArguments[1].ValueKind);
        Assert.Equal(true, ArgumentCapturingAttachedMacro.LastParsedArguments[1].Value);
    }

    [Fact]
    public void MacroArgument_ConstantValue_IsAvailableDuringExpansion_WithoutRecursiveBinding()
    {
        ConstantReadingAttachedMacro.LastCapturedValue = null;

        var (compilation, tree) = CreateCompilation("""
            #[AddEquatable("Widget")]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ConstantReadingMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        var expansion = model.GetMacroExpansion(attribute);

        Assert.NotNull(expansion);
        Assert.Equal("Widget", ConstantReadingAttachedMacro.LastCapturedValue);
        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void RawMacro_ArgumentsRequireExplicitOptIn()
    {
        var (compilation, _) = CreateCompilation("""
            #[AddEquatable("Widget")]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TestMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVM012"));
        Assert.Contains("AddEquatable", diagnostic.GetMessage());
    }

    [Fact]
    public void TypedMacroParameters_BindPositionalAndNamedArguments()
    {
        TypedParameterAttachedMacro.LastCapturedParameters = null;

        var (compilation, tree) = CreateCompilation("""
            #[Observable("TitleChanged", Notify: false)]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TypedParameterMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        var expansion = model.GetMacroExpansion(attribute);

        Assert.NotNull(expansion);
        var parameters = Assert.IsType<ObservableMacroParameters>(TypedParameterAttachedMacro.LastCapturedParameters);
        Assert.Equal("TitleChanged", parameters.Name);
        Assert.False(parameters.Notify);
    }

    [Fact]
    public void TypedMacroParameters_UnknownNamedArgument_ReportsDiagnostic()
    {
        var (compilation, _) = CreateCompilation("""
            #[Observable("TitleChanged", Unknown: true)]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TypedParameterMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVM032"));
        Assert.Contains("Unknown", diagnostic.GetMessage());
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
    public void ParentAndMemberMacros_ExpandIndependentlyFromOriginalTargets()
    {
        var (compilation, tree) = CreateCompilation("""
            #[InspectMembers]
            class Widget {
                #[RenameMember]
                var Value: int
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ParentChildMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Single(symbol.GetMembers("Observed_Value").OfType<IMethodSymbol>());
        Assert.Single(symbol.GetMembers("Renamed_Value").OfType<IPropertySymbol>());
        Assert.Empty(symbol.GetMembers("Observed_Renamed_Value"));
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
    public void MacroReportedArgumentValidationDiagnostic_UsesMacroDiagnosticPath()
    {
        var (compilation, tree) = CreateCompilation("""
            #[ValidateName("")]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ValidationMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVM021"));
        Assert.Contains("ValidateName", diagnostic.GetMessage());
        Assert.Contains("VAL001: name cannot be empty", diagnostic.GetMessage(), StringComparison.Ordinal);

        var argument = tree.GetRoot()
            .DescendantNodes()
            .OfType<ArgumentSyntax>()
            .Single();

        Assert.Equal(argument.Span, diagnostic.Location.SourceSpan);
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

    public sealed class ConstantReadingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ConstantReadingMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ConstantReadingAttachedMacro()];
    }

    public sealed class ConstantReadingAttachedMacro : IAttachedDeclarationMacro
    {
        public static object? LastCapturedValue { get; set; }

        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public bool AcceptsArguments => true;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            LastCapturedValue = context.Arguments[0].Constant.Value;
            return MacroExpansionResult.Empty;
        }
    }

    public sealed class TypedParameterMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "TypedParameterMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new TypedParameterAttachedMacro()];
    }

    public sealed class ObservableMacroParameters
    {
        public ObservableMacroParameters(string name)
        {
            Name = name;
        }

        public string Name { get; }

        public bool Notify { get; set; } = true;
    }

    public sealed class TypedParameterAttachedMacro : IAttachedDeclarationMacro<ObservableMacroParameters>
    {
        public static ObservableMacroParameters? LastCapturedParameters { get; set; }

        public string Name => "Observable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext<ObservableMacroParameters> context)
        {
            LastCapturedParameters = context.Parameters;
            return MacroExpansionResult.Empty;
        }
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

    public sealed class ValidationMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ValidationMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ValidationAttachedMacro()];
    }

    public sealed class ParentChildMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ParentChildMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new InspectMembersMacro(), new RenameMemberMacro()];
    }

    public sealed class ValidationAttachedMacroParameters(string name)
    {
        public string Name { get; } = name;
    }

    public sealed class InspectMembersMacro : IAttachedDeclarationMacro
    {
        public string Name => "InspectMembers";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var type = Assert.IsType<ClassDeclarationSyntax>(context.TargetDeclaration);
            var property = Assert.Single(type.Members.OfType<PropertyDeclarationSyntax>());
            var members = ParseMembers($$"""
                class __GeneratedContainer {
                    func Observed_{{property.Identifier.ValueText}}() -> int { return 0 }
                }
                """);

            return new MacroExpansionResult
            {
                IntroducedMembers = [members[0]]
            };
        }
    }

    public sealed class RenameMemberMacro : IAttachedDeclarationMacro
    {
        public string Name => "RenameMember";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = Assert.IsType<PropertyDeclarationSyntax>(context.TargetDeclaration);
            var members = ParseMembers($$"""
                class __GeneratedContainer {
                    var Renamed_{{property.Identifier.ValueText}}: int { get => 0 }
                }
                """);

            return new MacroExpansionResult
            {
                ReplacementDeclaration = members[0]
            };
        }
    }

    public sealed class ValidationAttachedMacro : IAttachedDeclarationMacro<ValidationAttachedMacroParameters>
    {
        public string Name => "ValidateName";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext<ValidationAttachedMacroParameters> context)
        {
            if (string.IsNullOrEmpty(context.Parameters.Name))
            {
                return new MacroExpansionResult
                {
                    MacroDiagnostics =
                    [
                        context.CreateArgumentDiagnostic(
                            context.Arguments[0],
                            "name cannot be empty",
                            code: "VAL001")
                    ]
                };
            }

            return MacroExpansionResult.Empty;
        }
    }

    public sealed class ArgumentCapturingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ArgumentCapturingMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ArgumentCapturingAttachedMacro()];
    }

    public sealed class ArgumentCapturingAttachedMacro : IAttachedDeclarationMacro
    {
        public static ArgumentListSyntax? LastCapturedArguments { get; set; }
        public static ImmutableArray<MacroArgument> LastParsedArguments { get; set; }

        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public bool AcceptsArguments => true;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            LastCapturedArguments = context.ArgumentList;
            LastParsedArguments = context.Arguments;
            return MacroExpansionResult.Empty;
        }
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

    private static ImmutableArray<MemberDeclarationSyntax> ParseMembers(string source)
    {
        var tree = SyntaxFactory.ParseSyntaxTree(source);
        var container = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
        return [.. container.Members];
    }
}
