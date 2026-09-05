using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class AttachedMacroSemanticTests : CompilationTestBase
{
    private new (Compilation Compilation, SyntaxTree Tree) CreateCompilation(
        string source,
        CompilationOptions? options = null,
        MetadataReference[]? references = null,
        string assemblyName = "test")
    {
        var tree = SyntaxTree.ParseText(source);
        var imports = SyntaxTree.ParseText("""
            global {
                import Raven.CodeAnalysis.Tests.Semantics.Macros.*
            }
            """);
        return (
            base.CreateCompilation([imports, tree], options, references, assemblyName),
            tree);
    }

    [Theory]
    [InlineData(false, false, "Property")]
    [InlineData(true, false, "Property")]
    [InlineData(false, true, "Property")]
    [InlineData(false, true, "Class")]
    [InlineData(true, true, "Property")]
    [InlineData(true, true, "Class")]
    public void ReplacedProperty_ValidatesAttributesAgainstProperty(bool querySymbolFirst, bool documentDiagnostics, string target)
    {
        var (compilation, tree) = CreateCompilation($$"""
            import System.*

            [AttributeUsage(AttributeTargets.{{target}})]
            class TargetedAttribute : Attribute { }

            class ViewModel {
                #[KeepPropertyAttributes]
                [Targeted]
                var Count: int = 0
            }
            """);
        compilation = compilation.AddMacroReferences(new MacroReference(new KeepPropertyAttributesMacro()));
        var model = compilation.GetSemanticModel(tree);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        if (querySymbolFirst)
            Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(property));

        var diagnostics = documentDiagnostics ? compilation.GetDocumentDiagnostics(tree) : compilation.GetDiagnostics();
        if (target == "Property")
            Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV0502");
        else
        {
            var diagnostic = Assert.Single(diagnostics.Where(diagnostic => diagnostic.Id == "RAV0502"));
            Assert.Contains("not valid on target 'property'", diagnostic.GetMessage());
        }
        var symbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(property));
        if (target == "Property")
            Assert.Contains(symbol.GetAttributes(), attribute => attribute.AttributeClass?.Name == "TargetedAttribute");
    }

    [Fact]
    public void ReplacedProperty_AttributeDiagnosticRecoversAfterUsageChanges()
    {
        const string source = """
            import System.*
            import Raven.CodeAnalysis.Tests.Semantics.Macros.*
            [AttributeUsage(AttributeTargets.Class)]
            class PropertyOnlyAttribute : Attribute { }

            class ViewModel {
                #[KeepPropertyAttributes]
                [PropertyOnly]
                var Count: int = 0
            }
            """;
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject("AttributeEdit",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        project = project.AddMacroReference(new MacroReference(new KeepPropertyAttributesMacro()));
        var document = project.AddDocument("ViewModel.rvn", Raven.CodeAnalysis.Text.SourceText.From(source), "/tmp/ViewModel.rvn");
        Assert.True(workspace.TryApplyChanges(document.Project.Solution));
        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single();
        Assert.Contains(compilation.GetDocumentDiagnostics(tree), diagnostic => diagnostic.Id == "RAV0502");

        var updatedSource = source.Replace("AttributeTargets.Class", "AttributeTargets.Property");
        Assert.True(workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id, Raven.CodeAnalysis.Text.SourceText.From(updatedSource))));
        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single();

        Assert.DoesNotContain(updatedCompilation.GetDocumentDiagnostics(updatedTree), diagnostic => diagnostic.Id == "RAV0502");
        Assert.DoesNotContain(updatedCompilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0502");
    }

    public sealed class KeepPropertyAttributesMacro : IMacroDefinition
    {
        public string Name => "KeepPropertyAttributes";

        public MacroExpansionResult Expand(PropertyDeclarationSyntax property, AttachedMacroContext context)
        {
            var replacement = (PropertyDeclarationSyntax)ParseMembers("""
                class Generated {
                    var Count: int { get => 0 set { } }
                }
                """)[0];
            return new MacroExpansionResult
            {
                ReplacementDeclaration = replacement.WithAttributeLists(SyntaxFactory.List(
                    property.AttributeLists.Where(list => !list.Attributes.Any(attribute => attribute.IsMacroAttribute()))))
            };
        }
    }

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TestAttachedMacro)));
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Id == "RAVM010");
    }

    [Fact]
    public void ErasedExecutor_ReceivesAttachedContext()
    {
        var executor = new AttachedSnapshotExecutor();
        var (compilation, tree) = CreateCompilation("""
            #[ErasedAttached]
            class Widget {}
            """);
        compilation = compilation.AddMacroReferences(new MacroReference(executor));
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        var expansion = compilation.GetSemanticModel(tree).GetMacroExpansion(attribute);

        Assert.NotNull(expansion);
        var context = Assert.IsType<AttachedMacroContext>(executor.Context);
        Assert.IsType<ClassDeclarationSyntax>(context.TargetDeclaration);
    }

    [Fact]
    public void TypeMacro_OnUnionCase_IsValidAndExpands()
    {
        CaseTrackingAttachedMacro.LastTargetCaseName = null;

        var (compilation, tree) = CreateCompilation("""
            union Result {
                #[TrackCase]
                case Ok(value: int)
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(CaseTrackingAttachedMacro)));

        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        var expansion = model.GetMacroExpansion(attribute);
        var diagnostics = compilation.GetDiagnostics();

        Assert.NotNull(expansion);
        Assert.Equal("Ok", CaseTrackingAttachedMacro.LastTargetCaseName);
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Id == "RAVM011");
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TestAttachedMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ExpandingAttachedMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ArgumentCapturingAttachedMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ConstantReadingAttachedMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TestAttachedMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TypedParameterAttachedMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TypedParameterAttachedMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ExpandingAttachedMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ReplacingAttachedMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ReplacingAttachedMacro)));

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

        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(InspectMembersMacro)),
            new MacroReference(typeof(RenameMemberMacro)));

        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Single(symbol.GetMembers("Observed_Value").OfType<IMethodSymbol>());
        Assert.Single(symbol.GetMembers("Renamed_Value").OfType<IPropertySymbol>());
        Assert.Empty(symbol.GetMembers("Observed_Renamed_Value"));
    }

    [Fact]
    public void StackedMacros_ExposeOriginalAndCurrentDeclarationSeparately()
    {
        TrackingMacroState.Reset();

        var (compilation, tree) = CreateCompilation("""
            class Widget {
                #[First]
                #[Second]
                var Value: int
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(TrackingFirstMacro)),
            new MacroReference(typeof(TrackingSecondMacro)));

        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Last();

        _ = model.GetMacroExpansion(attribute);

        Assert.Equal("Value", TrackingMacroState.SecondTargetName);
        Assert.Equal("First_Value", TrackingMacroState.SecondCurrentName);
    }

    [Fact]
    public void MacroExpansionDiagnostics_AreReportedBySemanticModel()
    {
        var (compilation, _) = CreateCompilation("""
            #[AddEquatable]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(DiagnosticAttachedMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ValidationAttachedMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ThrowingAttachedMacro)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVM020"));
        Assert.Contains("AddEquatable", diagnostic.GetMessage());
    }

    [Fact]
    public void TypedMacroExpansionFailure_ReportsUnderlyingException()
    {
        var (compilation, tree) = CreateCompilation("""
            #[TypedBoom]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ThrowingTypedAttachedMacro)));
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(static d => d.Id == "RAVM020"));

        Assert.Contains("TypedBoom", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("typed plugin boom", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.DoesNotContain("target of an invocation", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);

        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
        Assert.Equal(attribute.Name.Span, diagnostic.Location.SourceSpan);
    }

    [Fact]
    public void AttachedMacroCancellation_PropagatesAndDoesNotCacheFailure()
    {
        var (compilation, tree) = CreateCompilation("""
            #[CancelRaw]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(CancellingAttachedMacro)),
            new MacroReference(typeof(CancellingTypedAttachedMacro)));
        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
        using var cancellationSource = new CancellationTokenSource();
        CancellingAttachedMacro.CancellationSource = cancellationSource;

        Assert.ThrowsAny<OperationCanceledException>(
            () => model.GetMacroExpansion(attribute, cancellationSource.Token));

        CancellingAttachedMacro.CancellationSource = null;
        Assert.NotNull(model.GetMacroExpansion(attribute));
        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAVM020");
    }

    [Fact]
    public void TypedAttachedMacroCancellation_PropagatesThroughReflectionAndDoesNotCacheFailure()
    {
        var (compilation, tree) = CreateCompilation("""
            #[CancelTyped]
            class Widget {}
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(CancellingAttachedMacro)),
            new MacroReference(typeof(CancellingTypedAttachedMacro)));
        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
        using var cancellationSource = new CancellationTokenSource();
        CancellingTypedAttachedMacro.CancellationSource = cancellationSource;

        Assert.ThrowsAny<OperationCanceledException>(
            () => model.GetMacroExpansion(attribute, cancellationSource.Token));

        CancellingTypedAttachedMacro.CancellationSource = null;
        Assert.NotNull(model.GetMacroExpansion(attribute));
        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAVM020");
    }

    public sealed class TestAttachedMacro : IMacroDefinition
    {
        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(
            BaseTypeDeclarationSyntax target,
            AttachedMacroContext context) => MacroExpansionResult.Empty;
    }

    private sealed class AttachedSnapshotExecutor : IMacroExecutor
    {
        public string Name => "ErasedAttached";

        public MacroApplicationKind ApplicationKind => MacroApplicationKind.Attached;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroContext? Context { get; private set; }

        public MacroExecutionResult Expand(MacroExecutionContext context)
        {
            Context = context.Context;
            return MacroExecutionResult.Attached(MacroExpansionResult.Empty);
        }
    }

    public sealed class CaseTrackingAttachedMacro : IMacroDefinition
    {
        public static string? LastTargetCaseName { get; set; }

        public string Name => "TrackCase";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var caseDeclaration = Assert.IsType<CaseDeclarationSyntax>(context.TargetDeclaration);
            LastTargetCaseName = caseDeclaration.Identifier.ValueText;
            return MacroExpansionResult.Empty;
        }
    }

    public sealed class ConstantReadingAttachedMacro : IMacroDefinition
    {
        public static object? LastCapturedValue { get; set; }

        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(string value, AttachedMacroContext context)
        {
            LastCapturedValue = value;
            return MacroExpansionResult.Empty;
        }
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

    public sealed class TypedParameterAttachedMacro : IMacroDefinition
    {
        public static ObservableMacroParameters? LastCapturedParameters { get; set; }

        public string Name => "Observable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(
            string name,
            bool Notify,
            AttachedMacroContext context)
        {
            LastCapturedParameters = new ObservableMacroParameters(name) { Notify = Notify };
            return MacroExpansionResult.Empty;
        }
    }

    public sealed class ExpandingAttachedMacro : IMacroDefinition
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

    public sealed class DiagnosticAttachedMacro : IMacroDefinition
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

    public sealed class ThrowingAttachedMacro : IMacroDefinition
    {
        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => throw new InvalidOperationException("plugin boom");
    }

    public sealed class ThrowingTypedMacroParameters;

    public sealed class ThrowingTypedAttachedMacro : IMacroDefinition
    {
        public string Name => "TypedBoom";

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => throw new InvalidOperationException("typed plugin boom");
    }

    public sealed class CancellingAttachedMacro : IMacroDefinition
    {
        public static CancellationTokenSource? CancellationSource { get; set; }

        public string Name => "CancelRaw";
        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            CancellationSource?.Cancel();
            context.CancellationToken.ThrowIfCancellationRequested();
            return MacroExpansionResult.Empty;
        }
    }

    public sealed class CancellingTypedAttachedMacroParameters;

    public sealed class CancellingTypedAttachedMacro : IMacroDefinition
    {
        public static CancellationTokenSource? CancellationSource { get; set; }

        public string Name => "CancelTyped";
        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            CancellationSource?.Cancel();
            context.CancellationToken.ThrowIfCancellationRequested();
            return MacroExpansionResult.Empty;
        }
    }

    public sealed class ValidationAttachedMacroParameters(string name)
    {
        public string Name { get; } = name;
    }

    public sealed class InspectMembersMacro : IMacroDefinition
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

    public sealed class RenameMemberMacro : IMacroDefinition
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

    public static class TrackingMacroState
    {
        public static string? SecondTargetName { get; set; }
        public static string? SecondCurrentName { get; set; }

        public static void Reset()
        {
            SecondTargetName = null;
            SecondCurrentName = null;
        }
    }

    public sealed class TrackingFirstMacro : IMacroDefinition
    {
        public string Name => "First";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = Assert.IsType<PropertyDeclarationSyntax>(context.TargetDeclaration);
            var members = ParseMembers($$"""
                class __GeneratedContainer {
                    var First_{{property.Identifier.ValueText}}: int { get => 0 }
                }
                """);

            return new MacroExpansionResult
            {
                ReplacementDeclaration = members[0]
            };
        }
    }

    public sealed class TrackingSecondMacro : IMacroDefinition
    {
        public string Name => "Second";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var target = Assert.IsType<PropertyDeclarationSyntax>(context.TargetDeclaration);
            var current = Assert.IsType<PropertyDeclarationSyntax>(context.CurrentDeclaration);

            TrackingMacroState.SecondTargetName = target.Identifier.ValueText;
            TrackingMacroState.SecondCurrentName = current.Identifier.ValueText;

            return MacroExpansionResult.Empty;
        }
    }

    public sealed class ValidationAttachedMacro : IMacroDefinition
    {
        public string Name => "ValidateName";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(string name, AttachedMacroContext context)
        {
            if (string.IsNullOrEmpty(name))
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

    public sealed class ArgumentCapturingAttachedMacro : IMacroDefinition
    {
        public static ArgumentListSyntax? LastCapturedArguments { get; set; }
        public static ImmutableArray<MacroArgument> LastParsedArguments { get; set; }

        public string Name => "AddEquatable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(
            string name,
            bool Notify,
            AttachedMacroContext context)
        {
            LastCapturedArguments = context.ArgumentList;
            LastParsedArguments = context.Arguments;
            return MacroExpansionResult.Empty;
        }
    }

    public sealed class ReplacingAttachedMacro : IMacroDefinition
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
