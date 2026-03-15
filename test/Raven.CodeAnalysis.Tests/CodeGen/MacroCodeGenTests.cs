using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class MacroCodeGenTests
{
    [Fact]
    public void AttachedTypeMacro_IntroducedMethod_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run() -> bool {
                    return Widget().GeneratedMarker()
                }
            }

            #[AddEquatable]
            class Widget {}
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(EmitMacroPlugin)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(true, method!.Invoke(null, null));
    }

    [Fact]
    public void AttachedPropertyMacro_ReplacementProperty_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run() -> string {
                    val model = MyViewModel()
                    model.Title = "Hello from macro"
                    return model.Title
                }
            }

            class MyViewModel {
                #[Observable]
                var Title: string
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(EmitMacroPlugin)));

        AssertObservablePropertyShape(compilation, syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal("Hello from macro", method!.Invoke(null, null));
    }

    [Fact]
    public void AttachedPropertyMacro_WithInitializer_UsesReplacementSetterBody()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class MyViewModel {
                #[Observable]
                var Title: string = ""
            }

            class Harness {
                static func Run() -> int {
                    val model = MyViewModel()
                    model.Title = "Hello"
                    return model.GetTitleWriteCount()
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(EmitMacroPlugin)));

        AssertObservablePropertyShape(compilation, syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(1, method!.Invoke(null, null));
    }

    [Fact]
    public void AttachedPropertyMacro_WithProtectedBaseCall_UsesReplacementSetterBody()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            open class ObservableBase {
                var Count: int

                protected func RaisePropertyChanged(propertyName: string, oldValue: object?, newValue: object?) -> unit {
                    Count = Count + 1
                }
            }

            class MyViewModel : ObservableBase {
                #[Observable]
                var Title: string = ""
            }

            class Harness {
                static func Run() -> int {
                    val model = MyViewModel()
                    model.Title = "Hello"
                    return model.Count
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(SampleLikeObservableMacroPlugin)));

        var model = compilation.GetSemanticModel(syntaxTree);
        var propertyDeclaration = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single(static p => p.Identifier.ValueText == "Title");
        var propertySymbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(propertyDeclaration));
        var setSyntax = Assert.IsType<AccessorDeclarationSyntax>(propertySymbol.SetMethod!.DeclaringSyntaxReferences[0].GetSyntax());
        var generatedModel = compilation.GetSemanticModel(setSyntax.SyntaxTree);
        var raisePropertyChangedInvocation = setSyntax.Body!
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        Assert.NotNull(generatedModel.GetBoundNode(setSyntax.Body!, BoundTreeView.Lowered));
        Assert.NotNull(generatedModel.GetSymbolInfo(raisePropertyChangedInvocation).Symbol);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);

        Assert.Equal(1, method!.Invoke(null, null));
    }

    [Fact]
    public void AttachedPropertyMacro_WithDetachedSyntaxFactoryNodes_DoesNotRequireSyntheticRooting()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            open class ObservableBase {
                var Count: int

                protected func RaisePropertyChanged(propertyName: string, oldValue: object?, newValue: object?) -> unit {
                    Count = Count + 1
                }
            }

            class MyViewModel : ObservableBase {
                #[Observable]
                var Title: string = ""
            }

            class Harness {
                static func Run() -> int {
                    val model = MyViewModel()
                    model.Title = "Hello"
                    return model.Count
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(DetachedSyntaxFactoryObservableMacroPlugin)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);

        Assert.Equal(1, method!.Invoke(null, null));
    }

    [Fact]
    public void AttachedPropertyMacro_WithIntroducedGenericInitializer_EmitsWithoutSequencePointCrash()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*

            class MyViewModel {
                #[Reactive]
                var Title: string = ""
            }

            class Harness {
                static func Run() -> string {
                    val model = MyViewModel()
                    model.Title = "Hello"
                    return model.Title
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(GenericInitializerMacroPlugin)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);

        Assert.Equal("Hello", method!.Invoke(null, null));
    }

    private static void AssertObservablePropertyShape(Compilation compilation, SyntaxTree syntaxTree)
    {
        var model = compilation.GetSemanticModel(syntaxTree);
        var propertyDeclaration = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single(static p => p.Identifier.ValueText == "Title");

        var propertySymbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(propertyDeclaration));
        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(propertySymbol.ContainingType);

        var titleProperties = containingType.GetMembers("Title").OfType<IPropertySymbol>().ToArray();
        Assert.Single(titleProperties);
        Assert.Same(propertySymbol, titleProperties[0]);

        var sourcePropertySymbol = Assert.IsType<SourcePropertySymbol>(propertySymbol);
        Assert.Null(sourcePropertySymbol.BackingField);
        Assert.NotNull(propertySymbol.GetMethod);
        Assert.NotNull(propertySymbol.SetMethod);
        Assert.False(propertySymbol.GetMethod!.DeclaringSyntaxReferences.IsDefaultOrEmpty);
        Assert.False(propertySymbol.SetMethod!.DeclaringSyntaxReferences.IsDefaultOrEmpty);

        var getterMethods = containingType.GetMembers("get_Title").OfType<IMethodSymbol>().ToArray();
        var setterMethods = containingType.GetMembers("set_Title").OfType<IMethodSymbol>().ToArray();
        Assert.Single(getterMethods);
        Assert.Single(setterMethods);
        Assert.Same(propertySymbol.GetMethod, getterMethods[0]);
        Assert.Same(propertySymbol.SetMethod, setterMethods[0]);

        Assert.DoesNotContain(
            containingType.GetMembers().OfType<IFieldSymbol>(),
            static field => field.Name == "<Title>k__BackingField");

        var getSyntax = Assert.IsType<AccessorDeclarationSyntax>(propertySymbol.GetMethod.DeclaringSyntaxReferences[0].GetSyntax());
        var setSyntax = Assert.IsType<AccessorDeclarationSyntax>(propertySymbol.SetMethod.DeclaringSyntaxReferences[0].GetSyntax());
        var generatedModel = compilation.GetSemanticModel(getSyntax.SyntaxTree);
        Assert.NotNull(generatedModel.GetBoundNode(getSyntax.ExpressionBody!.Expression, BoundTreeView.Lowered));
        Assert.NotNull(generatedModel.GetBoundNode(setSyntax.Body!, BoundTreeView.Lowered));
    }

    public sealed class EmitMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "EmitMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new IntroducedMethodMacro(), new ObservablePropertyMacro()];
    }

    private sealed class IntroducedMethodMacro : IAttachedDeclarationMacro
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

    private sealed class ObservablePropertyMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var tree = SyntaxFactory.ParseSyntaxTree("""
                class __GeneratedContainer {
                    private var _Title: string
                    private var _TitleWriteCount: int

                    var Title: string {
                        get => _Title
                        set {
                            _Title = value
                            _TitleWriteCount = _TitleWriteCount + 1
                        }
                    }

                    func GetTitleWriteCount() -> int {
                        return _TitleWriteCount
                    }
                }
                """);

            var container = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
            var backingStorage = Assert.IsType<PropertyDeclarationSyntax>(container.Members[0]);
            var writeCountStorage = Assert.IsType<PropertyDeclarationSyntax>(container.Members[1]);
            var property = Assert.IsType<PropertyDeclarationSyntax>(container.Members[2]);
            var writeCountMethod = Assert.IsType<MethodDeclarationSyntax>(container.Members[3]);

            return new MacroExpansionResult
            {
                ReplacementDeclaration = property,
                IntroducedMembers = [backingStorage, writeCountStorage, writeCountMethod]
            };
        }
    }

    public sealed class SampleLikeObservableMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "SampleLikeObservableMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new SampleLikeObservablePropertyMacro()];
    }

    private sealed class SampleLikeObservablePropertyMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var tree = SyntaxFactory.ParseSyntaxTree("""
                class __GeneratedContainer {
                    private var _Title: string

                    var Title: string {
                        get => _Title
                        set {
                            val oldValue = _Title
                            _Title = value
                            RaisePropertyChanged(nameof(Title), oldValue, value)
                        }
                    }
                }
                """);

            var container = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
            var backingStorage = Assert.IsType<PropertyDeclarationSyntax>(container.Members[0]);
            var property = Assert.IsType<PropertyDeclarationSyntax>(container.Members[1]);

            return new MacroExpansionResult
            {
                ReplacementDeclaration = property,
                IntroducedMembers = [backingStorage]
            };
        }
    }

    public sealed class DetachedSyntaxFactoryObservableMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "DetachedSyntaxFactoryObservableMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new DetachedSyntaxFactoryObservablePropertyMacro()];
    }

    private sealed class DetachedSyntaxFactoryObservablePropertyMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = Assert.IsType<PropertyDeclarationSyntax>(context.TargetDeclaration);
            var propertyName = property.Identifier.ValueText;
            var backingFieldName = "_" + propertyName;

            var backingStorage = SyntaxFactory.PropertyDeclaration(
                SyntaxFactory.List<AttributeListSyntax>(),
                SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PrivateKeyword)),
                SyntaxFactory.Token(SyntaxKind.VarKeyword),
                SyntaxFactory.Identifier(backingFieldName),
                property.Type,
                property.Initializer);

            var replacement = property
                .WithAttributeLists(SyntaxFactory.List<AttributeListSyntax>())
                .WithAccessorList(SyntaxFactory.AccessorList(
                    SyntaxFactory.List<AccessorDeclarationSyntax>(
                    [
                        SyntaxFactory.AccessorDeclaration(
                            SyntaxKind.GetAccessorDeclaration,
                            SyntaxFactory.List<AttributeListSyntax>(),
                            SyntaxFactory.TokenList(),
                            SyntaxFactory.Token(SyntaxKind.GetKeyword),
                            SyntaxFactory.ArrowExpressionClause(SyntaxFactory.IdentifierName(backingFieldName))),
                        SyntaxFactory.AccessorDeclaration(
                            SyntaxKind.SetAccessorDeclaration,
                            SyntaxFactory.List<AttributeListSyntax>(),
                            SyntaxFactory.TokenList(),
                            SyntaxFactory.Token(SyntaxKind.SetKeyword),
                            SyntaxFactory.BlockStatement(
                                SyntaxFactory.List<StatementSyntax>(
                                [
                                    SyntaxFactory.LocalDeclarationStatement(
                                        SyntaxFactory.VariableDeclaration(
                                            SyntaxFactory.Token(SyntaxKind.ValKeyword),
                                            SyntaxFactory.SeparatedList<VariableDeclaratorSyntax>(
                                            [
                                                new SyntaxNodeOrToken(SyntaxFactory.VariableDeclarator(
                                                    SyntaxFactory.Identifier("oldValue"),
                                                    typeAnnotation: null,
                                                    initializer: SyntaxFactory.EqualsValueClause(SyntaxFactory.IdentifierName(backingFieldName))))
                                            ]))),
                                    SyntaxFactory.AssignmentStatement(
                                        SyntaxKind.SimpleAssignmentStatement,
                                        SyntaxFactory.IdentifierName(backingFieldName),
                                        SyntaxFactory.Token(SyntaxKind.EqualsToken),
                                        SyntaxFactory.IdentifierName("value")),
                                    SyntaxFactory.ExpressionStatement(
                                        SyntaxFactory.InvocationExpression(
                                            SyntaxFactory.IdentifierName("RaisePropertyChanged"),
                                            SyntaxFactory.ArgumentList(
                                                SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                                [
                                                    new SyntaxNodeOrToken(SyntaxFactory.Argument(SyntaxFactory.NameOfExpression(SyntaxFactory.IdentifierName(propertyName)))),
                                                    new SyntaxNodeOrToken(SyntaxFactory.Token(SyntaxKind.CommaToken)),
                                                    new SyntaxNodeOrToken(SyntaxFactory.Argument(SyntaxFactory.IdentifierName("oldValue"))),
                                                    new SyntaxNodeOrToken(SyntaxFactory.Token(SyntaxKind.CommaToken)),
                                                    new SyntaxNodeOrToken(SyntaxFactory.Argument(SyntaxFactory.IdentifierName("value")))
                                                ]))))
                                ])))
                    ])))
                .WithExpressionBody(null)
                .WithInitializer(null)
                .WithTerminatorToken(SyntaxFactory.Token(SyntaxKind.None));

            return new MacroExpansionResult
            {
                ReplacementDeclaration = replacement,
                IntroducedMembers = [backingStorage]
            };
        }
    }

    public sealed class GenericInitializerMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "GenericInitializerMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new GenericInitializerReactivePropertyMacro()];
    }

    private sealed class GenericInitializerReactivePropertyMacro : IAttachedDeclarationMacro
    {
        public string Name => "Reactive";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var tree = SyntaxFactory.ParseSyntaxTree("""
                class __GeneratedContainer {
                    private val _TitleChanged: List<string> = List<string>()

                    var Title: string {
                        get => _Title
                        set {
                            _Title = value
                            _TitleChanged.Add(value)
                        }
                    }

                    private var _Title: string = ""
                }
                """);

            var container = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
            var changedList = Assert.IsType<PropertyDeclarationSyntax>(container.Members[0]);
            var property = Assert.IsType<PropertyDeclarationSyntax>(container.Members[1]);
            var backingStorage = Assert.IsType<PropertyDeclarationSyntax>(container.Members[2]);

            return new MacroExpansionResult
            {
                ReplacementDeclaration = property,
                IntroducedMembers = [changedList, backingStorage]
            };
        }
    }
}
