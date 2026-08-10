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
            .AddMacroReferences(
                new MacroReference(new IntroducedMethodMacro()),
                new MacroReference(new ObservablePropertyMacro()));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(true, method!.Invoke(null, null));
    }

    [Fact]
    public void AttachedTypeMacro_ReplacementBaseListAndIntroducedMethod_AreEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import System.*

            class Harness {
                public static func Run() -> bool {
                    let first: IEquatable<Widget> = Widget("Ada", 37)
                    return first.Equals(Widget("Ada", 37))
                }
            }

            #[AddEquatableContract]
            class Widget(val Name: string, val Age: int)
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new EquatableContractMacro()));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(true, method!.Invoke(null, null));
    }

    [Fact]
    public void AttachedUnionMacro_ReplacementBaseListAndIntroducedMethod_AreEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            interface IFailure {
                func Describe() -> string
            }

            class Harness {
                public static func Run() -> string {
                    let value: Failure = Failure.Unknown
                    let failure: IFailure = value
                    return failure.Describe()
                }
            }

            #[ErrorLike]
            union Failure {
                case Unknown
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new ErrorLikeUnionMacro()));

        var model = compilation.GetSemanticModel(syntaxTree);
        var unionDeclaration = syntaxTree.GetRoot().Members.OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(unionDeclaration));
        Assert.Contains(unionSymbol.Interfaces, type => type.Name == "IFailure");

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal("unknown failure", method!.Invoke(null, null));
    }

    [Fact]
    public void AttachedPropertyMacro_ReplacementProperty_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run() -> string {
                    let model = MyViewModel()
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
            .AddMacroReferences(
                new MacroReference(new IntroducedMethodMacro()),
                new MacroReference(new ObservablePropertyMacro()));

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
                    let model = MyViewModel()
                    model.Title = "Hello"
                    return model.GetTitleWriteCount()
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(
                new MacroReference(new IntroducedMethodMacro()),
                new MacroReference(new ObservablePropertyMacro()));

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
                    let model = MyViewModel()
                    model.Title = "Hello"
                    return model.Count
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new SampleLikeObservablePropertyMacro()));

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
    public void AttachedPropertyMacro_DiagnosticsTraverseReplacementDeclarationOnly()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class MyViewModel {
                #[Observable]
                var Title: string = ""
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(
                new MacroReference(new IntroducedMethodMacro()),
                new MacroReference(new ObservablePropertyMacro()));

        var model = compilation.GetSemanticModel(syntaxTree);
        var propertyDeclaration = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single(static p => p.Identifier.ValueText == "Title");

        _ = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(propertyDeclaration));

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Id == "RAV0111");
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
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
                    let model = MyViewModel()
                    model.Title = "Hello"
                    return model.Count
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new DetachedSyntaxFactoryObservablePropertyMacro()));

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
                    let model = MyViewModel()
                    model.Title = "Hello"
                    return model.Title
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new GenericInitializerReactivePropertyMacro()));

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

    public sealed class IntroducedMethodMacro : IAttachedDeclarationMacro
    {
        public string Name => "AddEquatable";

        public string Namespace => string.Empty;

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

            return MacroExpansionResult.FromIntroducedMembers([method]);
        }
    }

    public sealed class EquatableContractMacro : IAttachedDeclarationMacro
    {
        public string Name => "AddEquatableContract";

        public string Namespace => string.Empty;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var current = Assert.IsType<ClassDeclarationSyntax>(context.CurrentDeclaration);
            var typeName = current.Identifier.ValueText;
            var tree = SyntaxFactory.ParseSyntaxTree($$"""
                class __GeneratedContainer : System.IEquatable<{{typeName}}> {
                    func Equals(other: {{typeName}}) -> bool {
                        return Name == other.Name && Age == other.Age
                    }
                }
                """);
            var container = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
            var method = Assert.IsType<MethodDeclarationSyntax>(container.Members.Single());

            return MacroExpansionResult.FromReplacement(
                current.WithBaseList(container.BaseList),
                [method]);
        }
    }

    public sealed class ErrorLikeUnionMacro : IAttachedDeclarationMacro
    {
        public string Name => "ErrorLike";

        public string Namespace => string.Empty;

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var current = Assert.IsType<UnionDeclarationSyntax>(context.CurrentDeclaration);
            var tree = SyntaxFactory.ParseSyntaxTree("""
                union __GeneratedContainer: IFailure {
                    case Placeholder
                    func Describe() -> string => "unknown failure"
                }
                """);
            var container = Assert.IsType<UnionDeclarationSyntax>(tree.GetRoot().Members.Single());
            var method = Assert.IsType<MethodDeclarationSyntax>(container.Members.OfType<MethodDeclarationSyntax>().Single());

            return MacroExpansionResult.FromReplacement(
                current.WithBaseList(container.BaseList),
                [method]);
        }
    }

    public sealed class ObservablePropertyMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public string Namespace => string.Empty;

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

    private sealed class SampleLikeObservablePropertyMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public string Namespace => string.Empty;

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
                            let oldValue = _Title
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

    private sealed class DetachedSyntaxFactoryObservablePropertyMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public string Namespace => string.Empty;

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

    private sealed class GenericInitializerReactivePropertyMacro : IAttachedDeclarationMacro
    {
        public string Name => "Reactive";

        public string Namespace => string.Empty;

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
