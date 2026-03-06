using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PropertyBindingTests : DiagnosticTestBase
{
    [Fact]
    public void StoredVarProperty_DeclarationBindsAsProperty()
    {
        const string testCode =
            """
            class Counter {
                var Count: int = 0
            }

            val counter = Counter()
            counter.Count = 1
            val current = counter.Count
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitFieldDeclaration_WithFieldKeyword_BindsAsStorageMember()
    {
        const string testCode =
            """
            class Counter {
                private field _count: int = 0

                func Increment() -> unit {
                    _count = _count + 1
                }
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void AccessingInstancePropertyAsStatic_ProducesDiagnostic()
    {
        string testCode =
            """
            class Foo {
                val Bar: int {
                    get => 0
                }
            }
            Foo.Bar
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.MemberDoesNotContainDefinition.Id).WithSpan(6, 5, 6, 8).WithArguments("Foo", "Bar")]);

        verifier.Verify();
    }

    [Fact]
    public void AccessingStaticPropertyAsInstance_ProducesDiagnostic()
    {
        string testCode =
            """
            class Foo {
                static val Bar: int {
                    get => 0
                }
            }
            val f = Foo()
            f.Bar;
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithSpan(7, 3, 7, 6).WithArguments("Bar")]);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyAutoProperty_AssignableInConstructor()
    {
        string testCode =
            """
            class Person {
                val Name: string { get; }
                init (name: string) {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyAutoProperty_AssignmentOutsideConstructor_ProducesDiagnostic()
    {
        string testCode =
            """
            class Person {
                val Name: string { get; }
                func SetName(name: string) -> unit {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0200").WithSpan(4, 9, 4, 13).WithArguments("Name")]);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyPropertyWithAccessorBody_AssignmentInConstructor_ProducesDiagnostic()
    {
        string testCode =
            """
            class Person {
                val Name: string {
                    get => ""
                }
                init (name: string) {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0200").WithSpan(6, 9, 6, 13).WithArguments("Name")]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyAccessorFieldKeyword_BindsToSynthesizedBackingField()
    {
        string testCode =
            """
            class Box {
                var Value: int {
                    get => field
                    set {
                        field = value;
                    }
                }
            }

            val box = Box()
            box.Value = 42
            val current = box.Value
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ValProperty_WithPublicSetAccessor_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                val Count: int {
                    get => field
                    set => field = value
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.ValPropertyCannotDeclareWritableAccessor.Id).WithSpan(4, 9, 4, 12).WithArguments("Count", "set")]);

        verifier.Verify();
    }

    [Fact]
    public void ValProperty_WithPrivateSetAccessor_DoesNotProduceDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                val Count: int {
                    get => field
                    private set => field = value
                }
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void ValProperty_WithPublicInitAccessor_DoesNotProduceDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                val Count: int {
                    get => field
                    init => field = value
                }
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void VarProperty_WithoutWritableShape_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                var Count: int {
                    get => field
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.VarPropertyRequiresWritableShape.Id).WithSpan(2, 5, 2, 8).WithArguments("Count")]);

        verifier.Verify();
    }

    [Fact]
    public void VarProperty_WithStorageInitializer_DoesNotProduceDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                var Count: int = 0
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void FieldKeyword_OutsidePropertyAccessor_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                func Read() -> int {
                    val value = field
                    return value
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.FieldKeywordOnlyValidInPropertyAccessor.Id).WithSpan(3, 21, 3, 26)]);

        verifier.Verify();
    }

    [Fact]
    public void FieldKeyword_InComputedProperty_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Counter {
                val Value: int => field
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.FieldKeywordRequiresPropertyBackingStorage.Id).WithSpan(2, 23, 2, 28).WithArguments("Value")]);

        verifier.Verify();
    }

    [Fact]
    public void ImplicitVarAutoProperty_OmittedAccessorList_NoErrors()
    {
        // var properties with omitted accessor list are treated as implicit auto-properties
        // (synthesized getter + setter + <Name>k__BackingField). No diagnostic expected.
        const string testCode =
            """
            class Entity {
                var Id: string
                var Count: int
                init(id: string, count: int) {
                    Id = id
                    Count = count
                }
            }
            val e = Entity("x", 1)
            val id = e.Id
            e.Count = 2
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void ImplicitValAutoProperty_OmittedAccessorList_NoErrors()
    {
        // val properties with omitted accessor list are treated as implicit getter-only auto-properties.
        const string testCode =
            """
            class Point {
                val X: int
                val Y: int
                init(x: int, y: int) {
                    X = x
                    Y = y
                }
            }
            val p = Point(3, 7)
            val x = p.X
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void ImplicitVarAutoProperty_DoesNotFireVarRequiresWritableShape()
    {
        // var with omitted accessor list synthesizes a setter, so VarPropertyRequiresWritableShape
        // must NOT fire (it would fire spuriously before the fix).
        const string testCode =
            """
            class Box {
                var Value: int
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void VarProperty_WithLessAccessibleSetter_ProducesDiagnostic()
    {
        const string testCode =
            """
            class Shipment {
                var Status: int { private set; }
            }
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult(CompilerDiagnostics.VarPropertyWritableAccessorMustMatchPropertyAccessibility.Id)
                    .WithSpan(2, 31, 2, 34)
                    .WithArguments("Status", "set"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ImplicitVarAutoProperty_MultipleProperties_InitAssignment_NoErrors()
    {
        // Regression: "Property Id does not have a setter" was thrown by the emitter
        // when assigning to a non-private var property with omitted accessor list.
        const string testCode =
            """
            class Shipment {
                var Id: string
                var IsPriority: bool
                init(id: string, isPriority: bool) {
                    Id = id
                    IsPriority = isPriority
                }
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void PrivateStoredProperty_SymbolInfo_ResolvesToProperty()
    {
        const string testCode =
            """
            class Counter {
                private var count: int = 0

                func Increment() -> () {
                    count = count + 1
                }

                func IncrementViaSelf() -> () {
                    self.count = self.count + 1
                }
            }
            """;

        var tree = SyntaxTree.ParseText(testCode);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var propertySyntax = root.DescendantNodes().OfType<PropertyDeclarationSyntax>().Single(p => p.Identifier.ValueText == "count");
        var property = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(propertySyntax));

        var countIdentifiers = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(identifier => identifier.Identifier.ValueText == "count")
            .ToArray();

        Assert.NotEmpty(countIdentifiers);
        foreach (var identifier in countIdentifiers)
        {
            var symbol = model.GetSymbolInfo(identifier).Symbol;
            var referencedProperty = Assert.IsAssignableFrom<IPropertySymbol>(symbol);
            Assert.True(SymbolEqualityComparer.Default.Equals(property, referencedProperty));
        }

        var memberAccesses = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Where(memberAccess => memberAccess.Name.Identifier.ValueText == "count")
            .ToArray();

        Assert.NotEmpty(memberAccesses);
        foreach (var memberAccess in memberAccesses)
        {
            var symbol = model.GetSymbolInfo(memberAccess).Symbol;
            var referencedProperty = Assert.IsAssignableFrom<IPropertySymbol>(symbol);
            Assert.True(SymbolEqualityComparer.Default.Equals(property, referencedProperty));
        }
    }

    [Fact]
    public void FieldKeyword_InAccessor_SymbolInfo_ResolvesToBackingField()
    {
        const string testCode =
            """
            class Box {
                private var value: int {
                    get => field
                    set => field = value
                }
            }
            """;

        var tree = SyntaxTree.ParseText(testCode);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var fieldIdentifiers = tree.GetRoot().DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(identifier => identifier.Identifier.ValueText == "field")
            .ToArray();

        Assert.NotEmpty(fieldIdentifiers);
        foreach (var fieldIdentifier in fieldIdentifiers)
        {
            var symbol = model.GetSymbolInfo(fieldIdentifier).Symbol;
            Assert.IsAssignableFrom<IFieldSymbol>(symbol);
        }
    }

    [Fact]
    public void StorageProperty_WithoutType_WithInitializer_InfersInitializerType()
    {
        const string testCode =
            """
            class Foo {
                val x = 2
            }
            """;

        var tree = SyntaxTree.ParseText(testCode);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var propertySyntax = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var propertySymbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(propertySyntax));

        Assert.Equal(SpecialType.System_Int32, propertySymbol.Type.SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.PropertyTypeAnnotationRequired);
    }

    [Fact]
    public void StorageProperty_WithoutType_WithoutInitializer_ReportsTypeAnnotationRequired()
    {
        const string testCode =
            """
            class Foo {
                val x
            }
            """;

        var tree = SyntaxTree.ParseText(testCode);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.PropertyTypeAnnotationRequired);
    }

}
