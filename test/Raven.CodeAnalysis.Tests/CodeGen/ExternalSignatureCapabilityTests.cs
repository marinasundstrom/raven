using System.Reflection;
using System.Runtime.Loader;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ExternalSignatureCapabilityTests
{
    [Fact]
    public void TransportedUnitCallbackUsesSubstitutedResultConvention()
    {
        var app = Compilation.Create("UnitCallbackAdmission", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Apply(action: (int) -> unit) { action(42) }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        var callback = (INamedTypeSymbol)method.Parameters[0].Type;
        var invoke = callback.GetMembers("Invoke").OfType<IMethodSymbol>().Single();
        Assert.False(LinearMethodBody.ReturnsValue(invoke));
    }

    [Fact]
    public void ExplicitUnitContractSupportsValuesWithoutChangingVoidCalls()
    {
        var app = Compilation.Create("UnitAdmission", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Notify() { }
                public static func Accept(value: unit) -> int {
                    Notify()
                    let copy = ()
                    return 42
                }
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                .WithTargetCoreAssemblyName("System.Runtime")
                .WithRuntimeUnitContract(new RuntimeUnitContract("System.Runtime", "System.ValueTuple")));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var methods = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>()
            .Select(s => (IMethodSymbol)app.GetSemanticModel(s.SyntaxTree).GetDeclaredSymbol(s)!).ToArray();
        var values = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsExternalValueSignatures: true);
        var accept = methods.Single(m => m.Name == "Accept");
        Assert.False(CallableSignature.TryCreate(accept, out _, ReflectionEmitCapabilities.Shared));
        Assert.True(SourceCallablePlan.TryCreate(accept, out var plan, values));
        Assert.True(plan!.TryLowerBody(app, _ => false, out _, out var failure, values), failure?.Detail);
        Assert.True(CallableSignature.TryCreate(methods.Single(m => m.Name == "Notify"), out var signature, values));
        Assert.False(signature.ReturnsValue);
        using var image = new MemoryStream();
        var result = app.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var context = new AssemblyLoadContext("unit-admission", true);
        try
        {
            image.Position = 0;
            var loaded = context.LoadFromStream(image);
            Assert.Equal(42, loaded.GetType("Consumer")!.GetMethod("Accept")!.Invoke(null, [new ValueTuple()]));
        }
        finally { context.Unload(); }
    }

    [Theory]
    [InlineData("struct", true)]
    [InlineData("class", false)]
    public void ImportedStaticPropertyUsesOwnerCategoryCapability(string kind, bool valueOwner)
    {
        var library = Compilation.Create("StaticPropertyContract", [SyntaxTree.ParseText($$"""
            public {{kind}} Holder {
                public static val Answer: int => 42
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var built = library.Emit(libraryImage);
        Assert.True(built.Success, string.Join("\n", built.Diagnostics));
        var app = Compilation.Create("StaticPropertyConsumer", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Run() -> int => Holder.Answer
            }
            """)], TestMetadataReferences.Default.Append(MetadataReference.CreateFromImage(libraryImage.ToArray())).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        EmissionCapabilities Capabilities(bool values, bool references) => new(
            Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(),
            Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsExternalValueSignatures: values, allowsExternalReferenceSignatures: references);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true, true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false, false)));
        Assert.Equal(valueOwner, plan.TryLowerBody(app, _ => false, out _, out _, Capabilities(true, false)));
        Assert.Equal(!valueOwner, plan.TryLowerBody(app, _ => false, out _, out _, Capabilities(false, true)));
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("static-property-" + kind, true);
        try
        {
            libraryImage.Position = 0;
            context.LoadFromStream(libraryImage);
            image.Position = 0;
            var loaded = context.LoadFromStream(image);
            Assert.Equal(42, loaded.GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
        }
        finally { context.Unload(); }
    }

    [Fact]
    public void GenericUnboxingRequiresExplicitCapability()
    {
        var app = Compilation.Create("UnboxingAdmission", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Extract<T>(value: object) -> T => (T)value
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        EmissionCapabilities Capabilities(bool boxing) => new(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>().Where(kind => boxing || kind != LinearInstructionKind.UnboxAny),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsGenericMethods: true, allowsExternalReferenceSignatures: true);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
    }

    [Fact]
    public void GenericBoxingRequiresExplicitCapability()
    {
        var app = Compilation.Create("BoxingAdmission", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Box<T>(value: T) -> object => value
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        EmissionCapabilities Capabilities(bool boxing) => new(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>().Where(kind => boxing || kind != LinearInstructionKind.BoxToObject),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsGenericMethods: true, allowsExternalReferenceSignatures: true);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
    }

    [Fact]
    public void ReferenceEnumerationPreservesBreakContinueAndResult()
    {
        var app = Compilation.Create("EnumerationAdmission", [SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            public static class Consumer {
                public static func Read(values: IEnumerable<int>) -> int {
                    var total = 0
                    for value in values {
                        if value == 1 { continue }
                        if value == 99 { break }
                        total = total + value
                    }
                    return total
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        EmissionCapabilities Capabilities(bool enumeration) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsGenericClassOwners: true, allowsInterfaceSignatures: true,
            allowsExternalReferenceSignatures: true, allowsExternalInstanceCalls: true, allowsInterfaceDispatch: true, allowsReferenceEnumeration: enumeration);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        using var image = new MemoryStream(); var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("enumeration-admission", true);
        try { image.Position = 0; Assert.Equal(42, context.LoadFromStream(image).GetType("Consumer")!.GetMethod("Read")!.Invoke(null, [new[] { 1, 19, 23, 99, 7 }])); }
        finally { context.Unload(); }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ImportedCasePatternUsesCheckedPayloadAndPreservesDotNetBehavior(bool generic)
    {
        var library = Compilation.Create(generic ? "GenericCaseLibrary" : "CaseLibrary", [SyntaxTree.ParseText($$"""
            public union Choice{{(generic ? "<T>" : "")}} {
                case Some({{(generic ? "T" : "int")}})
                case None
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var built = library.Emit(libraryImage);
        Assert.True(built.Success, string.Join("\n", built.Diagnostics));
        var app = Compilation.Create(generic ? "GenericCaseConsumer" : "CaseConsumer", [SyntaxTree.ParseText($$"""
            import Choice.*
            public static class Consumer {
                public static func Read(choice: Choice{{(generic ? "<int>" : "")}}) -> int {
                    var result = 0
                    match choice {
                        Some(let value) => { result = value }
                        None => { result = 0 }
                    }
                    return result
                }
                public static func Run() -> int => Read(Some{{(generic ? "<int>" : "")}}(42)) + Read(None())
            }
            """)], TestMetadataReferences.Default.Append(MetadataReference.CreateFromImage(libraryImage.ToArray())).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsExternalValueSignatures: true, allowsExternalValueInstanceCalls: true, allowsManagedReferences: true,
            allowsExternalConstructors: true, allowsNestedExternalTypes: true, allowsCasePatterns: true, allowsGenericClassOwners: true);
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.Text == "Read");
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(app, _ => false, out _, out var failure, capabilities), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("case-admission", true);
        try
        {
            libraryImage.Position = 0; context.LoadFromStream(libraryImage);
            image.Position = 0; Assert.Equal(42, context.LoadFromStream(image).GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
        }
        finally { context.Unload(); }
    }

    [Fact]
    public void LoweredExtensionCallsRequireExplicitCapability()
    {
        var app = Compilation.Create("ExtensionAdmission", [SyntaxTree.ParseText("""
            import System.Runtime.CompilerServices.*
            public static class NumberExtensions {
                [ExtensionAttribute]
                public static func Double(value: int) -> int => value + value
            }
            public static class Consumer {
                public static func Run() -> int {
                    let value = 21
                    return value.Double()
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.Text == "Run");
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        EmissionCapabilities Capabilities(bool extensions) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsArrays: true, allowsInterfaceSignatures: true, allowsExternalReferenceSignatures: true, allowsGenericClassOwners: true,
            allowsLoweredExtensionCalls: extensions);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("extension-admission", true);
        try { image.Position = 0; Assert.Equal(42, context.LoadFromStream(image).GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null)); }
        finally { context.Unload(); }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FunctionValuesRequireExplicitCapabilityAndPreserveDotNetExecution(bool lambda)
    {
        var app = Compilation.Create("FunctionAdmission", [SyntaxTree.ParseText($$"""
            public static class Consumer {
                public static func Increment(value: int) -> int => value + 2
                public static func Apply(callback: (int) -> int, value: int) -> int => callback(value)
                public static func Run() -> int {
                    let callback: (int) -> int = {{(lambda ? "(value: int) => value + 2" : "Increment")}}
                    return Apply(callback, 40)
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        EmissionCapabilities Capabilities(bool functions) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsFunctionValues: functions);
        foreach (var syntax in app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
            Assert.True(plan!.TryLowerBody(app, _ => false, out var body, out var failure, Capabilities(true)), failure?.Detail);
            foreach (var (function, location) in body!.Functions)
                Assert.True(LinearMethodBody.TryLower((IMethodSymbol)function.Symbol!, app.GetSemanticModel(location.SyntaxTree), location, _ => false, out _, out var nestedFailure, Capabilities(true), function), nestedFailure?.Detail);
            if (method.Name == "Apply") Assert.False(SourceCallablePlan.TryCreate(method, out _, Capabilities(false)));
        }
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("function-admission", true);
        try { image.Position = 0; Assert.Equal(42, context.LoadFromStream(image).GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null)); }
        finally { context.Unload(); }
    }

    [Fact]
    public void NestedImportsRequireExplicitCapability()
    {
        var library = Compilation.Create("NestedContract", [SyntaxTree.ParseText("""
            public class Container {
                public struct Item {
                    public var Value: int
                    public init(value: int) { Value = value }
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var built = library.Emit(libraryImage);
        Assert.True(built.Success, string.Join("\n", built.Diagnostics));
        var app = Compilation.Create("NestedConsumer", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Create() -> Container.Item => Container.Item(42)
            }
            """)], TestMetadataReferences.Default.Append(MetadataReference.CreateFromImage(libraryImage.ToArray())).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        EmissionCapabilities Capabilities(bool nested) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsExternalValueSignatures: true, allowsExternalConstructors: true, allowsNestedExternalTypes: nested);
        Assert.False(SourceCallablePlan.TryCreate(method, out _, Capabilities(false)));
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.True(plan!.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("nested-admission", true);
        try
        {
            libraryImage.Position = 0; context.LoadFromStream(libraryImage);
            image.Position = 0; var loaded = context.LoadFromStream(image);
            var item = loaded.GetType("Consumer")!.GetMethod("Create")!.Invoke(null, null)!;
            Assert.Equal(42, item.GetType().GetProperty("Value")!.GetValue(item));
        }
        finally { context.Unload(); }
    }

    [Fact]
    public void ImportedConstructorsRequireExplicitCapability()
    {
        var app = Compilation.Create("ConstructorCapability", [SyntaxTree.ParseText("""
            import System.*
            public static class Consumer {
                public static func Create() -> DateTime => DateTime(2026, 10, 2)
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        EmissionCapabilities Capabilities(bool constructors) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsExternalValueSignatures: true, allowsExternalConstructors: constructors);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(false)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("constructor-admission", true);
        try
        {
            image.Position = 0;
            Assert.Equal(new DateTime(2026, 10, 2), context.LoadFromStream(image).GetType("Consumer")!.GetMethod("Create")!.Invoke(null, null));
        }
        finally { context.Unload(); }
    }

    [Fact]
    public void ValueReceiverCallsRequireExplicitAddressCapability()
    {
        var library = Compilation.Create("ValueReceiverContract", [SyntaxTree.ParseText("""
            public struct Counter {
                private var value: int = 0
                public func TryGet(out output: int) -> bool {
                    value = value + 42
                    output = value
                    return true
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var built = library.Emit(libraryImage);
        Assert.True(built.Success, string.Join("\n", built.Diagnostics));
        var app = Compilation.Create("ValueReceiverConsumer", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Run(input: Counter) -> int {
                    var value = input
                    var result = 0
                    if !value.TryGet(out result) {
                        return 0
                    }
                    return result
                }
            }
            """)], TestMetadataReferences.Default.Append(MetadataReference.CreateFromImage(libraryImage.ToArray())).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        EmissionCapabilities Capabilities(bool receivers) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsExternalValueSignatures: true, allowsManagedReferences: true, allowsExternalValueInstanceCalls: receivers);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(false)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("value-receiver-admission", true);
        try
        {
            libraryImage.Position = 0; var loadedLibrary = context.LoadFromStream(libraryImage);
            image.Position = 0; var loaded = context.LoadFromStream(image);
            var input = Activator.CreateInstance(loadedLibrary.GetType("Counter")!);
            Assert.Equal(42, loaded.GetType("Consumer")!.GetMethod("Run")!.Invoke(null, [input]));
        }
        finally { context.Unload(); }
    }

    [Fact]
    public void ImportedInterfaceCallsRequireExplicitDispatchCapability()
    {
        var library = Compilation.Create("InterfaceContracts", [SyntaxTree.ParseText("""
            public interface Value<T> {
                func Echo(value: T) -> T
            }
            public class Concrete : Value<int> {
                public func Echo(value: int) -> int => value
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var built = library.Emit(libraryImage);
        Assert.True(built.Success, string.Join("\n", built.Diagnostics));
        var reference = MetadataReference.CreateFromImage(libraryImage.ToArray());
        var app = Compilation.Create("InterfaceConsumer", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Accept(value: Value<int>) -> int => value.Echo(42)
            }
            """)], TestMetadataReferences.Default.Append(reference).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        EmissionCapabilities Capabilities(bool dispatch) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: true, allowsGenericClassOwners: true, allowsInterfaceSignatures: true,
            allowsInterfaceDispatch: true, allowsExternalReferenceSignatures: true, allowsExternalInstanceCalls: dispatch);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(false)));
        Assert.False(plan!.TryLowerBody(app, _ => false, out _, out _, Capabilities(false)));
        Assert.True(plan.TryLowerBody(app, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        using var image = new MemoryStream();
        var result = app.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var context = new AssemblyLoadContext("interface-admission", isCollectible: true);
        try
        {
            libraryImage.Position = 0; var loadedLibrary = context.LoadFromStream(libraryImage);
            image.Position = 0; var loaded = context.LoadFromStream(image);
            var value = Activator.CreateInstance(loadedLibrary.GetType("Concrete")!);
            Assert.Equal(42, loaded.GetType("Consumer")!.GetMethod("Accept")!.Invoke(null, [value]));
        }
        finally { context.Unload(); }
    }

    [Fact]
    public void ExternalValueSignaturesRequireTheirOwnOptIn()
    {
        var app = Compilation.Create("ValueAdmission", [SyntaxTree.ParseText("""
            import System.*
            public static class Consumer {
                public static func Accept(value: DateTime) -> int => 42
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        var references = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true);
        var values = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsExternalValueSignatures: true);
        Assert.False(CallableSignature.TryCreate(method, out _, references));
        Assert.False(CallableSignature.TryCreate(method, out _, ReflectionEmitCapabilities.Shared));
        Assert.True(CallableSignature.TryCreate(method, out var signature, values));
        Assert.True(values.Allows(signature));
        Assert.False(references.Allows(signature));
        using var image = new MemoryStream();
        var result = app.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var context = new AssemblyLoadContext("value-admission", isCollectible: true);
        try
        {
            image.Position = 0;
            var assembly = context.LoadFromStream(image);
            Assert.Equal(42, assembly.GetType("Consumer")!.GetMethod("Accept")!.Invoke(null, [new DateTime(2026, 10, 1)]));
        }
        finally { context.Unload(); }
    }

    [Theory]
    [InlineData(OptimizationLevel.Debug)]
    [InlineData(OptimizationLevel.Release)]
    public void ExternalSignaturesRequireOptInAndOrdinaryDotNetStillExecutes(OptimizationLevel optimization)
    {
        var library = Compilation.Create("ExternalContracts", [SyntaxTree.ParseText("public class Box<T> { }")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var libraryResult = library.Emit(libraryImage);
        Assert.True(libraryResult.Success, string.Join("\n", libraryResult.Diagnostics));
        var reference = MetadataReference.CreateFromImage(libraryImage.ToArray());
        var app = Compilation.Create("ExternalConsumer", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Accept(value: Box<int>?) -> int => 42
            }
            """)], TestMetadataReferences.Default.Append(reference).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.False(CallableSignature.TryCreate(method, out _));
        Assert.False(CallableSignature.TryCreate(method, out _, ReflectionEmitCapabilities.Shared));
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsRootClassSignatures: true, allowsGenericClassOwners: true, allowsExternalReferenceSignatures: true);
        Assert.True(CallableSignature.TryCreate(method, out var signature, capabilities));
        Assert.True(capabilities.Allows(signature));
        Assert.False(ReflectionEmitCapabilities.Shared.Allows(signature));
        using var appImage = new MemoryStream();
        var result = app.Emit(appImage);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var context = new AssemblyLoadContext("external-signature-" + optimization, isCollectible: true);
        try
        {
            libraryImage.Position = 0;
            context.LoadFromStream(libraryImage);
            appImage.Position = 0;
            var assembly = context.LoadFromStream(appImage);
            Assert.Equal(42, assembly.GetType("Consumer")!.GetMethod("Accept")!.Invoke(null, [null]));
        }
        finally { context.Unload(); }
    }
}
