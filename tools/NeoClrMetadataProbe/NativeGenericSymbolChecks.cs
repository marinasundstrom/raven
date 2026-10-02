using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeGenericSymbolChecks
{
    internal static void Run(MetadataReference coreReference, AssemblyIdentity core, string output)
    {
        const string librarySource = """
            namespace Generics
            public func CreateBox(value: int) -> Box<int> => Box<int>(value)
            public func EchoBox(value: Box<int>) -> Box<int> => value
            public func OpenBox<T>(value: Box<T>) -> Box<T> => value
            public func OpenBoxes<T>(values: Box<T>[]) -> Box<T>[] => values
            public func Identity<T>(value: T) -> T => value
            public func ArrayIdentity<T>(values: T[]) -> T[] => values
            public class Box<TItem> {
                public field stored: TItem
                public init(value: TItem) { stored = value }
                public val Current: TItem => stored
                public func Set(value: TItem) { stored = value }
                public func Echo(values: TItem[]) -> TItem[] => values
                public func Same(value: Box<TItem>) -> Box<TItem> => value
            }
            public static class Algorithms {
                public static func First<T>(values: T[]) -> T => values[0]
                public static func Set<T>(values: T[], value: T) { values[0] = value }
                public static func Choose<T>(value: T) -> T => value
                public static func Choose<T, U>(value: T, ignored: U) -> T => value
            }
            """;
        var library = Compilation.Create("NativeGenericLibrary", [SyntaxTree.ParseText(librarySource)], [coreReference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var libraryResult = NeoClrCompilationEmitter.EmitMetadataAssembly(library, libraryImage,
            new(new("NativeGenericLibrary", new Version(1, 0, 0, 0)), core, []));
        Check(libraryResult.Success, string.Join("; ", libraryResult.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "NativeGenericLibrary.dll"), libraryImage.ToArray());
        File.WriteAllText(Path.Combine(output, "NativeGenericLibrary.rvn"), librarySource);
        var reference = NeoClrMetadataReference.ReadAssembly(libraryImage.ToArray());
        const string bridgeSource = """
            namespace GenericBridge
            import Generics.*
            public func Create(value: int) -> Box<int> => CreateBox(value)
            public class BoxStorage {
                public field Value: Box<int>
                public field Values: Box<int>[]
                public init(value: Box<int>, values: Box<int>[]) {
                    Value = value
                    Values = values
                }
            }
            public func RelayBox<T>(value: Box<T>) -> Box<T> => OpenBox(value)
            public func RelayBoxes<T>(values: Box<T>[]) -> Box<T>[] => OpenBoxes(values)
            """;
        var bridge = Compilation.Create("NativeGenericBridge", [SyntaxTree.ParseText(bridgeSource)], [coreReference, reference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var bridgeImage = new MemoryStream();
        var bridgeResult = NeoClrCompilationEmitter.EmitMetadataAssembly(bridge, bridgeImage,
            new(new("NativeGenericBridge", new Version(1, 0, 0, 0)), core, [new(reference, core)]));
        Check(bridgeResult.Success, string.Join("; ", bridgeResult.Diagnostics));
        var bridgeReference = NeoClrMetadataReference.ReadAssembly(bridgeImage.ToArray());
        File.WriteAllBytes(Path.Combine(output, "NativeGenericBridge.dll"), bridgeImage.ToArray());
        File.WriteAllText(Path.Combine(output, "NativeGenericBridge.rvn"), bridgeSource);
        const string source = """
            import Generics.*
            import GenericBridge.*
            class Item { var Number: int = 42 }
            func Forward<T>(value: T) -> T => Identity<T>(value)
            func ReadBox<T>(box: Box<T>) -> T => box.stored
            func WriteBox<T>(box: Box<T>, value: T) { box.stored = value }
            func Main() -> int {
                let item = Item()
                let same = Identity(item)
                same.Number = 7
                if item.Number != 7 { return 1 }
                if Forward<Item>(item).Number != 7 { return 2 }
                let values: int[] = [19, 23]
                let box = GenericBridge.RelayBox<int>(EchoBox(GenericBridge.Create(19)))
                let boxes: Box<int>[] = [box]
                GenericBridge.RelayBoxes(boxes)[0].Same(box).Set(42)
                if box.Current != 42 { return 6 }
                box.stored = 41
                if box.stored != 41 { return 11 }
                box.stored = 42
                let storage = BoxStorage(box, boxes)
                if storage.Value.Current != 42 { return 8 }
                storage.Value = Box<int>(19)
                storage.Values = [storage.Value]
                storage.Values[0].Set(23)
                if storage.Value.Current != 23 { return 9 }
                if box.Current != 42 { return 10 }
                let nominal = Box<Item>(item)
                nominal.Current.Number = 9
                if item.Number != 9 { return 7 }
                WriteBox(nominal, Item())
                if ReadBox(nominal).Number != 42 { return 12 }
                let alias = box.Echo(ArrayIdentity<int>(values))
                Algorithms.Set<int>(alias, 42)
                if Algorithms.Choose<int>(7) != 7 { return 3 }
                if Algorithms.Choose<int, bool>(8, true) != 8 { return 4 }
                let wide: long[] = [5000000000L]
                if Algorithms.First<long>(wide) != 5000000000L { return 5 }
                return Algorithms.First<int>(values)
            }
            """;
        IMethodSymbol? previousIdentity = null;
        foreach (var references in new MetadataReference[][] { [coreReference, reference, bridgeReference], [bridgeReference, reference, coreReference] })
        {
            var tree = SyntaxTree.ParseText(source);
            var compilation = Compilation.Create("NativeGenericConsumer", [tree], references, CompilationOptions.NeoCLR);
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            Check(errors.Length == 0, string.Join("; ", errors.Select(d => d.ToString())));
            var assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
            var bridgeAssembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(bridgeReference)!;
            var bridgeMethod = bridgeAssembly.GlobalNamespace.LookupNamespace("GenericBridge")!.GetMembers("Create").OfType<IMethodSymbol>().Single();
            Check(bridgeMethod.ReturnType is INamedTypeSymbol externalBox && ReferenceEquals(externalBox.OriginalDefinition, assembly.GetTypeByMetadataName("Generics.Box`1")), "canonical external constructed definition");
            var relayMethod = bridgeAssembly.GlobalNamespace.LookupNamespace("GenericBridge")!.GetMembers("RelayBox").OfType<IMethodSymbol>().Single();
            Check(relayMethod.ReturnType is INamedTypeSymbol externalOpen &&
                ReferenceEquals(externalOpen.TypeArguments[0], relayMethod.TypeParameters[0]) &&
                ReferenceEquals(externalOpen.OriginalDefinition, assembly.GetTypeByMetadataName("Generics.Box`1")), "external construction retains declaring method scope");
            var ns = assembly.GlobalNamespace.LookupNamespace("Generics")!;
            var boxDefinition = assembly.GetTypeByMetadataName("Generics.Box`1")!;
            Check(boxDefinition.Name == "Box" && boxDefinition.Arity == 1 && boxDefinition.TypeParameters[0].Name == "TItem" &&
                ReferenceEquals(boxDefinition.TypeParameters[0].DeclaringTypeParameterOwner, boxDefinition), "native generic owner identity");
            var boxType = (INamedTypeSymbol)boxDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
            Check(boxType.InstanceConstructors.Single().Parameters[0].Type.SpecialType == SpecialType.System_Int32 &&
                boxType.GetMembers("Current").OfType<IPropertySymbol>().Single().Type.SpecialType == SpecialType.System_Int32,
                "shared constructed owner substitution");
            var openMethod = ns.GetMembers("OpenBox").OfType<IMethodSymbol>().Single();
            Check(openMethod.ReturnType is INamedTypeSymbol openResult &&
                ReferenceEquals(openResult.TypeArguments[0], openMethod.TypeParameters[0]) &&
                ReferenceEquals(openMethod.ReturnType, openMethod.Parameters[0].Type), "constructed signature retains method scope");
            var sameMethod = boxDefinition.GetMembers("Same").OfType<IMethodSymbol>().Single();
            Check(sameMethod.ReturnType is INamedTypeSymbol ownerResult &&
                ReferenceEquals(ownerResult.TypeArguments[0], boxDefinition.TypeParameters[0]), "constructed signature retains owner scope");
            var identity = ns.GetMembers("Identity").OfType<IMethodSymbol>().Single();
            Check(identity.IsGenericMethod && identity.Arity == 1 && identity.TypeParameters[0].Name == "T" &&
                identity.TypeParameters[0].Ordinal == 0 && ReferenceEquals(identity.TypeParameters[0].DeclaringMethodParameterOwner, identity) &&
                ReferenceEquals(identity.ReturnType, identity.TypeParameters[0]) && ReferenceEquals(identity.Parameters[0].Type, identity.TypeParameters[0]), "canonical method parameter ownership");
            Check(!ReferenceEquals(previousIdentity, identity), "compilation-owned generic symbols");
            previousIdentity = identity;
            var array = ns.GetMembers("ArrayIdentity").OfType<IMethodSymbol>().Single();
            Check(!ReferenceEquals(array.TypeParameters[0], identity.TypeParameters[0]) &&
                array.ReturnType is IArrayTypeSymbol vector && ReferenceEquals(vector.ElementType, array.TypeParameters[0]) &&
                ReferenceEquals(array.ReturnType, array.Parameters[0].Type), "method-scoped vector parameter identity");
            var constructed = identity.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
            Check(ReferenceEquals(constructed.OriginalDefinition, identity) && constructed.ReturnType.SpecialType == SpecialType.System_Int32 &&
                constructed.Parameters[0].Type.SpecialType == SpecialType.System_Int32, "shared constructed method substitution");
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new("NativeGenericConsumer", new Version(1, 0, 0, 0)), core, [new(reference, core), new(bridgeReference, core)]));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "NativeGenericConsumer.dll"), image.ToArray());
            var invalid = Compilation.Create("InvalidGenericCall", [SyntaxTree.ParseText("import Generics.*\nfunc Wrong() -> int => Identity<int>(true)")], references,
                CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            var wrongConstruction = Compilation.Create("WrongConstruction", [SyntaxTree.ParseText("import Generics.*\nfunc Wrong(value: Box<bool>) -> Box<int> => OpenBox(value)")], references,
                CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            var wrongExplicit = Compilation.Create("WrongQualifiedArgument", [SyntaxTree.ParseText("import Generics.*\nfunc Wrong(value: Box<int>) -> Box<int> => GenericBridge.RelayBox<bool>(value)")], references,
                CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            Check(wrongExplicit.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "qualified explicit type argument is enforced");
            Check(wrongConstruction.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "incompatible generic constructions diagnose");
            Check(invalid.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "invalid generic argument diagnoses");
        }
        var missing = Compilation.Create("MissingGenericDependency", [SyntaxTree.ParseText("func Main() -> int => 0")], [coreReference, bridgeReference], CompilationOptions.NeoCLR);
        Check(missing.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "missing external generic dependency diagnoses");
        File.WriteAllText(Path.Combine(output, "NativeGenericConsumer.rvn"), source);
        Console.WriteLine("PASS direct native generic symbols, inference, substitution and emission");
    }
    private static void Check(bool value, string message) { if (!value) throw new Exception(message); }
}
