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
                private var stored: TItem
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
        const string source = """
            import Generics.*
            class Item { var Number: int = 42 }
            func Forward<T>(value: T) -> T => Identity<T>(value)
            func Main() -> int {
                let item = Item()
                let same = Identity(item)
                same.Number = 7
                if item.Number != 7 { return 1 }
                if Forward<Item>(item).Number != 7 { return 2 }
                let values: int[] = [19, 23]
                let box = OpenBox(EchoBox(CreateBox(19)))
                let boxes: Box<int>[] = [box]
                OpenBoxes(boxes)[0].Same(box).Set(42)
                if box.Current != 42 { return 6 }
                let nominal = Box<Item>(item)
                nominal.Current.Number = 9
                if item.Number != 9 { return 7 }
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
        foreach (var references in new MetadataReference[][] { [coreReference, reference], [reference, coreReference] })
        {
            var tree = SyntaxTree.ParseText(source);
            var compilation = Compilation.Create("NativeGenericConsumer", [tree], references, CompilationOptions.NeoCLR);
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            Check(errors.Length == 0, string.Join("; ", errors.Select(d => d.ToString())));
            var assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
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
                new(new("NativeGenericConsumer", new Version(1, 0, 0, 0)), core, [new(reference, reference.Definition, core)]));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "NativeGenericConsumer.dll"), image.ToArray());
            var invalid = Compilation.Create("InvalidGenericCall", [SyntaxTree.ParseText("import Generics.*\nfunc Wrong() -> int => Identity<int>(true)")], references,
                CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            var wrongConstruction = Compilation.Create("WrongConstruction", [SyntaxTree.ParseText("import Generics.*\nfunc Wrong(value: Box<bool>) -> Box<int> => OpenBox(value)")], references,
                CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            Check(wrongConstruction.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "incompatible generic constructions diagnose");
            Check(invalid.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "invalid generic argument diagnoses");
        }
        File.WriteAllText(Path.Combine(output, "NativeGenericConsumer.rvn"), source);
        Console.WriteLine("PASS direct native generic symbols, inference, substitution and emission");
    }
    private static void Check(bool value, string message) { if (!value) throw new Exception(message); }
}
