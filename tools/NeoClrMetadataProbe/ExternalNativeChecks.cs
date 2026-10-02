using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

using MetadataReference = Raven.CodeAnalysis.MetadataReference;

namespace NeoClrMetadataProbe;

internal static class ExternalNativeChecks
{
    internal static void Run(MetadataReference coreReference, AssemblyIdentity core, string output)
    {
        var payload = Library("NativePayloadLibrary", """
            namespace External
            public class Payload {
                public field Value: int
                public init(value: int) {
                    self.Value = value
                }
            }
            """, []);
        var holder = Library("NativeHolderLibrary", """
            namespace External
            public class Holder {
                public field Item: Payload
                public field Items: Payload[]
                public init(item: Payload) {
                    self.Item = item
                    self.Items = [item]
                }
                public init(items: Payload[]) {
                    self.Item = items[0]
                    self.Items = items
                }
                public var Current: Payload {
                    get => Item
                    set => Item = value
                }
                public var Batch: Payload[] {
                    get => Items
                    set => Items = value
                }
                public var self[index: int]: Payload {
                    get => Items[index]
                    set => Items[index] = value
                }
                public val self[key: string]: Payload {
                    get => Item
                    private set => Item = value
                }
                public val ReadOnly: Payload => Item
                public val Protected: int {
                    get => Item.Value
                    private set => Item.Value = value
                }
                public static val Answer: int => 42
                public static func PassItems(items: Payload[]) -> Payload[] => items
                public static func Numbers(items: int[]) -> int[] => items
                public static func Pass(value: Payload) -> Payload => value
            }
            """, [payload]);
        const string source = """
            import External.*
            func Main() -> int {
                let first = Payload(1)
                let holder = Holder(Holder.Pass(first))
                holder.Current = Payload(41)
                holder.Current.Value = Holder.Answer
                if holder.ReadOnly.Value != 42 { return 4 }
                if holder.Protected != 42 { return 5 }
                if first.Value != 1 { return 2 }
                let values: Payload[] = [first]
                let arrayHolder = Holder(Holder.PassItems(values))
                holder.Batch = arrayHolder.Batch
                holder[0] = holder.Item
                if holder[0].Value != 42 { return 6 }
                if holder["key"].Value != 42 { return 7 }
                if values[0].Value != 42 { return 3 }
                let numbers: int[] = [0]
                Holder.Numbers(numbers)[0] = values[0].Value
                return numbers[0]
            }
            """;
        File.WriteAllText(Path.Combine(output, "ExternalNativeConsumer.rvn"), source);
        foreach (var references in new MetadataReference[][] { [coreReference, holder, payload], [payload, holder, coreReference] })
        {
            var compilation = Compilation.Create("ExternalNativeConsumer", [SyntaxTree.ParseText(source)], references, CompilationOptions.NeoCLR);
            Check(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", compilation.GetDiagnostics()));
            var a = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(payload)!;
            var b = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(holder)!;
            var payloadType = a.GetTypeByMetadataName("External.Payload")!;
            var holderType = b.GetTypeByMetadataName("External.Holder")!;
            var pass = holderType.GetMembers("Pass").OfType<IMethodSymbol>().Single();
            Check(ReferenceEquals(pass.ReturnType, payloadType) && ReferenceEquals(pass.Parameters[0].Type, payloadType) &&
                ReferenceEquals(holderType.GetMembers("Item").OfType<IFieldSymbol>().Single().Type, payloadType) &&
                ReferenceEquals(holderType.InstanceConstructors.Single(c => c.Parameters[0].Type is not IArrayTypeSymbol).Parameters[0].Type, payloadType), "external canonical signature symbols");
            var arrayMethod = holderType.GetMembers("PassItems").OfType<IMethodSymbol>().Single();
            var arrayField = holderType.GetMembers("Items").OfType<IFieldSymbol>().Single();
            Check(arrayMethod.ReturnType is IArrayTypeSymbol { Rank: 1 } array && ReferenceEquals(array.ElementType, payloadType) &&
                ReferenceEquals(arrayMethod.ReturnType, arrayMethod.Parameters[0].Type) && ReferenceEquals(arrayMethod.ReturnType, arrayField.Type) &&
                ReferenceEquals(arrayMethod.ReturnType, holderType.InstanceConstructors.Single(c => c.Parameters[0].Type is IArrayTypeSymbol).Parameters[0].Type), "canonical native array symbols");
            var indexer = holderType.GetMembers().OfType<IPropertySymbol>().Single(p => p.IsIndexer && p.Parameters[0].Type.SpecialType == SpecialType.System_Int32);
            Check(indexer.Parameters.Length == 1 && ReferenceEquals(indexer.Type, payloadType) &&
                ReferenceEquals(indexer.GetMethod!.AssociatedSymbol, indexer) && ReferenceEquals(indexer.SetMethod!.AssociatedSymbol, indexer), "canonical indexer signature and accessors");
            var current = holderType.GetMembers("Current").OfType<IPropertySymbol>().Single();
            var batch = holderType.GetMembers("Batch").OfType<IPropertySymbol>().Single();
            Check(ReferenceEquals(current.Type, payloadType) && ReferenceEquals(batch.Type, arrayMethod.ReturnType) &&
                current.GetMethod is { MethodKind: MethodKind.PropertyGet } && current.SetMethod is { MethodKind: MethodKind.PropertySet } &&
                ReferenceEquals(current.GetMethod.AssociatedSymbol, current) && ReferenceEquals(current.SetMethod.AssociatedSymbol, current) &&
                holderType.GetMembers().Contains(current.GetMethod), "canonical native property/accessor identity");
            Check(holderType.GetMembers("ReadOnly").OfType<IPropertySymbol>().Single().SetMethod is null &&
                holderType.GetMembers("Protected").OfType<IPropertySymbol>().Single().SetMethod!.DeclaredAccessibility == Accessibility.Private &&
                holderType.GetMembers("Answer").OfType<IPropertySymbol>().Single().IsStatic, "readonly/private/static property contracts");
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new("ExternalNativeConsumer", new Version(1, 0, 0, 0)), core, [new(payload, payload.Definition, core), new(holder, holder.Definition, core)]));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "ExternalNativeConsumer.dll"), image.ToArray());
        }
        foreach (var assignment in new[] { "holder.ReadOnly = Payload(0)", "holder.Protected = 0", "holder[\"key\"] = Payload(0)", "holder[true] = Payload(0)" })
        {
            var syntax = SyntaxTree.ParseText("import External.*\nfunc Main() -> int {\nlet holder = Holder(Payload(1))\n" + assignment + "\nreturn 0\n}");
            Check(!syntax.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "invalid property test syntax");
            var invalidProperty = Compilation.Create("InvalidProperty", [syntax], [coreReference, holder, payload], CompilationOptions.NeoCLR);
            Check(invalidProperty.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "forbidden property assignment accepted: " + assignment);
        }
        var empty = new AssemblyBuilder(payload.Definition.Identity, core);
        empty.AddClass("External", "Different");
        var missingType = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteBinary(empty.WriteNativeAssembly(), core));
        var wrong = new AssemblyBuilder(new("NativePayloadLibrary", new Version(2, 0, 0, 0)), core);
        wrong.AddClass("External", "Payload");
        var wrongVersion = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteBinary(wrong.WriteNativeAssembly(), core));
        foreach (var references in new MetadataReference[][] { [coreReference, holder], [coreReference, holder, wrongVersion], [coreReference, holder, missingType], [coreReference, holder, payload, payload] })
        {
            var invalid = Compilation.Create("InvalidExternal", [SyntaxTree.ParseText("func Main() -> int { return 0 }")], references, CompilationOptions.NeoCLR);
            Check(invalid.GetDiagnostics().Any(d => d.Id == "RAVT003"), "invalid signature dependency accepted");
        }
        Console.WriteLine("PASS external native nominal signatures and exact dependency diagnostics");

        NeoClrMetadataReference Library(string name, string text, NeoClrMetadataReference[] dependencies)
        {
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(text)], [coreReference, .. dependencies],
                CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            using var image = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new(name, new Version(1, 0, 0, 0)), core, dependencies.Select(d => new NeoClrMetadataDependency(d, d.Definition, core)).ToArray()));
            Check(emitted.Success, name + ": " + string.Join("; ", emitted.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, name + ".dll"), image.ToArray());
            File.WriteAllText(Path.Combine(output, name + ".rvn"), text);
            return NeoClrMetadataReference.ReadAssembly(image.ToArray());
        }
    }
    private static void Check(bool value, string message) { if (!value) throw new Exception(message); }
}
