using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeTypeChecks
{
    internal static void Run(MetadataReference coreReference, AssemblyIdentity core, string output)
    {
        const string source = """
            namespace Example
            public static class NativeMath {
                public static func Create(value: int) -> Calculator => Calculator(value)
                public static func Pass(value: Calculator) -> Calculator => value
                public static func Echo(value: int) -> int => value
                public static func Echo(value: bool) -> bool => value
                internal static func Hidden() -> int => 0
                private static func Secret() -> int => 0
            }
            public class Calculator {
                private var stored: int
                public field Visible: int
                public init(value: int) {
                    self.stored = value
                    self.Visible = value
                }
                public func Same(value: Calculator) -> Calculator => value
                public func Add(value: int) -> int => stored + value
                private func Secret() -> int => 0
            }
            public class Snapshot {
                public field Value: int
                public init(source: Calculator) {
                    self.Value = source.Visible
                }
            }
            public func PassCalculator(value: Calculator) -> Calculator => value
            public class Restricted {
                private init() {}
            }
            internal static class HiddenType {
                public static func Value() -> int => 0
            }
            """;
        var library = Compilation.Create("NativeTypeLibrary", [SyntaxTree.ParseText(source)], [coreReference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(library, libraryImage,
            new(new("NativeTypeLibrary", new Version(1, 0, 0, 0)), core, []));
        Check(emitted.Success, string.Join("; ", emitted.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "NativeTypeLibrary.dll"), libraryImage.ToArray());
        File.WriteAllText(Path.Combine(output, "NativeTypeLibrary.rvn"), source);
        var reference = NeoClrMetadataReference.ReadAssembly(libraryImage.ToArray());
        const string app = """
            import Example.*
            func Main() -> int {
                if !NativeMath.Echo(true) { return 1 }
                let calculator = NativeMath.Create(20)
                let alias = PassCalculator(NativeMath.Pass(calculator.Same(calculator)))
                alias.Visible = NativeMath.Echo(22)
                return alias.Add(Snapshot(calculator).Value)
            }
            """;
        foreach (var references in new MetadataReference[][] { [coreReference, reference], [reference, coreReference] })
        {
            var compilation = Compilation.Create("NativeTypeConsumer", [SyntaxTree.ParseText(app)], references, CompilationOptions.NeoCLR);
            Check(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", compilation.GetDiagnostics()));
            var assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
            var type = assembly.GetTypeByMetadataName("Example.NativeMath");
            Check(type is { IsStatic: true, IsAbstract: true, IsClosed: true, TypeKind: TypeKind.Class, Arity: 0 }, "native type classification");
            Check(ReferenceEquals(type, assembly.GlobalNamespace.LookupNamespace("Example")!.LookupType("NativeMath")), "type lookup identity");
            var tree = compilation.SyntaxTrees.Single();
            var model = compilation.GetSemanticModel(tree);
            var calls = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().ToArray();
            Check(calls.Length >= 3, "overload and instance calls");
            var methods = calls.Select(call => model.GetSymbolInfo(call).Symbol as IMethodSymbol).Where(m => m?.Name == "Echo").ToArray();
            Check(methods.All(m => m is not null && ReferenceEquals(m.ContainingType, type) && ReferenceEquals(m.ContainingAssembly, assembly)), "nominal method ownership");
            Check(methods.Select(m => m!.ReturnType.SpecialType).ToHashSet().SetEquals([SpecialType.System_Boolean, SpecialType.System_Int32]), "primitive overload selection");
            var calculator = assembly.GetTypeByMetadataName("Example.Calculator");
            Check(calculator is { IsStatic: false, IsAbstract: false, IsClosed: false } && calculator.InstanceConstructors.Length == 1, "instance class/constructor classification");
            var create = type!.GetMembers("Create").OfType<IMethodSymbol>().Single();
            var pass = type.GetMembers("Pass").OfType<IMethodSymbol>().Single();
            var same = calculator!.GetMembers("Same").OfType<IMethodSymbol>().Single();
            Check(ReferenceEquals(create.ReturnType, calculator) && ReferenceEquals(pass.Parameters[0].Type, calculator) &&
                ReferenceEquals(pass.ReturnType, calculator) && ReferenceEquals(same.ReturnType, calculator) &&
                ReferenceEquals(same.Parameters[0].Type, calculator), "canonical nominal parameter/result symbols");
            var snapshot = assembly.GetTypeByMetadataName("Example.Snapshot")!;
            Check(ReferenceEquals(snapshot.InstanceConstructors.Single().Parameters[0].Type, calculator), "nominal constructor parameter identity");
            var stored = calculator!.GetMembers("stored").OfType<IFieldSymbol>().Single();
            var visible = calculator.GetMembers("Visible").OfType<IFieldSymbol>().Single();
            Check(stored.DeclaredAccessibility == Accessibility.Private && stored.Type.SpecialType == SpecialType.System_Int32 &&
                visible.DeclaredAccessibility == Accessibility.Public && !visible.IsStatic && !visible.IsReadOnly && ReferenceEquals(visible.ContainingType, calculator), "native field identity/type/access");
            var add = calls.Select(call => model.GetSymbolInfo(call).Symbol as IMethodSymbol).Single(m => m?.Name == "Add");
            Check(!add!.IsStatic && ReferenceEquals(add.ContainingType, calculator), "instance method ownership");
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new("NativeTypeConsumer", new Version(1, 0, 0, 0)), core, [new(reference, reference.Definition, core)]));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "NativeTypeConsumer.dll"), image.ToArray());
        }
        foreach (var expression in new[] { "NativeMath.Hidden()", "NativeMath.Secret()", "HiddenType.Value()", "Calculator(20).Secret()", "Calculator(20).stored", "Restricted()", "NativeMath.Echo(\"wrong\")", "NativeMath.Pass(\"wrong\").Visible" })
        {
            var rejected = Compilation.Create("Rejected", [SyntaxTree.ParseText("import Example.*\nfunc Main() -> int { return " + expression + " }")],
                [coreReference, reference], CompilationOptions.NeoCLR);
            var diagnostics = rejected.GetDiagnostics();
            Check(expression.Contains("wrong")
                ? diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error)
                : diagnostics.Any(d => d.Id == "RAV0500"), "access/signature violation accepted: " + expression + ": " + string.Join("; ", diagnostics));
        }
        var fieldConsumer = Compilation.Create("NativeFieldConsumer",
            [SyntaxTree.ParseText("import Example.*\nfunc Main() -> int { return Calculator(42).Visible }")],
            [coreReference, reference], CompilationOptions.NeoCLR);
        Check(!fieldConsumer.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "public field semantic binding failed");
        using var fieldOutput = new MemoryStream();
        var emittedField = NeoClrCompilationEmitter.EmitMetadataAssembly(fieldConsumer, fieldOutput,
            new(new("NativeFieldConsumer", new Version(1, 0, 0, 0)), core, [new(reference, reference.Definition, core)]));
        Check(emittedField.Success && fieldOutput.Length > 0, "external field emission failed: " + string.Join("; ", emittedField.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "NativeFieldConsumer.dll"), fieldOutput.ToArray());
        Console.WriteLine("PASS native class identity, constructors, instance/static calls, access and emission");
    }
    private static void Check(bool value, string message) { if (!value) throw new Exception(message); }
}
