using NeoCLR.Metadata.Experimental.Model;
using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeSelfChecks
{
    const string Source = """
        public interface Cloneable { func Clone() -> Self; val Current: Self { get; } }
        public interface Comparable<T> { func Compare(other: T) -> int }
        public interface Number : Comparable<Self> {
            static val Zero: Self { get; }
            static func +(left: Self, right: Self) -> Self
            static func Add(left: Self, right: Self) -> Self
        }
        public struct Count : Number, Cloneable {
            public func Clone() -> Self => self
            public val Current: Self => self
            public var Value: int
            public func Compare(other: Self) -> int => Value - other.Value
            static val Zero: Self => Count()
            static func +(left: Self, right: Self) -> Self => Add(left, right)
            public static func Add(left: Self, right: Self) -> Self {
                var result = Count()
                result.Value = left.Value + right.Value
                return result
            }
        }
        public class Consumer {
            public static func Copy<T>(value: T) -> T where T: Cloneable => value.Clone()
            public static func Sum<T>(left: T, right: T) -> T where T: Number => T.Add(left, right) + T.Zero
        }
        """;

    const string InheritanceSource = """
        public interface Cloneable { func Clone() -> Self }
        public open class Base : Cloneable {
            public virtual func Clone() -> Self => self
        }
        public class Derived : Base {}
        public class Consumer {
            public static func Copy<T>(value: T) -> T where T: Cloneable => value.Clone()
            public static func Check(value: Derived) -> Base => Copy<Base>(value)
        }
        """;

    internal static void Run(MetadataReference[] references, CompilationOptions options,
        Func<string, NeoClrEmissionBackend> backend, string directory, AssemblyDefinition core)
    {
        for (var variant = 0; variant < 4; variant++)
        {
            var source = variant switch
            {
                1 => InheritanceSource.Replace("class Derived : Base {}", "class Derived : Base { public override func Clone() -> Base => self }"),
                2 => InheritanceSource.Replace("class Derived : Base {}", "class Derived : Base, Cloneable { func Cloneable.Clone() -> Self => self }")
                    .Replace("Check(value: Derived) -> Base => Copy<Base>(value)", "Check(value: Derived) -> Derived => Copy<Derived>(value)"),
                3 => Source,
                _ => InheritanceSource
            };
            var dispatchSource = source;
            // Keep inherited-Self signature coverage independent of the remaining
            // conformance-owner limitation. Nominal instance dispatch is supported.
            source = source.Replace("=> value.Clone()", "=> value")
                .Replace("=> T.Add(left, right) + T.Zero", "=> left");
            var name = "NativeSelf" + variant;
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(source)], references,
                options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            if (variant == 0)
            {
                var dispatch = Compilation.Create(name + "Dispatch", [SyntaxTree.ParseText(dispatchSource)], references,
                    options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
                using var unsupportedImage = new MemoryStream();
                var unsupported = dispatch.Emit(unsupportedImage, null,
                    new EmitOptions().WithBackend(backend(name + "Dispatch")));
                if (unsupported.Success || !unsupported.Diagnostics.Any(diagnostic => diagnostic.Id == "NEOMETA003" &&
                    diagnostic.GetMessage().Contains("missing public interface implementation: .Derived.Clone", StringComparison.Ordinal)))
                    throw new Exception("Inherited Self conformance-owner limitation changed.");
            }
            using var image = new MemoryStream();
            var result = compilation.Emit(image, null, new EmitOptions().WithBackend(backend(name)));
            // The metadata writer currently rebinds inherited Self to Derived.
            // Keep this unsupported shape explicit until conformance-owner tracking is fixed.
            if (variant < 2)
            {
                if (result.Success || !result.Diagnostics.Any(diagnostic => diagnostic.Id == "NEOMETA003" &&
                    diagnostic.GetMessage().Contains("missing public interface implementation: .Derived.Clone", StringComparison.Ordinal)))
                    throw new Exception("Inherited Self limitation changed: " + string.Join("\n", result.Diagnostics));
                continue;
            }
            if (!result.Success) throw new Exception(name + ": " + string.Join("\n", result.Diagnostics));
            var snapshot = AssemblyDefinition.ReadNativeAssembly(image.ToArray());
            var types = new MetadataLoadContext([core, snapshot]).Resolve(snapshot.Identity).GetTypes();
            var cloneable = types.Single(type => type.Name == "Cloneable");
            if (cloneable.GetMethods().Single(method => method.Name == "Clone").ReturnType is not SelfTypeInfo)
                throw new Exception("Native interface Self signature was erased.");
            if (variant < 3)
            {
                var baseType = types.Single(type => type.Name == "Base");
                if (baseType.GetMethods().Single(method => method.Name == "Clone").ReturnType.DisplayName != "Base")
                    throw new Exception("Inherited Self must retain the declaring Base result.");
                if (variant > 0)
                {
                    var method = types.Single(type => type.Name == "Derived").GetMethods()
                        .Single(method => method.Name.EndsWith("Clone", StringComparison.Ordinal));
                    if (method.ReturnType.DisplayName != (variant == 2 ? "Derived" : "Base"))
                        throw new Exception("Override or explicit reimplementation lost its Self result.");
                }
            }
            else
            {
                var number = types.Single(type => type.Name == "Number");
                if (number.GetGenericArguments().Count != 0 ||
                    number.GetMethods().Single(method => method.Name == "Add").ReturnType is not SelfTypeInfo)
                    throw new Exception("Native Self changed interface arity or lost its native signature.");
                var count = types.Single(type => type.Name == "Count");
                if (count.GetMethods().Single(method => method.Name == "Add").ReturnType.DisplayName != "Count")
                    throw new Exception("Concrete Self implementation result was erased.");
                var consumer = types.Single(type => type.Name == "Consumer");
                foreach (var method in consumer.GetMethods().Where(method => method.Name is "Copy" or "Sum"))
                    if (method.GetGenericArguments().Count != 1 || method.ReturnType is not MethodGenericParameterTypeInfo)
                        throw new Exception("Generic Self caller result was erased.");
            }
            File.WriteAllBytes(Path.Combine(directory, name + ".dll"), image.ToArray());

        }
    }
}
