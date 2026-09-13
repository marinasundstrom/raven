using System.Reflection;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetMetadataEmissionTests
{
    [Theory]
    [InlineData(false, ".Ok(let number)", ".Error(_)")]
    [InlineData(true, ".Ok(let number)", ".Error(_)")]
    [InlineData(false, "Ok(let number)", "Error(_)")]
    [InlineData(true, "Ok(let number)", "Error(_)")]
    [InlineData(true, "Choice.Ok<int>(let number)", "Choice.Error<string>(_)")]
    public void ImportedMemberUnionDestructuresThroughItsContract(bool targetMetadata, string success, string failure)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-member-pattern", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "MemberContracts.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("MemberContracts",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                namespace Contracts {
                    public static class Buffers { public static T[] Echo<T>(T[] value) => value; }
                    public static class Choice {
                        public struct Ok<T> {
                            public T Value;
                            public Ok(T value) { Value = value; }
                            public void Deconstruct(out T value) { value = Value; Value = default; }
                        }
                        public struct Error<E> {
                            public E Value;
                            public Error(E value) { Value = value; }
                            public void Deconstruct(out E value) { value = Value; }
                        }
                    }
                    [System.Runtime.CompilerServices.Union]
                    public struct Choice<T,E> {
                        private object value;
                        public object Value => value;
                        public Choice(Choice.Ok<T> value) { this.value = value; }
                        public Choice(Choice.Error<E> value) { this.value = value; }
                        public bool TryGetValue(out Choice.Ok<T> result) {
                            if (value is Choice.Ok<T> found) { result = found; return true; }
                            result = default; return false;
                        }
                        public bool TryGetValue(out Choice.Error<E> result) {
                            if (value is Choice.Error<E> found) { result = found; return true; }
                            result = default; return false;
                        }
                    }
                }
                """)], paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream);
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var references = paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray();
            var compilation = Compilation.Create("MemberConsumer", [SyntaxTree.ParseText($$"""
                import Contracts.*
                import Contracts.Choice.*
                public class Consumer {
                    public static func Pick(value: Choice<int, string>) -> int {
                        return match value {
                            {{success}} => number
                            {{failure}} => -1
                        }
                    }
                    public static func Run(value: int) -> int {
                        let numbers: int[] = [value]
                        let copied = Buffers.Echo<int>(numbers)
                        if copied[0] != value { return -100 }
                        if value < 0 { return Pick(Choice<int, string>(Choice.Error<string>("failed"))) }
                        let choice = Choice<int, string>(Choice.Ok<int>(value))
                        return Pick(choice) + Pick(choice)
                    }
                }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                    metadataImportOptions: new MetadataImportOptions("System.Runtime")));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = targetMetadata
                ? compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))))
                : compilation.Emit(output);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(output, references);
            var run = loaded.Assembly.GetType("Consumer")!.GetMethod("Run")!;
            Assert.Equal(84, run.Invoke(null, [42]));
            Assert.Equal(-1, run.Invoke(null, [-1]));
        }
        finally { Directory.Delete(directory, recursive: true); }
    }

    [Fact]
    public void ImportedUnionCasePatternKeepsClosedCarrierLocals()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-pattern", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var libraryPath = Path.Combine(directory, "UnionContract.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        try
        {
            var library = Compilation.Create("UnionContract", [SyntaxTree.ParseText("""
                public union Outcome<T, E> {
                    case Ok(value: T)
                    case Error(error: E)
                }
                """)], paths.Select(MetadataReference.CreateFromFile).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(libraryPath))
            {
                var emitted = library.Emit(stream);
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var compilation = Compilation.Create("Consumer", [SyntaxTree.ParseText("""
                public class Consumer {
                    public static func Pick(value: Outcome<int, string>) -> int {
                        return match value {
                            .Ok(let number) => number
                            .Error(_) => -1
                        }
                    }
                }
                """)], paths.Append(libraryPath).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, metadataImportOptions: new MetadataImportOptions("System.Runtime")));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using var assembly = AssemblyDefinition.ReadAssembly(output);
            var locals = assembly.MainModule.GetType("Consumer").Methods.Single(m => m.Name == "Pick").Body.Variables;
            Assert.Contains(locals, v => v.VariableType.FullName == "Outcome`2<System.Int32,System.String>");
            Assert.DoesNotContain(locals, v => v.VariableType.FullName.Contains("RavenMetadata"));
        }
        finally { Directory.Delete(directory, recursive: true); }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AbstractAndVirtualInterfaceImplementationsPreserveDispatchFlags(bool targetMetadata)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var compilation = Compilation.Create("AbstractConsumer", [SyntaxTree.ParseText("""
            interface Reader { func Read() -> int }
            abstract class Base : Reader { abstract func Read() -> int }
            class Derived : Base { override func Read() -> int { return 42 } }
            open class VirtualReader : Reader { virtual func Read() -> int { return 7 } }
            """)], paths.Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, metadataImportOptions: new MetadataImportOptions("System.Runtime")));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = targetMetadata
            ? compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))))
            : compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        output.Position = 0;
        using var assembly = AssemblyDefinition.ReadAssembly(output);
        var abstractMethod = assembly.MainModule.GetType("Base").Methods.Single(m => m.Name == "Read");
        Assert.True(abstractMethod.IsAbstract);
        Assert.True(abstractMethod.IsVirtual);
        Assert.False(abstractMethod.IsFinal);
        Assert.False(abstractMethod.HasBody);
        var virtualMethod = assembly.MainModule.GetType("VirtualReader").Methods.Single(m => m.Name == "Read");
        Assert.True(virtualMethod.IsVirtual);
        Assert.True(virtualMethod.IsNewSlot);
        Assert.False(virtualMethod.IsFinal);
        var implementation = assembly.MainModule.GetType("Derived").Methods.Single(m => m.Name == "Read");
        Assert.True(implementation.IsVirtual);
        Assert.False(implementation.IsNewSlot);
    }

    [Theory]
    [InlineData("class")]
    [InlineData("struct")]
    public void RetargetedGenericUsesApplicationTypeWithoutSelfAssemblyReference(string category)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-application", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "TargetContainer.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetContainer",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public class Box<T> { public T Value; public Box(T value) { Value = value; } public T Get() => Value; } }")],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var compilation = Compilation.Create("ApplicationConsumer", [SyntaxTree.ParseText($$"""
                import Contracts.*
                {{category}} Payload { var Number: int }
                func Create(value: Payload) -> Payload {
                    let box = Box<Payload>(value)
                    box.Value = value
                    return box.Get()
                }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, metadataImportOptions: new MetadataImportOptions("System.Runtime")));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using var assembly = AssemblyDefinition.ReadAssembly(output);
            Assert.DoesNotContain(assembly.MainModule.AssemblyReferences, a => a.Name == "ApplicationConsumer");
            var members = assembly.MainModule.GetMemberReferences().Where(m => m.DeclaringType is GenericInstanceType).ToArray();
            Assert.NotEmpty(members);
            foreach (var member in members)
            {
                var owner = Assert.IsType<GenericInstanceType>(member.DeclaringType);
                Assert.Equal("Contracts.Box`1<Payload>", owner.FullName);
                Assert.Equal("TargetContainer", owner.Scope.Name);
                Assert.Same(assembly.MainModule, owner.GenericArguments[0].Scope);
                Assert.Equal(category == "struct", owner.GenericArguments[0].IsValueType);
            }
        }
        finally { Directory.Delete(directory, recursive: true); }
    }

    [Fact]
    public void RetargetedGenericFieldAccessPreservesMetadataOwner()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-field", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "TargetFields.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetFields",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public struct Box<T> { public T Value; } }")],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var compilation = Compilation.Create("FieldConsumer", [SyntaxTree.ParseText("""
                import Contracts.*
                func Read(box: Box<int>) -> int { return box.Value }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, metadataImportOptions: new MetadataImportOptions("System.Runtime")));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using var assembly = AssemblyDefinition.ReadAssembly(output);
            var field = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<FieldReference>());
            Assert.Equal("Contracts.Box`1<System.Int32>", field.DeclaringType.FullName);
            Assert.Equal("TargetFields", field.DeclaringType.Scope.Name);
            Assert.Equal(0, Assert.IsType<GenericParameter>(field.FieldType).Position);
        }
        finally { Directory.Delete(directory, recursive: true); }
    }

    [Fact]
    public void RetargetedEmptyArrayDoesNotRequireHostArrayFactory()
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var compilation = Compilation.Create("EmptyArrayConsumer", [SyntaxTree.ParseText("""
            func Empty() -> int[] { return [] }
            """)], paths.Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                metadataImportOptions: new MetadataImportOptions("System.Runtime")));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output, null,
            new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        output.Position = 0;
        using var assembly = AssemblyDefinition.ReadAssembly(output);
        Assert.DoesNotContain(assembly.MainModule.GetMemberReferences(),
            member => member.DeclaringType.FullName == "System.Array" && member.Name == "Empty");
        var method = assembly.MainModule.Types.SelectMany(t => t.Methods).Single(m => m.Name == "Empty");
        Assert.Equal("System.Int32", Assert.IsType<ArrayType>(method.ReturnType).ElementType.FullName);
    }

    [Fact]
    public void RetargetedEmissionPreservesClosedGenericMethodSpecification()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-delegate", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "TargetCallbacks.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetCallbacks",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public static class Ops { public static T Echo<T>(T value) => value; } }")],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var compilation = Compilation.Create("CallbackConsumer", [SyntaxTree.ParseText("""
                import Contracts.*
                func Echo(value: int) -> int { return Ops.Echo<int>(value) }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, metadataImportOptions: new MetadataImportOptions("System.Runtime")));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using var assembly = AssemblyDefinition.ReadAssembly(output);
            var echo = assembly.MainModule.Types.SelectMany(t => t.Methods).Single(m => m.Name == "Echo");
            var call = Assert.Single(echo.Body.Instructions.Select(i => i.Operand).OfType<GenericInstanceMethod>());
            Assert.Equal("Contracts.Ops", call.DeclaringType.FullName);
            Assert.Equal("TargetCallbacks", call.DeclaringType.Scope.Name);
            Assert.Equal("System.Int32", Assert.Single(call.GenericArguments).FullName);
            var parameter = Assert.IsType<GenericParameter>(Assert.Single(call.Parameters).ParameterType);
            Assert.Equal(GenericParameterType.Method, parameter.Type);
            Assert.Equal(0, parameter.Position);
            Assert.Equal(GenericParameterType.Method, Assert.IsType<GenericParameter>(call.ReturnType).Type);
        }
        finally { Directory.Delete(directory, recursive: true); }
    }

    [Fact]
    public void RetargetedEmissionConstructsReferenceOnlyGenericDelegate()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-delegate", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "TargetCallbacks.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetCallbacks",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public delegate T Callback<T>(); }")],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var compilation = Compilation.Create("CallbackConsumer", [SyntaxTree.ParseText("""
                import Contracts.*
                func Number() -> int { return 42 }
                func Make() -> Callback<int> { return Number }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, metadataImportOptions: new MetadataImportOptions("System.Runtime")));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using var assembly = AssemblyDefinition.ReadAssembly(output);
            var make = assembly.MainModule.Types.SelectMany(t => t.Methods).Single(m => m.Name == "Make");
            Assert.Equal("Contracts.Callback`1<System.Int32>", make.ReturnType.FullName);
            Assert.Equal("TargetCallbacks", make.ReturnType.Scope.Name);
            var constructor = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>()
                .Where(m => m.Name == ".ctor" && m.DeclaringType.FullName == make.ReturnType.FullName));
            Assert.Equal("TargetCallbacks", constructor.DeclaringType.Scope.Name);
            Assert.Equal(new[] { "System.Object", "System.IntPtr" }, constructor.Parameters.Select(p => p.ParameterType.FullName));
        }
        finally { Directory.Delete(directory, recursive: true); }
    }

    [Fact]
    public void RetargetedEmissionPreservesReferenceOnlyGenericTypesAndMembers()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-metadata", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "TargetContracts.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetContracts",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                [assembly: System.Runtime.CompilerServices.ReferenceAssembly]
                namespace System.Runtime.CompilerServices { public sealed class UnionAttribute : System.Attribute { } }
                namespace Contracts {
                    public class Value { }
                    public static class Cases {
                        public struct Item<T> { public Item(T value) { Value = value; } public T Value { get; } }
                    }
                    [System.Runtime.CompilerServices.Union]
                    public struct Container<T> {
                        public Container(Cases.Item<T> item) { }
                        public object Value => default;
                        public bool TryGetValue(out Cases.Item<T> item) { item = default; return false; }
                    }
                }
                """)],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using (var stream = File.Create(path))
        {
            var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        }

        var source = """
            import Contracts.*
            func Echo(value: Container<int>) -> Container<int> {
                let copy = value
                return copy
            }
            func Wrap(value: Cases.Item<int>) -> Container<int> { return Container<int>(value) }
            func Make(value: int) -> Cases.Item<int> { return Cases.Item<int>(value) }
            func Read(value: Cases.Item<int>) -> int { return value.Value }
            func ReadReturned() -> int { return Make(42).Value }
            func ReadContainer(value: Container<int>) -> int {
                if value is Cases.Item<int> item { return item.Value }
                return 0
            }
            """;
        var compilation = Compilation.Create("TargetConsumer", [SyntaxTree.ParseText(source)],
            paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                metadataImportOptions: new MetadataImportOptions("System.Runtime")));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        output.Position = 0;
        using var assembly = AssemblyDefinition.ReadAssembly(output);
        Assert.DoesNotContain(assembly.MainModule.Types, t => t.Name.StartsWith("<RavenMetadata"));
        var methods = assembly.MainModule.Types.SelectMany(t => t.Methods).ToArray();
        var echo = methods.Single(m => m.Name == "Echo");
        var carrier = Assert.IsType<GenericInstanceType>(echo.ReturnType);
        Assert.Equal("Contracts.Container`1", carrier.ElementType.FullName);
        Assert.Equal("TargetContracts", carrier.Scope.Name);
        Assert.Equal("System.Int32", Assert.Single(carrier.GenericArguments).FullName);
        Assert.Equal(carrier.FullName, Assert.Single(echo.Parameters).ParameterType.FullName);
        Assert.Contains(echo.Body.Variables, v => v.VariableType.FullName == carrier.FullName);
        var constructor = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>()
            .Where(m => m.Name == ".ctor" && m.DeclaringType.FullName == "Contracts.Cases/Item`1<System.Int32>"));
        Assert.True(constructor.DeclaringType.IsValueType);
        Assert.Equal(0, Assert.IsType<GenericParameter>(Assert.Single(constructor.Parameters).ParameterType).Position);
        var wrapper = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>()
            .Where(m => m.Name == ".ctor" && m.DeclaringType.FullName == "Contracts.Container`1<System.Int32>"));
        var wrappedCase = Assert.IsType<GenericInstanceType>(Assert.Single(wrapper.Parameters).ParameterType);
        Assert.Equal("Contracts.Cases/Item`1", wrappedCase.ElementType.FullName);
        Assert.Equal(0, Assert.IsType<GenericParameter>(Assert.Single(wrappedCase.GenericArguments)).Position);
        var getter = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>().Where(m => m.Name == "get_Value"));
        Assert.Equal("Contracts.Cases/Item`1<System.Int32>", getter.DeclaringType.FullName);
        Assert.True(getter.DeclaringType.IsValueType);
        Assert.Equal("TargetContracts", getter.DeclaringType.Scope.Name);
        Assert.Equal(0, Assert.IsType<GenericParameter>(getter.ReturnType).Position);
        var extractor = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>().Where(m => m.Name == "TryGetValue"));
        var outputCase = Assert.IsType<GenericInstanceType>(Assert.IsType<ByReferenceType>(Assert.Single(extractor.Parameters).ParameterType).ElementType);
        Assert.Equal("Contracts.Cases/Item`1", outputCase.ElementType.FullName);
        Assert.True(outputCase.IsValueType);
        Assert.Equal(0, Assert.IsType<GenericParameter>(Assert.Single(outputCase.GenericArguments)).Position);
    }
}
