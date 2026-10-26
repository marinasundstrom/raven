using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class InterfaceMemberAccessibilityTests
{
    private static Compilation Compile(string source) => Compilation.Create(
        "InterfaceAccess" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)],
        TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

    [Theory]
    [InlineData("", Accessibility.Public, true)]
    [InlineData("public", Accessibility.Public, true)]
    [InlineData("sealed", Accessibility.Public, false)]
    [InlineData("static", Accessibility.Public, false)]
    [InlineData("private static", Accessibility.Private, false)]
    [InlineData("internal static", Accessibility.Internal, false)]
    [InlineData("protected static", Accessibility.ProtectedAndProtected, false)]
    [InlineData("protected internal static", Accessibility.ProtectedOrInternal, false)]
    [InlineData("private protected static", Accessibility.ProtectedAndInternal, false)]
    [InlineData("private", Accessibility.Private, false)]
    [InlineData("internal", Accessibility.Internal, true)]
    [InlineData("protected", Accessibility.ProtectedAndProtected, true)]
    [InlineData("protected internal", Accessibility.ProtectedOrInternal, true)]
    [InlineData("private protected", Accessibility.ProtectedAndInternal, true)]
    public void DefaultMembers_PreserveAccessibilityAndVirtualDispatch(string modifier, Accessibility access, bool virtualMember)
    {
        var csharpIndexer = modifier.Contains("static") ? "" : $"{modifier} int this[int index] {{ get => index; }}";
        var ravenIndexer = modifier.Contains("static") ? "" : $"{modifier} val self[index: int]: int {{ get => index }}";
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var csharp = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("CSharpInterfaceAccess",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($$"""
                public interface Contract {
                    {{modifier}} int Read() => 42;
                    {{modifier}} int Value { get => 42; }
                    {{csharpIndexer}}
                    {{modifier}} event System.Action Changed { add {} remove {} }
                }
                """)], paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(csharp.GetDiagnostics(), d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error);
        using var expectedImage = new MemoryStream();
        Assert.True(csharp.Emit(expectedImage).Success);
        var expectedType = Assembly.Load(expectedImage.ToArray()).GetType("Contract")!;

        var compilation = Compile($$"""
            public interface Contract {
                {{modifier}} func Read() -> int => 42
                {{modifier}} val Value: int { get => 42 }
                {{ravenIndexer}}
                {{modifier}} event Changed: System.Action { add {} remove {} }
            }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var type = compilation.GetTypeByMetadataName("Contract")!;
        var methods = type.GetMembers().OfType<IMethodSymbol>().ToArray();
        Assert.NotEmpty(methods);
        Assert.All(methods, m =>
        {
            Assert.Equal(access, m.DeclaredAccessibility);
            Assert.False(m.IsAbstract);
            Assert.Equal(virtualMember, m.IsVirtual);
        });
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var emitted = Assembly.Load(image.ToArray()).GetType("Contract")!;
        Assert.All(emitted.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance | BindingFlags.DeclaredOnly), m =>
        {
            Assert.Equal(virtualMember, m.IsVirtual);
            Assert.False(m.IsAbstract);
            var expected = expectedType.GetMethod(m.Name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance)!;
            Assert.Equal(expected.Attributes, m.Attributes);
        });
    }

    [Theory]
    [InlineData("private func Read() -> int")]
    [InlineData("private val Value: int { get; }")]
    [InlineData("private val Value: int")]
    [InlineData("private val self[index: int]: int { get; }")]
    [InlineData("private event Changed: System.Action")]
    [InlineData("private virtual func Read() -> int => 42")]
    public void InvalidPrivateRequirements_AreDiagnosed(string member)
    {
        var compilation = Compile($"interface Contract {{ {member} }}");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error && d.Id == "RAV0332");
    }

    [Theory]
    [InlineData("public", "private", false)]
    [InlineData("public", "internal", false)]
    [InlineData("public", "public", true)]
    [InlineData("internal", "public", false)]
    [InlineData("protected", "public", false)]
    [InlineData("protected internal", "public", false)]
    [InlineData("private protected", "public", false)]
    public void ImplicitImplementation_RequiresPublicMembers(string contractAccess, string implementationAccess, bool valid)
    {
        var compilation = Compile($$"""
            interface Contract { {{contractAccess}} func Read() -> int }
            class Provider : Contract { {{implementationAccess}} func Read() -> int => 42 }
            """);
        Assert.Equal(!valid, compilation.GetDiagnostics().Any(d => d.Id == "RAV0330"));
    }

    [Theory]
    [InlineData("internal")]
    [InlineData("protected")]
    [InlineData("protected internal")]
    [InlineData("private protected")]
    public void NonPublicRequirements_AllowExplicitImplementation(string modifier)
    {
        var compilation = Compile($$"""
            interface Contract { {{modifier}} func Read() -> int }
            class Provider : Contract { func Contract.Read() -> int => 42 }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        var provider = assembly.GetType("Provider")!;
        var map = provider.GetInterfaceMap(assembly.GetType("Contract")!);
        Assert.Equal(42, Assert.Single(map.TargetMethods).Invoke(Activator.CreateInstance(provider), null));
    }
    [Theory]
    [InlineData("val Value: int { get; }", "private val Value: int => 42")]
    [InlineData("internal val Value: int { get; }", "val Value: int => 42")]
    [InlineData("val self[index: int]: int { get; }", "private val self[index: int]: int => index")]
    [InlineData("event Changed: System.Action", "private event Changed: System.Action")]
    [InlineData("internal event Changed: System.Action", "event Changed: System.Action")]
    public void NonPublicPropertiesAndEvents_DoNotImplicitlyImplement(string requirement, string implementation)
    {
        var compilation = Compile($$"""
            interface Contract { {{requirement}} }
            class Provider : Contract { {{implementation}} }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0330");
    }

    [Fact]
    public void PrivateHelper_CannotBeExplicitlyImplemented()
    {
        var compilation = Compile("""
            interface Contract { private func Read() -> int => 42 }
            class Provider : Contract { func Contract.Read() -> int => 1 }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0315");
    }

    [Theory]
    [InlineData("private")]
    [InlineData("sealed")]
    public void NonVirtualHelper_IsCallableInsideDefaultImplementation(string modifier)
    {
        var compilation = Compile($$"""
            public interface Contract {
                {{modifier}} func Helper() -> int => 42
                func Read() -> int => Helper()
            }
            public class Provider : Contract { func Helper() -> int => 1 }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        var instance = Activator.CreateInstance(assembly.GetType("Provider")!);
        Assert.Equal(42, assembly.GetType("Contract")!.GetMethod("Read")!.Invoke(instance, null));
    }

    [Fact]
    public void ProtectedHelper_IsAccessibleFromDerivedInterface()
    {
        var compilation = Compile("""
            interface Base { protected func Helper() -> int => 42 }
            interface Derived : Base { func Read() -> int => self.Helper() }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void PrivateHelper_IsInaccessibleOutsideItsInterface()
    {
        var compilation = Compile("""
            interface Contract { private func Helper() -> int => 42 }
            func Read(value: Contract) -> int => value.Helper()
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("private protected", Accessibility.ProtectedAndInternal)]
    [InlineData("protected internal", Accessibility.ProtectedOrInternal)]
    public void AccessorAccessibility_PreservesCombinedModifiers(string modifier, Accessibility expected)
    {
        var compilation = Compile($$"""
            interface Contract {
                val Value: int { get => 42; {{modifier}} set {} }
                val self[index: int]: int { get => index; {{modifier}} set {} }
            }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var properties = compilation.GetTypeByMetadataName("Contract")!.GetMembers().OfType<IPropertySymbol>();
        Assert.All(properties, p => Assert.Equal(expected, p.SetMethod!.DeclaredAccessibility));
    }

}
