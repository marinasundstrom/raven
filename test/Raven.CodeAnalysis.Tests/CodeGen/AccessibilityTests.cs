using System;
using System.Reflection;
using Raven.CodeAnalysis.Tests.Utilities;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class AccessibilityTests
{
    [Fact]
    public void TypeAccessibility_IsEmittedCorrectly()
    {
        var code = """
internal class InternalType { }
public class PublicType { }
""";

        using var loadContext = Emit(code, out var assembly);

        var internalType = assembly.GetType("InternalType", throwOnError: true)!;
        var publicType = assembly.GetType("PublicType", throwOnError: true)!;

        Assert.True(internalType.IsNotPublic);
        Assert.True(publicType.IsPublic);
    }

    [Fact]
    public void NestedTypeAccessibility_IsEmittedCorrectly()
    {
        var code = """
public class Container {
    public class PublicNested { }
    internal class InternalNested { }
    private class PrivateNested { }
    protected class ProtectedNested { }
    protected internal class ProtectedInternalNested { }
    private protected class PrivateProtectedNested { }
}
""";

        using var loadContext = Emit(code, out var assembly);

        var container = assembly.GetType("Container", throwOnError: true)!;
        var bindingFlags = BindingFlags.NonPublic | BindingFlags.Public;

        AssertNestedVisibility(container, "PublicNested", TypeAttributes.NestedPublic, bindingFlags);
        AssertNestedVisibility(container, "InternalNested", TypeAttributes.NestedAssembly, bindingFlags);
        AssertNestedVisibility(container, "PrivateNested", TypeAttributes.NestedPrivate, bindingFlags);
        AssertNestedVisibility(container, "ProtectedNested", TypeAttributes.NestedFamily, bindingFlags);
        AssertNestedVisibility(container, "ProtectedInternalNested", TypeAttributes.NestedFamORAssem, bindingFlags);
        AssertNestedVisibility(container, "PrivateProtectedNested", TypeAttributes.NestedFamANDAssem, bindingFlags);
    }

    [Fact]
    public void MethodAccessibility_IsEmittedCorrectly()
    {
        var code = """
public class MethodContainer {
    public Foo() -> unit { return; }
    internal Bar() -> unit { return; }
    private Baz() -> unit { return; }
    protected Quux() -> unit { return; }
    protected internal Mix() -> unit { return; }
    private protected Inter() -> unit { return; }
}
""";

        using var loadContext = Emit(code, out var assembly);

        var container = assembly.GetType("MethodContainer", throwOnError: true)!;
        var flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly;

        AssertMethodAccessibility(container, "Foo", MethodAttributes.Public, flags);
        AssertMethodAccessibility(container, "Bar", MethodAttributes.Assembly, flags);
        AssertMethodAccessibility(container, "Baz", MethodAttributes.Private, flags);
        AssertMethodAccessibility(container, "Quux", MethodAttributes.Family, flags);
        AssertMethodAccessibility(container, "Mix", MethodAttributes.FamORAssem, flags);
        AssertMethodAccessibility(container, "Inter", MethodAttributes.FamANDAssem, flags);
    }

    [Fact]
    public void ConstructorAccessibility_IsEmittedCorrectly()
    {
        var code = """
public class CtorContainer {
    public init() { }
    internal init(value: int) { }
    private init(flag: bool) { }
    protected init(name: string) { }
    protected internal init(count: double) { }
    private protected init(bytes: byte) { }
    static init() { }
}
""";

        using var loadContext = Emit(code, out var assembly);

        var container = assembly.GetType("CtorContainer", throwOnError: true)!;
        var flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;

        AssertConstructorAccessibility(container, Array.Empty<Type>(), MethodAttributes.Public, flags);
        AssertConstructorAccessibility(container, new[] { typeof(int) }, MethodAttributes.Assembly, flags);
        AssertConstructorAccessibility(container, new[] { typeof(bool) }, MethodAttributes.Private, flags);
        AssertConstructorAccessibility(container, new[] { typeof(string) }, MethodAttributes.Family, flags);
        AssertConstructorAccessibility(container, new[] { typeof(double) }, MethodAttributes.FamORAssem, flags);
        AssertConstructorAccessibility(container, new[] { typeof(byte) }, MethodAttributes.FamANDAssem, flags);

        var typeInitializer = container.TypeInitializer;
        Assert.NotNull(typeInitializer);
        Assert.Equal(MethodAttributes.Private, typeInitializer!.Attributes & MethodAttributes.MemberAccessMask);
    }

    [Fact]
    public void NamedConstructor_SynthesizesPrivateDefaultConstructor()
    {
        var code = """
public class Person {
    var name: string;

    public init WithName(name: string) {
        self.name = name;
    }
}
""";

        using var loadContext = Emit(code, out var assembly);

        var person = assembly.GetType("Person", throwOnError: true)!;

        var publicCtor = person.GetConstructor(BindingFlags.Instance | BindingFlags.Public, binder: null, types: Type.EmptyTypes, modifiers: null);
        Assert.Null(publicCtor);

        var nonPublicCtor = person.GetConstructor(BindingFlags.Instance | BindingFlags.NonPublic, binder: null, types: Type.EmptyTypes, modifiers: null);
        Assert.NotNull(nonPublicCtor);
        Assert.Equal(MethodAttributes.Private, nonPublicCtor!.Attributes & MethodAttributes.MemberAccessMask);
    }

    [Fact]
    public void SemanticModel_ReportsDeclaredAccessibility()
    {
        var code = """
internal class Sample {
    private init() { }
    protected internal init(value: int) { }
    public Run() -> unit { return; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(syntaxTree);

        var model = compilation.GetSemanticModel(syntaxTree);
        var classDecl = syntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var ctorDecls = syntaxTree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().ToArray();
        var methodDecl = syntaxTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        var classSymbol = (INamedTypeSymbol)model.GetDeclaredSymbol(classDecl)!;
        Assert.Equal(Accessibility.Internal, classSymbol.DeclaredAccessibility);

        var firstCtor = (IMethodSymbol)model.GetDeclaredSymbol(ctorDecls[0])!;
        var secondCtor = (IMethodSymbol)model.GetDeclaredSymbol(ctorDecls[1])!;
        Assert.Equal(Accessibility.Private, firstCtor.DeclaredAccessibility);
        Assert.Equal(Accessibility.ProtectedOrInternal, secondCtor.DeclaredAccessibility);

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodDecl)!;
        Assert.Equal(Accessibility.Public, methodSymbol.DeclaredAccessibility);
    }

    private static void AssertNestedVisibility(Type container, string nestedName, TypeAttributes expected, BindingFlags flags)
    {
        var nestedType = container.GetNestedType(nestedName, flags);
        Assert.NotNull(nestedType);
        Assert.Equal(expected, nestedType!.Attributes & TypeAttributes.VisibilityMask);
    }

    private static void AssertMethodAccessibility(Type container, string methodName, MethodAttributes expected, BindingFlags flags)
    {
        var method = container.GetMethod(methodName, flags);
        Assert.NotNull(method);
        Assert.Equal(expected, method!.Attributes & MethodAttributes.MemberAccessMask);
    }

    private static void AssertConstructorAccessibility(Type container, Type[] parameterTypes, MethodAttributes expected, BindingFlags flags)
    {
        var ctor = container.GetConstructor(flags, binder: null, parameterTypes, null);
        Assert.NotNull(ctor);
        Assert.Equal(expected, ctor!.Attributes & MethodAttributes.MemberAccessMask);
    }

    private static Compilation CreateCompilation(SyntaxTree syntaxTree)
    {
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(MetadataReference.CreateFromFile(runtimePath));

        return compilation;
    }

    private static ResolvingAssemblyLoadContext Emit(string code, out Assembly assembly)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Seek(0, SeekOrigin.Begin);
        var referencePaths = compilation.References
            .OfType<PortableExecutableReference>()
            .Select(r => r.FilePath!)
            .ToArray();

        var loadContext = new ResolvingAssemblyLoadContext(referencePaths);
        assembly = loadContext.LoadFromStream(peStream);
        return loadContext;
    }
}
