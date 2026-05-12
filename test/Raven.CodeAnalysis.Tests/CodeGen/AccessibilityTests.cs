using System;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class AccessibilityTests
{
    [Fact]
    public void TypeAccessibility_IsEmittedCorrectly()
    {
        var code = """
class DefaultType { }
internal class InternalType { }
public class PublicType { }
""";

        using var metadataContext = Emit(code, out var assembly);

        var defaultType = assembly.GetType("DefaultType", throwOnError: true)!;
        var internalType = assembly.GetType("InternalType", throwOnError: true)!;
        var publicType = assembly.GetType("PublicType", throwOnError: true)!;

        Assert.True(defaultType.IsPublic);
        Assert.True(internalType.IsNotPublic);
        Assert.True(publicType.IsPublic);
    }

    [Fact]
    public void NestedTypeDefaultAccessibility_IsEmittedCorrectly()
    {
        var code = """
public class Container {
    class DefaultNested { }
    public class PublicNested { }
}

public interface IContainer {
    class InterfaceNested { }
}
""";

        using var metadataContext = Emit(code, out var assembly);

        var container = assembly.GetType("Container", throwOnError: true)!;
        var interfaceContainer = assembly.GetType("IContainer", throwOnError: true)!;
        var flags = BindingFlags.Public | BindingFlags.NonPublic;

        AssertNestedVisibility(container, "DefaultNested", TypeAttributes.NestedPrivate, flags);
        AssertNestedVisibility(container, "PublicNested", TypeAttributes.NestedPublic, flags);
        AssertNestedVisibility(interfaceContainer, "InterfaceNested", TypeAttributes.NestedPublic, flags);
    }

    [Fact]
    public void MethodAccessibility_IsEmittedCorrectly()
    {
        var code = """
public class MethodContainer {
    public func Foo() -> unit { return; }
    internal func Bar() -> unit { return; }
    private func Baz() -> unit { return; }
    protected func Quux() -> unit { return; }
    protected internal func Mix() -> unit { return; }
    private protected func Inter() -> unit { return; }
}
""";

        using var metadataContext = Emit(code, out var assembly);

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
    public void SemanticModel_ReportsDeclaredAccessibility()
    {
        var code = """
class DefaultType { }
internal class Sample {
    private init() { }
    protected internal init(value: int) { }
    public func Run() -> unit { return; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(syntaxTree);

        var model = compilation.GetSemanticModel(syntaxTree);
        var classDecls = syntaxTree.GetRoot().DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .ToDictionary(static declaration => declaration.Identifier.ValueText);
        var ctorDecls = syntaxTree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().ToArray();
        var methodDecl = syntaxTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        var defaultTypeSymbol = (INamedTypeSymbol)model.GetDeclaredSymbol(classDecls["DefaultType"])!;
        Assert.Equal(Accessibility.Public, defaultTypeSymbol.DeclaredAccessibility);

        var classSymbol = (INamedTypeSymbol)model.GetDeclaredSymbol(classDecls["Sample"])!;
        Assert.Equal(Accessibility.Internal, classSymbol.DeclaredAccessibility);

        var firstCtor = (IMethodSymbol)model.GetDeclaredSymbol(ctorDecls[0])!;
        var secondCtor = (IMethodSymbol)model.GetDeclaredSymbol(ctorDecls[1])!;
        Assert.Equal(Accessibility.Private, firstCtor.DeclaredAccessibility);
        Assert.Equal(Accessibility.ProtectedOrInternal, secondCtor.DeclaredAccessibility);

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodDecl)!;
        Assert.Equal(Accessibility.Public, methodSymbol.DeclaredAccessibility);
    }

    [Fact]
    public void SemanticModel_ReportsNestedTypeDefaultAccessibility()
    {
        var code = """
class Container {
    class DefaultNested { }
    public class PublicNested { }
}

interface IContainer {
    class InterfaceNested { }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(syntaxTree);
        var model = compilation.GetSemanticModel(syntaxTree);
        var declarations = syntaxTree.GetRoot().DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .ToDictionary(static declaration => declaration.Identifier.ValueText);

        var defaultNested = (INamedTypeSymbol)model.GetDeclaredSymbol(declarations["DefaultNested"])!;
        var publicNested = (INamedTypeSymbol)model.GetDeclaredSymbol(declarations["PublicNested"])!;
        var interfaceNested = (INamedTypeSymbol)model.GetDeclaredSymbol(declarations["InterfaceNested"])!;

        Assert.Equal(Accessibility.Private, defaultNested.DeclaredAccessibility);
        Assert.Equal(Accessibility.Public, publicNested.DeclaredAccessibility);
        Assert.Equal(Accessibility.Public, interfaceNested.DeclaredAccessibility);
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(MetadataReference.CreateFromFile(runtimePath));

        return compilation;
    }

    private static MetadataLoadContext Emit(string code, out Assembly assembly)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Seek(0, SeekOrigin.Begin);
        var resolver = new PathAssemblyResolver(compilation.References
            .OfType<PortableExecutableReference>()
            .Select(r => r.FilePath)
            .Where(p => p is not null)!);

        var metadataContext = new MetadataLoadContext(resolver);
        assembly = metadataContext.LoadFromStream(peStream);
        return metadataContext;
    }
}
