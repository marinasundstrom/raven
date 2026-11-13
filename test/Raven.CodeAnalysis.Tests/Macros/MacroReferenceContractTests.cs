using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;
using System.Threading;
using System.Threading.Tasks;

using Mono.Cecil;
using Mono.Cecil.Cil;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "DotNet")]
public sealed class MacroReferenceContractTests
{
    [Fact]
    public void DotNet_DirectDefinitionsRetainIdentityAndDisplay()
    {
        var macro = new Counting();
        var reference = new MacroReference(macro);
        Assert.Same(macro, Assert.Single(reference.Macros));
        Assert.Same(macro, Assert.Single(reference.Macros));
        Assert.Equal(typeof(Counting).Assembly.FullName, reference.Display);
        Assert.Null(reference.SourceProjectFilePath);
        Assert.NotEqual(reference, new MacroReference(macro));
    }

    [Fact]
    public void DotNet_ExportedTypeIsLazyAndPublishesOneSnapshotUnderConcurrency()
    {
        Counting.Calls = 0;
        var reference = new MacroReference(typeof(Counting));
        Assert.Equal(0, Counting.Calls);
        var instances = new IMacroDefinition[16];
        Parallel.For(0, instances.Length, i => instances[i] = Assert.Single(reference.Macros));
        Assert.Equal(1, Counting.Calls);
        Assert.All(instances, instance => Assert.Same(instances[0], instance));
        Assert.NotSame(instances[0], Assert.Single(new MacroReference(typeof(Counting)).Macros));
        Assert.Equal(2, Counting.Calls);
    }

    [Fact]
    public void DotNet_ImageIsCopiedAndLoadedIntoCollectibleContext()
    {
        var image = Image("ImageMacro");
        var reference = MacroReference.CreateFromImage(image, " ");
        Array.Clear(image);
        Assert.Equal("<in-memory macro assembly>", reference.Display);
        var macro = Assert.Single(reference.Macros);
        Assert.Equal("ImageMacro", macro.GetType().Name);
        Assert.Equal(string.Empty, macro.GetType().Assembly.Location);
        Assert.Equal(42, macro.GetType().GetMethod("Expand")!.Invoke(macro, null));
        Assert.Same(macro, Assert.Single(reference.Macros));
        Assert.True(AssemblyLoadContext.GetLoadContext(macro.GetType().Assembly)!.IsCollectible);
    }

    [Fact]
    public void DotNet_LoadedAssemblyReferencesOwnSeparateProviderSnapshots()
    {
        using var stream = new MemoryStream(Image("AssemblyMacro"));
        var context = new AssemblyLoadContext("MacroReferenceTest", isCollectible: true);
        try
        {
            var assembly = context.LoadFromStream(stream);
            var first = MacroReference.CreateFromAssembly(assembly, "label");
            var second = new MacroReference(assembly);
            Assert.Equal("label", first.Display);
            Assert.Equal(assembly.Location, second.Display);
            var a = Assert.Single(first.Macros);
            var b = Assert.Single(second.Macros);
            Assert.NotSame(a, b);
            Assert.Same(a.GetType(), b.GetType());
            Assert.Same(a, Assert.Single(first.Macros));
        }
        finally { context.Unload(); }
    }

    [Fact]
    public void DotNet_FileExportsShareLoadedTypesButNotProviderInstancesAndHashContent()
    {
        using var directory = new TemporaryDirectory();
        var path = Path.Combine(directory.Path, "Plugin.dll");
        File.WriteAllBytes(path, Image("Before"));
        var first = MacroReference.CreateFromFile(path, "./provider.rvnproj");
        var a = Assert.Single(first.Macros);
        var second = MacroReference.CreateFromFile(path);
        var b = Assert.Single(second.Macros);
        Assert.NotSame(a, b);
        Assert.Same(a.GetType(), b.GetType());
        Assert.Equal(Path.GetFullPath(path), first.Display);
        Assert.Equal(string.Empty, a.GetType().Assembly.Location);
        Assert.Equal(Path.GetFullPath("./provider.rvnproj"), first.SourceProjectFilePath);
        var replacement = Path.Combine(directory.Path, "replacement.dll");
        File.WriteAllBytes(replacement, Image("After"));
        File.Move(replacement, path, overwrite: true);
        var c = Assert.Single(MacroReference.CreateFromFile(path).Macros);
        Assert.Equal("After", c.GetType().Name);
        Assert.NotSame(a.GetType().Assembly, c.GetType().Assembly);
        Assert.Same(a, Assert.Single(first.Macros));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DotNet_FileLoadsAdjacentOrExplicitDependencies(bool explicitDependency)
    {
        using var directory = new TemporaryDirectory();
        var dependencyName = "MacroDependency_" + Guid.NewGuid().ToString("N");
        var dependencyDirectory = explicitDependency ? Directory.CreateDirectory(Path.Combine(directory.Path, "deps")).FullName : directory.Path;
        var dependencyPath = Path.Combine(dependencyDirectory, dependencyName + ".dll");
        File.WriteAllBytes(dependencyPath, Image("Helper", dependencyName: dependencyName, isDependency: true));
        var path = Path.Combine(directory.Path, "Plugin.dll");
        File.WriteAllBytes(path, Image("Dependent", dependencyName));
        var reference = explicitDependency
            ? MacroReference.CreateFromFile(path, null, ["", path, dependencyPath, dependencyPath])
            : MacroReference.CreateFromFile(path);
        var macro = Assert.Single(reference.Macros);
        Assert.Equal(42, macro.GetType().GetMethod("Expand")!.Invoke(macro, null));
        if (explicitDependency)
        {
            var replacement = dependencyPath + ".new";
            File.WriteAllBytes(replacement, Image("Helper", dependencyName, isDependency: true, value: 43));
            File.Move(replacement, dependencyPath, overwrite: true);
            var refreshed = Assert.Single(MacroReference.CreateFromFile(path, null, [dependencyPath]).Macros);
            Assert.Equal(43, refreshed.GetType().GetMethod("Expand")!.Invoke(refreshed, null));
            Assert.Equal(42, macro.GetType().GetMethod("Expand")!.Invoke(macro, null));
        }
    }

    [Fact]
    public void DotNet_InvalidInputsFailAtTheExistingEagerOrLazyBoundary()
    {
        Assert.Throws<ArgumentNullException>(() => MacroReference.CreateFromAssembly(null!));
        Assert.Throws<ArgumentNullException>(() => MacroReference.CreateFromImage(null!));
        Assert.Throws<ArgumentException>(() => MacroReference.CreateFromImage([]));
        Assert.Throws<ArgumentException>(() => MacroReference.CreateFromFile(" "));
        Assert.Throws<ArgumentException>(() => new MacroReference(typeof(string)));
        var malformed = MacroReference.CreateFromImage([1, 2, 3], "bad");
        Assert.Equal("bad", malformed.Display);
        var error = Assert.Throws<BadImageFormatException>(() => _ = malformed.Macros);
        Assert.Same(error, Assert.Throws<BadImageFormatException>(() => _ = malformed.Macros));
        var missing = MacroReference.CreateFromFile(Path.Combine(Path.GetTempPath(), Guid.NewGuid() + ".dll"));
        Assert.Throws<InvalidOperationException>(() => _ = missing.Macros);
    }

    [Fact]
    public void DotNet_ImageWithMissingDependencyReportsMissingAssembly()
    {
        var reference = MacroReference.CreateFromImage(Image("MissingDependency", "Missing_" + Guid.NewGuid().ToString("N")));
        var macro = Assert.Single(reference.Macros);
        var failure = Assert.Throws<TargetInvocationException>(() => macro.GetType().GetMethod("Expand")!.Invoke(macro, null));
        Assert.IsType<FileNotFoundException>(failure.GetBaseException());
    }

    private static byte[] Image(string typeName, string? dependencyName = null, bool isDependency = false, int value = 42)
    {
        var name = isDependency ? dependencyName! : "MacroImage_" + Guid.NewGuid().ToString("N");
        using var assembly = AssemblyDefinition.CreateAssembly(new AssemblyNameDefinition(name, new Version(1, 0)), name, ModuleKind.Dll);
        var module = assembly.MainModule;
        var type = new TypeDefinition("Fixtures", typeName, Mono.Cecil.TypeAttributes.Public | Mono.Cecil.TypeAttributes.Class, module.ImportReference(typeof(object)));
        module.Types.Add(type);
        if (!isDependency) type.Interfaces.Add(new InterfaceImplementation(module.ImportReference(typeof(IMacroDefinition))));
        var constructor = new MethodDefinition(".ctor", Mono.Cecil.MethodAttributes.Public | Mono.Cecil.MethodAttributes.SpecialName | Mono.Cecil.MethodAttributes.RTSpecialName, module.TypeSystem.Void);
        type.Methods.Add(constructor);
        var ctor = constructor.Body.GetILProcessor();
        ctor.Emit(OpCodes.Ldarg_0);
        ctor.Emit(OpCodes.Call, module.ImportReference(typeof(object).GetConstructor(Type.EmptyTypes)!));
        ctor.Emit(OpCodes.Ret);
        var expand = new MethodDefinition(isDependency ? "Value" : "Expand", Mono.Cecil.MethodAttributes.Public |
            (isDependency ? Mono.Cecil.MethodAttributes.Static : 0), module.TypeSystem.Int32);
        type.Methods.Add(expand);
        var body = expand.Body.GetILProcessor();
        if (dependencyName is not null && !isDependency)
        {
            var dependency = new AssemblyNameReference(dependencyName, new Version(1, 0));
            module.AssemblyReferences.Add(dependency);
            body.Emit(OpCodes.Call, new MethodReference("Value", module.TypeSystem.Int32,
                new TypeReference("Fixtures", "Helper", module, dependency))
            { HasThis = false });
        }
        else body.Emit(OpCodes.Ldc_I4, value);
        body.Emit(OpCodes.Ret);
        using var stream = new MemoryStream();
        assembly.Write(stream);
        return stream.ToArray();
    }

    private sealed class TemporaryDirectory : IDisposable
    {
        public string Path { get; } = Directory.CreateDirectory(System.IO.Path.Combine(System.IO.Path.GetTempPath(), "raven-macro-" + Guid.NewGuid())).FullName;
        public void Dispose() => Directory.Delete(Path, recursive: true);
    }

    public sealed class Counting : IMacroDefinition
    {
        public static int Calls;
        public Counting() => Interlocked.Increment(ref Calls);
        public int Expand() => 42;
    }
}
