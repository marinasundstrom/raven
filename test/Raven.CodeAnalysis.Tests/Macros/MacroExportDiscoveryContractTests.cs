using System;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "DotNet")]
public sealed class MacroExportDiscoveryContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DotNet_FallbackMarkerRestrictsDiscoveryToPublicTypes(bool marked)
    {
        using var assembly = NewAssembly();
        var visible = DefineMacro(assembly, "Visible", true);
        var hidden = DefineMacro(assembly, "Hidden", false);
        DefineMacro(assembly, "Invalid", true, macro: false);
        if (marked) Mark(assembly);
        Assert.Equal(marked ? new[] { visible } : new[] { visible, hidden },
            MacroExportDiscovery.GetExportedTypes(visible.Assembly));
    }

    [Fact]
    public void DotNet_ExplicitMarkersRetainOrderDeduplicateAndPermitHiddenTypes()
    {
        using var assembly = NewAssembly();
        var first = DefineMacro(assembly, "First", true);
        var second = DefineMacro(assembly, "Second", false);
        Mark(assembly, second);
        Mark(assembly, first);
        Mark(assembly, second);
        Assert.Equal(new[] { second, first }, MacroExportDiscovery.GetExportedTypes(first.Assembly));
    }

    [Fact]
    public void DotNet_MixedMarkersAreRejectedBeforeDiscovery()
    {
        using var assembly = NewAssembly();
        var type = DefineMacro(assembly, "Macro", true);
        Mark(assembly);
        Mark(assembly, type);
        Assert.Contains("mixes explicit entry points", Assert.Throws<InvalidOperationException>(() =>
            MacroExportDiscovery.GetExportedTypes(type.Assembly)).Message);
    }

    [Fact]
    public void DotNet_ExplicitExportsMustBelongToAssemblyAndHaveSupportedContract()
    {
        using var foreignAssembly = NewAssembly();
        var local = DefineMacro(foreignAssembly, "Local", true);
        Mark(foreignAssembly, typeof(Good));
        Assert.Contains("must be declared in assembly", Assert.Throws<InvalidOperationException>(() =>
            MacroExportDiscovery.GetExportedTypes(local.Assembly)).Message);
        using var invalidAssembly = NewAssembly();
        var invalid = DefineMacro(invalidAssembly, "Invalid", true, macro: false);
        Mark(invalidAssembly, invalid);
        Assert.Contains("one supported Expand contract", Assert.Throws<InvalidOperationException>(() =>
            MacroExportDiscovery.GetExportedTypes(invalid.Assembly)).Message);
    }

    [Theory]
    [InlineData(typeof(Good), true)]
    [InlineData(typeof(PrivateConstructor), true)]
    [InlineData(typeof(Abstract), false)]
    [InlineData(typeof(Generic<>), false)]
    [InlineData(typeof(Generic<int>), true)]
    [InlineData(typeof(NoDefaultConstructor), false)]
    [InlineData(typeof(Unclassified), false)]
    [InlineData(typeof(Ambiguous), false)]
    [InlineData(typeof(int), false)]
    public void DotNet_ConstructibilityPreservesExistingProviderRules(Type type, bool expected)
        => Assert.Equal(expected, MacroExportDiscovery.IsConstructibleExportedType(type));

    [Fact]
    public void DotNet_CreationProducesFreshInstancesIncludingNonpublicConstructors()
    {
        Assert.IsType<Good>(MacroExportDiscovery.CreateMacroInstance(typeof(Good)));
        var first = MacroExportDiscovery.CreateMacroInstance(typeof(PrivateConstructor));
        var second = MacroExportDiscovery.CreateMacroInstance(typeof(PrivateConstructor));
        Assert.IsType<PrivateConstructor>(first);
        Assert.NotSame(first, second);
    }

    [Theory]
    [InlineData("specific failure", "specific failure")]
    [InlineData(" ", "FormatException")]
    public void DotNet_ConstructorFailuresUnwrapInvocationExceptions(string message, string expectedDetail)
    {
        Throwing.Failure = new FormatException(message);
        var exception = Assert.Throws<InvalidOperationException>(() => MacroExportDiscovery.CreateMacroInstance(typeof(Throwing)));
        Assert.Same(Throwing.Failure, exception.InnerException);
        Assert.Contains(expectedDetail, exception.Message);
        Assert.Contains(typeof(Throwing).FullName!, exception.Message);
    }

    private sealed class Fixture : IDisposable
    {
        public AssemblyBuilder Assembly { get; }
        public ModuleBuilder Module { get; }

        public Fixture()
        {
            // Attribute Type arguments resolve by assembly name; dynamic fixtures
            // need a scoped resolver and cannot be collectible on this runtime.
            Assembly = AssemblyBuilder.DefineDynamicAssembly(new AssemblyName("MacroDiscovery_" + Guid.NewGuid().ToString("N")), AssemblyBuilderAccess.Run);
            Module = Assembly.DefineDynamicModule("Main");
            AppDomain.CurrentDomain.AssemblyResolve += Resolve;
        }

        private Assembly? Resolve(object? sender, ResolveEventArgs args)
            => new AssemblyName(args.Name).Name == Assembly.GetName().Name ? Assembly : null;

        public void Dispose() => AppDomain.CurrentDomain.AssemblyResolve -= Resolve;
    }

    private static Fixture NewAssembly() => new();

    private static Type DefineMacro(Fixture assembly, string name, bool visible, bool macro = true)
    {
        var module = assembly.Module;
        var type = module.DefineType(name, (visible ? TypeAttributes.Public : TypeAttributes.NotPublic) | TypeAttributes.Class);
        type.DefineDefaultConstructor(MethodAttributes.Public);
        if (macro)
        {
            type.AddInterfaceImplementation(typeof(IMacroDefinition));
            var expand = type.DefineMethod("Expand", MethodAttributes.Public, typeof(int), Type.EmptyTypes);
            var il = expand.GetILGenerator();
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Ret);
        }
        return type.CreateType()!;
    }

    private static void Mark(Fixture assembly, Type? exportedType = null)
        => assembly.Assembly.SetCustomAttribute(exportedType is null
            ? new CustomAttributeBuilder(typeof(RavenCompilerPluginAttribute).GetConstructor(Type.EmptyTypes)!, [])
            : new CustomAttributeBuilder(typeof(RavenCompilerPluginAttribute).GetConstructor([typeof(Type)])!, [exportedType]));

    public sealed class Good : IMacroDefinition { public int Expand() => 1; }
    public sealed class PrivateConstructor : IMacroDefinition { private PrivateConstructor() { } public int Expand() => 1; }
    public abstract class Abstract : IMacroDefinition { public int Expand() => 1; }
    public sealed class Generic<T> : IMacroDefinition { public int Expand() => 1; }
    public sealed class NoDefaultConstructor : IMacroDefinition { public NoDefaultConstructor(int value) { } public int Expand() => 1; }
    public sealed class Unclassified : IMacroDefinition { }
    public sealed class Ambiguous : IMacroDefinition { public int Expand() => 1; public int Expand(int value) => value; }
    public sealed class Throwing : IMacroDefinition
    {
        public static Exception Failure = new FormatException();
        public Throwing() => throw new TargetInvocationException(new TargetInvocationException(Failure));
        public int Expand() => 1;
    }
}
