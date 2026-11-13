using System;
using System.IO;
using System.Linq;

using Mono.Cecil;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "DotNet")]
public sealed class MacroAssemblyMetadataContractTests
{
    [Theory]
    [InlineData(false, "Raven.CodeAnalysis.Macros", "RavenCompilerPluginAttribute", true)]
    [InlineData(true, "Raven.CodeAnalysis.Macros", "RavenCompilerPluginAttribute", true)]
    [InlineData(false, "Other", "RavenCompilerPluginAttribute", false)]
    [InlineData(true, "Raven.CodeAnalysis.Macros", "OtherAttribute", false)]
    public void DotNet_MarkerRecognizesReferencedAndDefinedConstructors(bool local, string ns, string name, bool expected)
    {
        var path = Path.Combine(Path.GetTempPath(), Guid.NewGuid() + ".dll");
        try
        {
            using (var assembly = AssemblyDefinition.CreateAssembly(new AssemblyNameDefinition("DiscoveryFixture", new Version(1, 0)), "DiscoveryFixture", ModuleKind.Dll))
            {
                var module = assembly.MainModule;
                MethodReference constructor;
                if (local)
                {
                    var type = new TypeDefinition(ns, name, TypeAttributes.Public | TypeAttributes.Class, module.ImportReference(typeof(Attribute)));
                    module.Types.Add(type);
                    var method = new MethodDefinition(".ctor", MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.RTSpecialName, module.TypeSystem.Void);
                    type.Methods.Add(method);
                    method.Body.Instructions.Add(Mono.Cecil.Cil.Instruction.Create(Mono.Cecil.Cil.OpCodes.Ret));
                    constructor = method;
                }
                else
                {
                    var reference = new AssemblyNameReference("ExternalPluginContract", new Version(1, 0));
                    module.AssemblyReferences.Add(reference);
                    var type = new TypeReference(ns, name, module, reference);
                    constructor = new MethodReference(".ctor", module.TypeSystem.Void, type) { HasThis = true };
                }
                assembly.CustomAttributes.Add(new CustomAttribute(constructor));
                module.AssemblyReferences.Add(new AssemblyNameReference("DependencyOne", new Version(1, 0)));
                module.AssemblyReferences.Add(new AssemblyNameReference("DependencyTwo", new Version(1, 0)));
                assembly.Write(path);
            }
            Assert.Equal(expected, MacroAssemblyMetadata.HasCompilerPluginMarker(path));
            Assert.Equal("DiscoveryFixture", MacroAssemblyMetadata.GetAssemblyName(path));
            var names = MacroAssemblyMetadata.GetReferencedAssemblyNames(path);
            Assert.Contains("DependencyOne", names);
            Assert.Contains("DependencyTwo", names);
            Assert.True(names.IndexOf("DependencyOne") < names.IndexOf("DependencyTwo"));
            // Readers must release handles, including successful early marker returns.
            using var exclusive = File.Open(path, FileMode.Open, FileAccess.ReadWrite, FileShare.None);
        }
        finally
        {
            File.Delete(path);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DotNet_MissingAndMalformedFilesReturnEmptyResults(bool createMalformed)
    {
        var path = Path.Combine(Path.GetTempPath(), Guid.NewGuid() + ".dll");
        try
        {
            if (createMalformed)
                File.WriteAllText(path, "not a PE image");
            Assert.False(MacroAssemblyMetadata.HasCompilerPluginMarker(path));
            Assert.Null(MacroAssemblyMetadata.GetAssemblyName(path));
            Assert.Empty(MacroAssemblyMetadata.GetReferencedAssemblyNames(path));
        }
        finally
        {
            File.Delete(path);
        }
    }

    [Fact]
    public void DotNet_NetModuleHasReferencesButNoAssemblyIdentityOrMarker()
    {
        var path = Path.Combine(Path.GetTempPath(), Guid.NewGuid() + ".netmodule");
        try
        {
            using (var module = ModuleDefinition.CreateModule("ModuleFixture", ModuleKind.NetModule))
            {
                module.AssemblyReferences.Add(new AssemblyNameReference("ModuleDependency", new Version(1, 0)));
                module.Write(path);
            }
            Assert.False(MacroAssemblyMetadata.HasCompilerPluginMarker(path));
            Assert.Null(MacroAssemblyMetadata.GetAssemblyName(path));
            Assert.Contains("ModuleDependency", MacroAssemblyMetadata.GetReferencedAssemblyNames(path));
        }
        finally
        {
            File.Delete(path);
        }
    }

    [Fact]
    public void DotNet_UnmarkedAssemblyStillExposesItsIdentity()
    {
        var path = typeof(object).Assembly.Location;
        Assert.False(MacroAssemblyMetadata.HasCompilerPluginMarker(path));
        Assert.Equal(typeof(object).Assembly.GetName().Name, MacroAssemblyMetadata.GetAssemblyName(path));
    }
}
