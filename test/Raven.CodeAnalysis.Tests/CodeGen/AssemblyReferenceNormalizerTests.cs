using Mono.Cecil;

using System.Reflection;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

using MetadataReaderProvider = System.Reflection.Metadata.MetadataReaderProvider;
using MetadataStreamOptions = System.Reflection.Metadata.MetadataStreamOptions;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class AssemblyReferenceNormalizerTests
{
    [Fact]
    public void NormalizeCoreLibReference_DoesNotResolveReferencedAssemblies()
    {
        using var input = File.OpenRead(typeof(Compilation).Assembly.Location);
        using var output = new MemoryStream();
        using var resolver = new ThrowingAssemblyResolver();

        AssemblyReferenceNormalizer.NormalizeCoreLibReference(input, output, resolver);

        Assert.NotEqual(0, output.Length);
    }

    [Fact]
    public void RetargetAssemblyReferences_UsesTargetCompilationIdentity()
    {
        using var input = File.OpenRead(typeof(Compilation).Assembly.Location);
        using var source = AssemblyDefinition.ReadAssembly(typeof(Compilation).Assembly.Location);
        var sourceReference = source.MainModule.AssemblyReferences.First();
        var targetVersion = new Version(42, 0, 0, 0);
        var targetReference = new AssemblyNameReference(sourceReference.Name, targetVersion)
        {
            Culture = sourceReference.Culture,
            PublicKeyToken = [.. sourceReference.PublicKeyToken]
        };
        var targetReferences = new Dictionary<string, AssemblyNameReference>(StringComparer.OrdinalIgnoreCase)
        {
            [targetReference.Name] = targetReference
        };
        using var output = new MemoryStream();
        using var resolver = new ThrowingAssemblyResolver();

        AssemblyReferenceNormalizer.RetargetAssemblyReferences(
            input,
            output,
            targetReferences,
            resolver);

        output.Position = 0;
        using var retargeted = AssemblyDefinition.ReadAssembly(output);
        Assert.Contains(
            retargeted.MainModule.AssemblyReferences,
            reference => reference.Name == targetReference.Name && reference.Version == targetVersion);
    }

    [Fact]
    public void RetargetCoreLibraryReference_RewritesHostCoreScopesToTargetIdentity()
    {
        using var input = File.OpenRead(typeof(Compilation).Assembly.Location);
        using var output = new MemoryStream();
        using var resolver = new ThrowingAssemblyResolver();
        var targetIdentity = new AssemblyNameReference("mscorlib", new Version(1, 17, 11, 0));

        AssemblyReferenceNormalizer.RetargetCoreLibraryReference(
            input,
            output,
            targetIdentity,
            resolver);

        output.Position = 0;
        using var retargeted = AssemblyDefinition.ReadAssembly(output);
        var references = retargeted.MainModule.AssemblyReferences;

        Assert.Contains(references, reference => reference.FullName == targetIdentity.FullName);
        Assert.DoesNotContain(references, reference => reference.Name == "System.Private.CoreLib");
        Assert.DoesNotContain(references, reference => reference.Name == "System.Runtime");
        Assert.DoesNotContain(
            retargeted.MainModule.GetTypeReferences(),
            type => type.Scope is AssemblyNameReference reference &&
                    (reference.Name == "System.Private.CoreLib" || reference.Name == "System.Runtime"));
    }

    [Fact]
    public void EmitOptions_RetargetsRavenAssemblyToTargetCoreIdentity()
    {
        var syntaxTree = SyntaxTree.ParseText("func Main() {}");
        var compilation = Compilation.Create(
            "TargetCoreProbe",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var targetIdentity = new AssemblyName("mscorlib, Version=1.17.11.0, Culture=neutral, PublicKeyToken=null");
        using var output = new MemoryStream();

        var result = compilation.Emit(output, pdbStream: null, emitOptions: new EmitOptions(targetIdentity));

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        output.Position = 0;
        using var assembly = AssemblyDefinition.ReadAssembly(output);
        Assert.Contains(
            assembly.MainModule.AssemblyReferences,
            reference => reference.Name == "mscorlib" && reference.Version == targetIdentity.Version);
        Assert.DoesNotContain(
            assembly.MainModule.AssemblyReferences,
            reference => reference.Name == "System.Private.CoreLib" || reference.Name == "System.Runtime");
    }

    [Fact]
    public void EmitOptions_RetargetedAssemblyKeepsMatchingPortablePdb()
    {
        var syntaxTree = SyntaxTree.ParseText("func Main() { let value = 42 }");
        var compilation = Compilation.Create(
            "TargetCoreSymbolsProbe",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var targetIdentity = new AssemblyName("mscorlib, Version=1.17.11.0, Culture=neutral, PublicKeyToken=null");
        using var output = new MemoryStream();
        using var pdbOutput = new MemoryStream();

        var result = compilation.Emit(output, pdbOutput, new EmitOptions(targetIdentity));

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        output.Position = 0;
        pdbOutput.Position = 0;
        using var peReader = new PEReader(output, PEStreamOptions.LeaveOpen);
        var codeViewEntry = Assert.Single(
            peReader.ReadDebugDirectory(),
            static entry => entry.Type == DebugDirectoryEntryType.CodeView);
        var codeView = peReader.ReadCodeViewDebugDirectoryData(codeViewEntry);
        using var pdbProvider = MetadataReaderProvider.FromPortablePdbStream(
            pdbOutput,
            MetadataStreamOptions.LeaveOpen);
        var pdbReader = pdbProvider.GetMetadataReader();
        var pdbId = pdbReader.DebugMetadataHeader.Id;
        var visibleSequencePoints = Enumerable.Range(
                1,
                pdbReader.GetTableRowCount(TableIndex.MethodDebugInformation))
            .SelectMany(row => pdbReader
                .GetMethodDebugInformation(MetadataTokens.MethodDebugInformationHandle(row))
                .GetSequencePoints())
            .Where(static point => !point.IsHidden)
            .ToArray();

        Assert.Equal(codeView.Guid, new Guid(pdbId.AsSpan(0, 16)));
        Assert.Equal(codeViewEntry.Stamp, BitConverter.ToUInt32(pdbId.AsSpan(16, 4)));
        Assert.Contains(visibleSequencePoints, static point => point.StartLine == 1);
    }

    private sealed class ThrowingAssemblyResolver : IAssemblyResolver
    {
        public AssemblyDefinition Resolve(AssemblyNameReference name)
            => throw new InvalidOperationException($"Unexpected resolution of '{name}'.");

        public AssemblyDefinition Resolve(
            AssemblyNameReference name,
            ReaderParameters parameters)
            => throw new InvalidOperationException($"Unexpected resolution of '{name}'.");

        public void Dispose()
        {
        }
    }
}
