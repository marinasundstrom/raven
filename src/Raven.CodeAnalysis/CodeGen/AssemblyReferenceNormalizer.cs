using Mono.Cecil;

namespace Raven.CodeAnalysis.CodeGen;

internal static class AssemblyReferenceNormalizer
{
    private static readonly byte[] SystemRuntimePublicKeyToken = [0xb0, 0x3f, 0x5f, 0x7f, 0x11, 0xd5, 0x0a, 0x3a];

    internal static void NormalizeCoreLibReference(Stream peInput, Stream peOutput)
    {
        if (peInput is null)
            throw new ArgumentNullException(nameof(peInput));
        if (peOutput is null)
            throw new ArgumentNullException(nameof(peOutput));

        peInput.Position = 0;

        var assembly = AssemblyDefinition.ReadAssembly(
            peInput,
            new ReaderParameters
            {
                InMemory = true,
                ReadingMode = ReadingMode.Immediate
            });

        var module = assembly.MainModule;
        var coreLibRefs = module.AssemblyReferences
            .Where(reference => string.Equals(reference.Name, "System.Private.CoreLib", StringComparison.OrdinalIgnoreCase))
            .ToArray();

        if (coreLibRefs.Length == 0)
        {
            peInput.Position = 0;
            peInput.CopyTo(peOutput);
            return;
        }

        var runtimeRef = module.AssemblyReferences
            .FirstOrDefault(reference => string.Equals(reference.Name, "System.Runtime", StringComparison.OrdinalIgnoreCase));

        foreach (var coreLibRef in coreLibRefs)
        {
            runtimeRef ??= CreateSystemRuntimeReference(coreLibRef);
            RewriteReferenceScope(module, coreLibRef, runtimeRef);
            module.AssemblyReferences.Remove(coreLibRef);
        }

        if (!module.AssemblyReferences.Contains(runtimeRef))
            module.AssemblyReferences.Add(runtimeRef);

        assembly.Write(peOutput);
    }

    private static AssemblyNameReference CreateSystemRuntimeReference(AssemblyNameReference sourceReference)
    {
        var runtimeReference = new AssemblyNameReference("System.Runtime", sourceReference.Version)
        {
            Culture = sourceReference.Culture,
            HasPublicKey = false
        };

        runtimeReference.PublicKeyToken = [.. SystemRuntimePublicKeyToken];
        return runtimeReference;
    }

    private static void RewriteReferenceScope(ModuleDefinition module, AssemblyNameReference oldReference, AssemblyNameReference newReference)
    {
        foreach (var typeReference in module.GetTypeReferences())
        {
            if (ReferenceEquals(typeReference.Scope, oldReference))
                typeReference.Scope = newReference;
        }

        foreach (var memberReference in module.GetMemberReferences())
        {
            if (memberReference.DeclaringType is { Scope: var scope } && ReferenceEquals(scope, oldReference))
                memberReference.DeclaringType.Scope = newReference;
        }
    }
}
