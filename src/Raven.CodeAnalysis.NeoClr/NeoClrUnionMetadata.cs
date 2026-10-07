using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

// Target-owned metadata authoring. The existing Raven attribute contract carries
// union relationships; no importer objects or runtime reflection are involved.
internal static class NeoClrUnionMetadata
{
    internal static void Emit(AssemblyBuilder assembly, IReadOnlyList<SourceUnionDeclarationPlan> unions,
        Func<INamedTypeSymbol, TypeBuilder> resolve, MethodBuilder? sourceMarker = null)
    {
        var marker = sourceMarker ?? Define("System.Runtime.CompilerServices", "UnionAttribute", []);
        var caseMarker = Define("Raven.Runtime.CompilerServices", "RavenUnionCaseAttribute",
            [PrimitiveType.String, PrimitiveType.String, PrimitiveType.Int32]);
        var companionMarker = Define("Raven.Runtime.CompilerServices", "RavenUnionCompanionAttribute", [PrimitiveType.String]);
        foreach (var plan in unions)
        {
            var owner = resolve(plan.Union);
            owner.AddCustomAttribute(new(marker.Definition, []));
            foreach (var @case in plan.Union.DeclaredCaseTypes.OrderBy(c => c.Ordinal))
                owner.AddCustomAttribute(new(caseMarker.Definition,
                    [new(PrimitiveType.String, ((INamedTypeSymbol)@case).ToFullyQualifiedMetadataName()),
                     new(PrimitiveType.String, @case.Name), new(PrimitiveType.Int32, @case.Ordinal)]));
            if (plan.Union.GetMetadataCaseContainer() is SynthesizedUnionCompanionTypeSymbol companion)
                resolve(companion).AddCustomAttribute(new(companionMarker.Definition,
                    [new(PrimitiveType.String, plan.Union.ToFullyQualifiedMetadataName())]));
        }

        MethodBuilder Define(string ns, string name, PrimitiveType[] parameters)
        {
            // When no source marker exists, preserve the bounded embedded-marker contract.
            // Like Raven's embedded CLI markers, these are owned by the output assembly.
            // Native metadata currently models attribute constructors as nominal records;
            // it does not yet model the CLI System.Attribute base class.
            var owner = assembly.AddClass(ns, name, TypeVisibility.Internal);
            var constructor = owner.AddConstructor(new MethodSignature(PrimitiveType.Void, parameters));
            constructor.GetILGenerator().Return();
            return constructor;
        }
    }
}
