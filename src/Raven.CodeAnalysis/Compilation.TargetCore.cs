using System.Linq;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private static readonly DiagnosticDescriptor s_invalidTargetCore = DiagnosticDescriptor.Create(
        "RAVT003", "Invalid target core configuration", "", "",
        "Target core configuration cannot be used: {0}.", "compiler", DiagnosticSeverity.Error, true);

    private static Diagnostic TargetCoreError(string reason)
        => Diagnostic.Create(s_invalidTargetCore, Location.None, reason);

    private Diagnostic? GetTargetCoreConfigurationDiagnostic()
    {
        if (Options.RuntimeUnitContract is { } unit)
        {
            if (Options.TargetCoreAssemblyName != unit.AssemblyName || string.IsNullOrWhiteSpace(unit.TypeName))
                return TargetCoreError("the unit contract requires its explicitly configured target core assembly and type");
            var type = GetTypeByMetadataName(unit.TypeName, unit.AssemblyName);
            if (type is null || !type.IsValueType || type.Arity != 0 || type.ContainingType is not null || type.ContainingAssembly?.Name != unit.AssemblyName
                || type.GetMembers().OfType<IFieldSymbol>().Any(field => !field.IsStatic))
                return TargetCoreError("the unit contract must name an empty value type in the target core");
        }
        if (Options.TargetCoreAssemblyName is not { } name)
            return null;
        if (string.IsNullOrWhiteSpace(name) || Options.MetadataImportOptions?.CoreAssemblyName != name)
            return TargetCoreError("emission requires the same explicitly supplied metadata core assembly");
        return null;
    }

    private bool TryResolveTargetEmitOptions(EmitOptions? requested, out EmitOptions? effective, out Diagnostic? diagnostic)
    {
        effective = requested;
        diagnostic = GetTargetCoreConfigurationDiagnostic();
        if (diagnostic is not null)
            return false;
        if (Options.TargetCoreAssemblyName is null)
            return true;

        // Metadata setup has already resolved this assembly exclusively from the
        // supplied references. No host reflection load or runner-side selection.
        var identity = CoreAssembly.GetName();
        if (requested?.TargetCoreLibraryIdentity is { } explicitIdentity && explicitIdentity.FullName != identity.FullName)
        {
            diagnostic = TargetCoreError("explicit emission options conflict with the project's target core identity");
            return false;
        }
        effective = new EmitOptions(identity);
        return true;
    }
}
