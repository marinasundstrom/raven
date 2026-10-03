using System;
using System.Linq;

namespace Raven.CodeAnalysis.Targets;

// Shared implementation of the current CLI transport, not a universal native
// runtime contract. Platform policy is supplied by the selected concrete contract.
internal abstract partial class CliRuntimeContract(CompilationOptions options)
{
    internal string PreferredSpecialTypeAssemblyName => "System.Runtime";

    protected CompilationOptions Options { get; } = options;

    internal abstract string TupleTypeName { get; }
    internal abstract bool UsesInhabitedDelegateResults { get; }
    internal abstract bool HasNativeSelfContract { get; }

    protected abstract string? GetPlatformConfigurationError();

    // Configuration-only checks must not open references or resolve symbols.
    internal string? GetConfigurationError()
    {
        if (GetPlatformConfigurationError() is { } platformError)
            return platformError;

        if (Options.TargetCoreAssemblyName is { } coreName &&
            (string.IsNullOrWhiteSpace(coreName) ||
             (!Options.UsesDiscoveredTargetCore && Options.MetadataImportOptions?.CoreAssemblyName != coreName)))
        {
            return "emission requires the same explicitly supplied metadata core assembly";
        }

        if (Options.RuntimeUnitContract is { } unit &&
            (string.IsNullOrWhiteSpace(unit.AssemblyName) ||
             string.IsNullOrWhiteSpace(unit.TypeName) ||
             (!unit.MapClrVoidToUnit && Options.TargetCoreAssemblyName != unit.AssemblyName) ||
             (unit.MapClrVoidToUnit && (Options.TargetPlatform != TargetPlatform.DotNet ||
                 Options.TargetCoreAssemblyName is { } selectedCore && selectedCore != unit.AssemblyName))))
        {
            return "the unit contract requires an explicit target core, or the .NET void-to-unit bootstrap policy";
        }

        if (Options.RuntimeTypeOfContract is { } typeOf &&
            (string.IsNullOrWhiteSpace(typeOf.AssemblyName) ||
             string.IsNullOrWhiteSpace(typeOf.TypeInfoTypeName) ||
             string.IsNullOrWhiteSpace(typeOf.ContextTypeName)))
        {
            return "the typeof contract requires assembly, type-info interface and context type names";
        }

        return null;
    }

    internal string? GetResolvedConfigurationError(Compilation compilation, string? metadataCoreName)
    {
        if (GetConfigurationError() is { } error)
            return error;

        if (Options.RuntimeSelfTypeContract is not null &&
            (compilation.ResolveRuntimeSelfType() is not { Arity: 0, DeclaredAccessibility: Accessibility.Public } selfType ||
             selfType.GetMembers().OfType<IFieldSymbol>().Any(field => !field.IsStatic)))
            return "native Self requires a public, nongeneric, fieldless marker in the configured assembly; this contract targets a native runtime, not CLR execution";

        if (Options.RuntimeTypeOfContract is not null && ResolveTypeOf(compilation) is null)
            return "the typeof contract requires a public interface and context in the configured assembly, with public static Current and instance GetTypeInfoFromHandle(RuntimeTypeHandle) returning that interface";

        if (Options.RuntimeUnitContract is { } unit)
        {
            var type = compilation.GetTypeByMetadataName(unit.TypeName, unit.AssemblyName);
            if (type is null || unit.MapClrVoidToUnit && (type.SpecialType == SpecialType.System_Void || type.DeclaredAccessibility != Accessibility.Public) || !type.IsValueType || type.Arity != 0 || type.ContainingType is not null || type.ContainingAssembly?.Name != unit.AssemblyName
                || type.GetMembers().OfType<IFieldSymbol>().Any(field => !field.IsStatic))
                return "the unit contract must name a public empty non-void value type in its configured assembly";
        }

        if (Options.TargetCoreAssemblyName is { } name && Options.UsesDiscoveredTargetCore && metadataCoreName != name)
            return "emission requires the same explicitly supplied metadata core assembly";

        return null;
    }

    internal string GetSpecialTypeMetadataName(SpecialType specialType)
    {
        return specialType switch
        {
            SpecialType.System_Object => "System.Object",
            SpecialType.System_Enum => "System.Enum",
            SpecialType.System_MulticastDelegate => "System.MulticastDelegate",
            SpecialType.System_Delegate => "System.Delegate",
            SpecialType.System_ValueType => "System.ValueType",
            SpecialType.System_Void => "System.Void",
            SpecialType.System_Boolean => "System.Boolean",
            SpecialType.System_Char => "System.Char",
            SpecialType.System_SByte => "System.SByte",
            SpecialType.System_Byte => "System.Byte",
            SpecialType.System_Int16 => "System.Int16",
            SpecialType.System_UInt16 => "System.UInt16",
            SpecialType.System_Int32 => "System.Int32",
            SpecialType.System_UInt32 => "System.UInt32",
            SpecialType.System_Int64 => "System.Int64",
            SpecialType.System_UInt64 => "System.UInt64",
            SpecialType.System_Decimal => "System.Decimal",
            SpecialType.System_Single => "System.Single",
            SpecialType.System_Double => "System.Double",
            SpecialType.System_String => "System.String",
            SpecialType.System_IntPtr => "System.IntPtr",
            SpecialType.System_UIntPtr => "System.UIntPtr",
            SpecialType.System_Array => "System.Array",
            SpecialType.System_Collections_IEnumerable => "System.Collections.IEnumerable",
            SpecialType.System_Collections_Generic_IEnumerable_T => "System.Collections.Generic.IEnumerable`1",
            SpecialType.System_Collections_Generic_IList_T => "System.Collections.Generic.IList`1",
            SpecialType.System_Collections_Generic_ICollection_T => "System.Collections.Generic.ICollection`1",
            SpecialType.System_Collections_IEnumerator => "System.Collections.IEnumerator",
            SpecialType.System_Collections_Generic_IEnumerator_T => "System.Collections.Generic.IEnumerator`1",
            SpecialType.System_Nullable_T => "System.Nullable",
            SpecialType.System_DateTime => "System.DateTime",
            SpecialType.System_Runtime_CompilerServices_IsVolatile => "System.Runtime.CompilerServices.IsVolatile",
            SpecialType.System_IDisposable => "System.IDisposable",
            SpecialType.System_TypedReference => "System.TypedReference",
            SpecialType.System_ArgIterator => "System.ArgIterator",
            SpecialType.System_RuntimeArgumentHandle => "System.RuntimeArgumentHandle",
            SpecialType.System_RuntimeFieldHandle => "System.RuntimeFieldHandle",
            SpecialType.System_RuntimeMethodHandle => "System.RuntimeMethodHandle",
            SpecialType.System_RuntimeTypeHandle => "System.RuntimeTypeHandle",
            SpecialType.System_IAsyncResult => "System.IAsyncResult",
            SpecialType.System_AsyncCallback => "System.AsyncCallback",
            SpecialType.System_Runtime_CompilerServices_AsyncVoidMethodBuilder => "System.Runtime.CompilerServices.AsyncVoidMethodBuilder",
            SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder => "System.Runtime.CompilerServices.AsyncTaskMethodBuilder",
            SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T => "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1",
            SpecialType.System_Runtime_CompilerServices_AsyncStateMachineAttribute => "System.Runtime.CompilerServices.AsyncStateMachineAttribute",
            SpecialType.System_Runtime_CompilerServices_IteratorStateMachineAttribute => "System.Runtime.CompilerServices.IteratorStateMachineAttribute",
            SpecialType.System_Threading_Tasks_Task => "System.Threading.Tasks.Task",
            SpecialType.System_Threading_Tasks_Task_T => Options.UseHeapAsyncStateMachines && Options.TargetCoreAssemblyName is not null ? "System.Tasks.Task`1" : "System.Threading.Tasks.Task`1",
            SpecialType.System_Runtime_InteropServices_WindowsRuntime_EventRegistrationToken => "System.Runtime.InteropServices.WindowsRuntime.EventRegistrationToken",
            SpecialType.System_Runtime_InteropServices_WindowsRuntime_EventRegistrationTokenTable_T => "System.Runtime.InteropServices.WindowsRuntime.EventRegistrationTokenTable`1",
            SpecialType.System_ValueTuple_T1 => $"{TupleTypeName}`1",
            SpecialType.System_ValueTuple_T2 => $"{TupleTypeName}`2",
            SpecialType.System_ValueTuple_T3 => $"{TupleTypeName}`3",
            SpecialType.System_ValueTuple_T4 => $"{TupleTypeName}`4",
            SpecialType.System_ValueTuple_T5 => $"{TupleTypeName}`5",
            SpecialType.System_ValueTuple_T6 => $"{TupleTypeName}`6",
            SpecialType.System_ValueTuple_T7 => $"{TupleTypeName}`7",
            SpecialType.System_ValueTuple_TRest => $"{TupleTypeName}`8",
            SpecialType.System_Type => "System.Type",
            SpecialType.System_Exception => "System.Exception",
            SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine => "System.Runtime.CompilerServices.IAsyncStateMachine",
            _ => throw new InvalidOperationException("Special type is not supported."),
        };
    }
}
