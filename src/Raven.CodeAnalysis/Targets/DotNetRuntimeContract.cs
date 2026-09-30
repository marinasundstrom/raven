using System;

namespace Raven.CodeAnalysis.Targets;

// Names and representations belong to the platform contract, not its loader.
internal sealed class DotNetRuntimeContract(CompilationOptions options)
{
    internal string PreferredSpecialTypeAssemblyName => "System.Runtime";

    // Preserve the experimental branch policy until neoCLR has its own target.
    internal string TupleTypeName => options.TargetCoreAssemblyName == "NeoCLR.CoreProbe"
        ? "System.Tuple" : "System.ValueTuple";

    // Configuration-only checks must not open references or resolve symbols.
    internal string? GetConfigurationError()
    {
        if (options.TargetCoreAssemblyName is { } coreName &&
            (string.IsNullOrWhiteSpace(coreName) ||
             (!options.UsesDiscoveredTargetCore && options.MetadataImportOptions?.CoreAssemblyName != coreName)))
        {
            return "emission requires the same explicitly supplied metadata core assembly";
        }

        if (options.RuntimeUnitContract is { } unit &&
            (string.IsNullOrWhiteSpace(unit.AssemblyName) ||
             string.IsNullOrWhiteSpace(unit.TypeName) ||
             options.TargetCoreAssemblyName != unit.AssemblyName))
        {
            return "the unit contract requires its explicitly configured target core assembly and type";
        }

        if (options.RuntimeTypeOfContract is { } typeOf &&
            (string.IsNullOrWhiteSpace(typeOf.AssemblyName) ||
             string.IsNullOrWhiteSpace(typeOf.TypeInfoTypeName) ||
             string.IsNullOrWhiteSpace(typeOf.ContextTypeName)))
        {
            return "the typeof contract requires assembly, type-info interface and context type names";
        }

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
            SpecialType.System_Threading_Tasks_Task_T => options.UseHeapAsyncStateMachines && options.TargetCoreAssemblyName is not null ? "System.Tasks.Task`1" : "System.Threading.Tasks.Task`1",
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
