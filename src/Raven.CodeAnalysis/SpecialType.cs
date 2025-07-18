namespace Raven.CodeAnalysis;

public enum SpecialType
{
    None = 0,

    // System namespace
    System_Object = 1,
    System_Enum = 2,
    System_MulticastDelegate = 3,
    System_Delegate = 4,
    System_ValueType = 5,
    System_Void = 6,
    System_Boolean = 7,
    System_Char = 8,
    System_SByte = 9,
    System_Byte = 10,
    System_Int16 = 11,
    System_UInt16 = 12,
    System_Int32 = 13,
    System_UInt32 = 14,
    System_Int64 = 15,
    System_UInt64 = 16,
    System_Decimal = 17,
    System_Single = 18,
    System_Double = 19,
    System_String = 20,
    System_IntPtr = 21,
    System_UIntPtr = 22,
    System_Array = 23,
    System_Collections_IEnumerable = 24,
    System_Collections_Generic_IEnumerable_T = 25,
    System_Collections_Generic_IList_T = 26,
    System_Collections_Generic_ICollection_T = 27,
    System_Collections_IEnumerator = 28,
    System_Collections_Generic_IEnumerator_T = 29,
    System_Nullable_T = 30,

    // Task and async
    System_DateTime = 31,
    System_Runtime_CompilerServices_IsVolatile = 32,
    System_IDisposable = 33,
    System_TypedReference = 34,
    System_ArgIterator = 35,
    System_RuntimeArgumentHandle = 36,
    System_RuntimeFieldHandle = 37,
    System_RuntimeMethodHandle = 38,
    System_RuntimeTypeHandle = 39,
    System_IAsyncResult = 40,
    System_AsyncCallback = 41,
    System_Runtime_CompilerServices_AsyncVoidMethodBuilder = 42,
    System_Runtime_CompilerServices_AsyncTaskMethodBuilder = 43,
    System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T = 44,
    System_Runtime_CompilerServices_AsyncStateMachineAttribute = 45,
    System_Runtime_CompilerServices_IteratorStateMachineAttribute = 46,
    System_Threading_Tasks_Task = 47,
    System_Threading_Tasks_Task_T = 48,

    // Interop
    System_Runtime_InteropServices_WindowsRuntime_EventRegistrationToken = 49,
    System_Runtime_InteropServices_WindowsRuntime_EventRegistrationTokenTable_T = 50,

    // Tuple types
    System_ValueTuple_T1 = 51,
    System_ValueTuple_T2 = 52,
    System_ValueTuple_T3 = 53,
    System_ValueTuple_T4 = 54,
    System_ValueTuple_T5 = 55,
    System_ValueTuple_T6 = 56,
    System_ValueTuple_T7 = 57,
    System_ValueTuple_TRest = 58,

    System_Type
}