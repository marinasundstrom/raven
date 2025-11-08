// Roslyn RoslynAsyncEntry.dll
.method private final hidebysig newslot virtual
        instance void MoveNext () cil managed
{
        .override method instance void [System.Runtime]System.Runtime.CompilerServices.IAsyncStateMachine::MoveNext()
        // Header size: 12
        // Code size: 152 (0x98)
        .maxstack 3
        .locals init (
                [0] int32,
                [1] int32,
                [2] valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<int32>,
                [3] class [System.Runtime]System.Exception
        )

        IL_0000: ldarg.0
        IL_0001: ldfld int32 Program/'<Main>d__0'::'<>1__state'
        IL_0006: stloc.0
        .try
        {
                IL_0007: ldloc.0
                IL_0008: brfalse.s IL_0040

                IL_000a: ldc.i4.s 42
                IL_000c: call class [System.Runtime]System.Threading.Tasks.Task`1<int32> Program::Test(int32)
                IL_0011: callvirt instance valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<!0> class [System.Runtime]System.Threading.Tasks.Task`1<int32>::GetAwaiter()
                IL_0016: stloc.2
                IL_0017: ldloca.s 2
                IL_0019: call instance bool valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<int32>::get_IsCompleted()
                IL_001e: brtrue.s IL_005c

                IL_0020: ldarg.0
                IL_0021: ldc.i4.0
                IL_0022: dup
                IL_0023: stloc.0
                IL_0024: stfld int32 Program/'<Main>d__0'::'<>1__state'
                IL_0029: ldarg.0
                IL_002a: ldloc.2
                IL_002b: stfld valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<int32> Program/'<Main>d__0'::'<>u__1'
                IL_0030: ldarg.0
                IL_0031: ldflda valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32> Program/'<Main>d__0'::'<>t__builder'
                IL_0036: ldloca.s 2
                IL_0038: ldarg.0
                IL_0039: call instance void valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32>::AwaitUnsafeOnCompleted<valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<int32>, valuetype Program/'<Main>d__0'>(!!0&, !!1&)
                IL_003e: leave.s IL_0097

                IL_0040: ldarg.0
                IL_0041: ldfld valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<int32> Program/'<Main>d__0'::'<>u__1'
                IL_0046: stloc.2
                IL_0047: ldarg.0
                IL_0048: ldflda valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<int32> Program/'<Main>d__0'::'<>u__1'
                IL_004d: initobj valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<int32>
                IL_0053: ldarg.0
                IL_0054: ldc.i4.m1
                IL_0055: dup
                IL_0056: stloc.0
                IL_0057: stfld int32 Program/'<Main>d__0'::'<>1__state'

                IL_005c: ldloca.s 2
                IL_005e: call instance !0 valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter`1<int32>::GetResult()
                IL_0063: call void [System.Console]System.Console::WriteLine(int32)
                IL_0068: ldc.i4.0
                IL_0069: stloc.1
                IL_006a: leave.s IL_0083
        } // end .try
        catch [System.Runtime]System.Exception
        {
                IL_006c: stloc.3
                IL_006d: ldarg.0
                IL_006e: ldc.i4.s -2
                IL_0070: stfld int32 Program/'<Main>d__0'::'<>1__state'
                IL_0075: ldarg.0
                IL_0076: ldflda valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32> Program/'<Main>d__0'::'<>t__builder'
                IL_007b: ldloc.3
                IL_007c: call instance void valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32>::SetException(class [System.Runtime]System.Exception)
                IL_0081: leave.s IL_0097
        } // end handler

        IL_0083: ldarg.0
        IL_0084: ldc.i4.s -2
        IL_0086: stfld int32 Program/'<Main>d__0'::'<>1__state'
        IL_008b: ldarg.0
        IL_008c: ldflda valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32> Program/'<Main>d__0'::'<>t__builder'
        IL_0091: ldloc.1
        IL_0092: call instance void valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32>::SetResult(!0)

        IL_0097: ret
} // end of method '<Main>d__0'::MoveNext
