// Raven async_entry_raven_noinst.dll
.method assembly final hidebysig newslot virtual
        instance void MoveNext () cil managed
{
        .override method instance void [System.Private.CoreLib]System.Runtime.CompilerServices.IAsyncStateMachine::MoveNext()
        // Header size: 12
        // Code size: 267 (0x10b)
        .maxstack 3
        .locals init (
                [0] int32,
                [1] int32,
                [2] valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32>,
                [3] valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32>,
                [4] valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32>,
                [5] int32,
                [6] int32,
                [7] valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32>,
                [8] int32,
                [9] int32,
                [10] class [System.Private.CoreLib]System.Exception,
                [11] int32
        )

        .try
        {
                IL_0000: ldarg.0
                IL_0001: ldfld int32 Program/'Program+<>c__AsyncStateMachine1'::_state
                IL_0006: ldc.i4.m1
                IL_0007: ceq
                IL_0009: brtrue IL_0013

                IL_000e: br IL_0018

                IL_0013: br IL_0035

                IL_0018: ldarg.0
                IL_0019: ldfld int32 Program/'Program+<>c__AsyncStateMachine1'::_state
                IL_001e: ldc.i4.0
                IL_001f: ceq
                IL_0021: brtrue IL_002b

                IL_0026: br IL_0030

                IL_002b: br IL_0087

                IL_0030: br IL_0035

                IL_0035: ldc.i4.s 42
                IL_0037: call class [System.Private.CoreLib]System.Threading.Tasks.Task`1<int32> Program::Test(int32)
                IL_003c: call instance valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<!0> class [System.Private.CoreLib]System.Threading.Tasks.Task`1<int32>::GetAwaiter()
                IL_0041: stloc.s 4
                IL_0043: ldarg.0
                IL_0044: ldloc.s 4
                IL_0046: stfld valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32> Program/'Program+<>c__AsyncStateMachine1'::'<>awaiter0'
                IL_004b: ldarg.0
                IL_004c: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32> Program/'Program+<>c__AsyncStateMachine1'::'<>awaiter0'
                IL_0051: call instance bool valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32>::get_IsCompleted()
                IL_0056: brtrue IL_0060

                IL_005b: br IL_0065

                IL_0060: br IL_0087

                IL_0065: ldc.i4.0
                IL_0066: stloc.s 5
                IL_0068: ldarg.0
                IL_0069: ldloc.s 5
                IL_006b: stfld int32 Program/'Program+<>c__AsyncStateMachine1'::_state
                IL_0070: ldarg.0
                IL_0071: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder Program/'Program+<>c__AsyncStateMachine1'::_builder
                IL_0076: ldarg.0
                IL_0077: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32> Program/'Program+<>c__AsyncStateMachine1'::'<>awaiter0'
                IL_007c: ldarg.0
                IL_007d: call instance void [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder::AwaitUnsafeOnCompleted<valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32>, valuetype Program/'Program+<>c__AsyncStateMachine1'>(!!0&, !!1&)
                IL_0082: leave IL_010a

                IL_0087: ldc.i4.m1
                IL_0088: stloc.s 6
                IL_008a: ldarg.0
                IL_008b: ldloc.s 6
                IL_008d: stfld int32 Program/'Program+<>c__AsyncStateMachine1'::_state
                IL_0092: ldarg.0
                IL_0093: ldfld valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32> Program/'Program+<>c__AsyncStateMachine1'::'<>awaiter0'
                IL_0098: stloc.2
                IL_0099: ldsfld valuetype System.Unit System.Unit::Value
                IL_009e: pop
                IL_009f: ldloc.3
                IL_00a0: stloc.s 7
                IL_00a2: ldarg.0
                IL_00a3: ldloc.s 7
                IL_00a5: stfld valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32> Program/'Program+<>c__AsyncStateMachine1'::'<>awaiter0'
                IL_00aa: ldloca.s 2
                IL_00ac: call instance !0 valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<int32>::GetResult()
                IL_00b1: stloc.1
                IL_00b2: ldsfld valuetype System.Unit System.Unit::Value
                IL_00b7: pop
                IL_00b8: ldloc.1
                IL_00b9: stloc.s 8
                IL_00bb: ldloc.s 8
                IL_00bd: stloc.0
                IL_00be: ldloc.0
                IL_00bf: call void [System.Console]System.Console::WriteLine(int32)
                IL_00c4: ldc.i4.s -2
                IL_00c6: stloc.s 9
                IL_00c8: ldarg.0
                IL_00c9: ldloc.s 9
                IL_00cb: stfld int32 Program/'Program+<>c__AsyncStateMachine1'::_state
                IL_00d0: ldarg.0
                IL_00d1: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder Program/'Program+<>c__AsyncStateMachine1'::_builder
                IL_00d6: call instance void [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder::SetResult()
                IL_00db: leave IL_010a

                IL_00e0: leave IL_010a
        } // end .try
        catch [System.Private.CoreLib]System.Exception
        {
                IL_00e5: stloc.s 10
                IL_00e7: ldc.i4.s -2
                IL_00e9: stloc.s 11
                IL_00eb: ldarg.0
                IL_00ec: ldloc.s 11
                IL_00ee: stfld int32 Program/'Program+<>c__AsyncStateMachine1'::_state
                IL_00f3: ldarg.0
                IL_00f4: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder Program/'Program+<>c__AsyncStateMachine1'::_builder
                IL_00f9: ldloc.s 10
                IL_00fb: call instance void [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder::SetException(class [System.Private.CoreLib]System.Exception)
                IL_0100: leave IL_010a

                IL_0105: leave IL_010a
        } // end handler

        IL_010a: ret
} // end of method 'Program+<>c__AsyncStateMachine1'::MoveNext
