// Excerpt from Program.Program+<>c__AsyncStateMachine1.MoveNext before
// preserving the builder pointer for SetResult(). The pointer log
// consumes the address, so the subsequent call runs without 'this'.

IL_01cb: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder Program/'Program+<>c__AsyncStateMachine1'::_builder
IL_01d0: conv.u
IL_01d1: stloc.s 19
IL_01d3: ldstr "Step24/MainAsync:_builder:addr -> 0x{0:x}"
IL_01d8: ldloc.s 19
IL_01da: conv.u8
IL_01db: box [System.Private.CoreLib]System.UInt64
IL_01e0: call void [System.Console]System.Console::WriteLine(string, object)
IL_01e5: call instance void [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder::SetResult()
