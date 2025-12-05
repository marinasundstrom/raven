.method hidebysig static default [System.Private.CoreLib]System.Threading.Tasks.Task`1<!0> Test(int32 'value') cil managed
{
  .locals init(Program/Program+<>c__AsyncStateMachine0 V_0, int32 V_1, int32 V_2, class [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0> V_3)
  IL_0000: newobj instance void class Program/Program+<>c__AsyncStateMachine0::.ctor()
  IL_0005: stloc.0
  IL_0006: ldarg.0
  IL_0007: stloc.1
  IL_0008: ldloca.s class V_0
  IL_000a: ldloc.1
  IL_000b: stfld int32 Program/Program+<>c__AsyncStateMachine0::_value
  IL_0010: ldc.i4.m1
  IL_0011: stloc.2
  IL_0012: ldloca.s class V_0
  IL_0014: ldloc.2
  IL_0015: stfld int32 Program/Program+<>c__AsyncStateMachine0::_state
  IL_001a: call [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0> class [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Int32>::Create()
  IL_001f: stloc.3
  IL_0020: ldloca.s class V_0
  IL_0022: ldloc.3
  IL_0023: stfld class [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0> Program/Program+<>c__AsyncStateMachine0::_builder
  IL_0028: ldloca.s class V_0
  IL_002a: ldflda class [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0> Program/Program+<>c__AsyncStateMachine0::_builder
  IL_002f: ldloca.s class V_0
  IL_0031: call instance System.Void class valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1::Start<Program/Program+<>c__AsyncStateMachine0>(!!0&)
  IL_0036: ldloca.s class V_0
  IL_0038: ldflda class [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0> Program/Program+<>c__AsyncStateMachine0::_builder
  IL_003d: call instance [System.Private.CoreLib]System.Threading.Tasks.Task`1<!0> class [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Int32>::get_Task()
  IL_0042: ret
}

.class nested private sequential ansi sealed Program+<>c__AsyncStateMachine0 extends [System.Private.CoreLib]System.ValueType implements [System.Private.CoreLib]System.Runtime.CompilerServices.IAsyncStateMachine
{
  .field int32 _state
  .field int32 _value
  .field class [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0> _builder
  .field valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter '<>awaiter0'

  .method hidebysig newslot virtual final instance default void MoveNext() cil managed
  {
    IL_00bb: ldarg.0
    IL_00bc: ldflda class [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0> Program/Program+<>c__AsyncStateMachine0::_builder
    IL_00c1: ldarg.0
    IL_00c2: ldfld int32 Program/Program+<>c__AsyncStateMachine0::_value
    IL_00c7: call instance System.Void class valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1::SetResult(var)
    IL_00cc: leave IL_0118
  }
}
