.method private hidebysig static default [System.Runtime]System.Threading.Tasks.Task`1<int32> Test(int32 'value') cil managed
{
  .locals init(class Program/<Test>d__0 V_0)
  IL_0000: newobj instance void class Program/<Test>d__0::.ctor()
  IL_0005: stloc.0
  IL_0006: ldarg.0
  IL_0007: stfld int32 Program/<Test>d__0::value
  IL_000c: ldloca.s class V_0
  IL_000e: ldc.i4.m1
  IL_000f: stfld int32 Program/<Test>d__0::'<>1__state'
  IL_0014: ldflda class [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32> Program/<Test>d__0::'<>t__builder'
  IL_0019: call instance System.Void class [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32>::Start<class Program/<Test>d__0>(!!0&)
  IL_001e: ldflda class [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32> Program/<Test>d__0::'<>t__builder'
  IL_0023: call instance class [System.Runtime]System.Threading.Tasks.Task`1<int32> class [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32>::get_Task()
  IL_0028: ret
}

.class nested private auto ansi sealed beforefieldinit Program/<Test>d__0 extends [System.Runtime]System.Object implements [System.Runtime]System.Runtime.CompilerServices.IAsyncStateMachine
{
  .field public int32 'value'
  .field public int32 '<>1__state'
  .field public class [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32> '<>t__builder'
  .field public valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter '<>u__1'

  .method private hidebysig newslot virtual final instance default void MoveNext() cil managed
  {
    IL_0065: ldloca.s class V_2
    IL_0067: call instance void class valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter::GetResult()
    IL_006d: ldarg.0
    IL_006e: ldfld int32 Program/<Test>d__0::value
    IL_0073: stloc.1
    IL_0090: ldarg.0
    IL_0091: ldc.i4.s -2
    IL_0093: stfld int32 Program/<Test>d__0::'<>1__state'
    IL_0098: ldarg.0
    IL_0099: ldflda class [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32> Program/<Test>d__0::'<>t__builder'
    IL_009e: ldloc.1
    IL_009f: call instance void class [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<int32>::SetResult(var)
    IL_00a4: ret
  }
}
