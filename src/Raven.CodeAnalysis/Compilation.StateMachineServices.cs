namespace Raven.CodeAnalysis;

public partial class Compilation : StateMachineServices
{
    bool StateMachineServices.UseHeapAsyncStateMachines => Options.UseHeapAsyncStateMachines;

    bool StateMachineServices.CaptureAsyncExceptions => Options.CaptureAsyncExceptions;
}
