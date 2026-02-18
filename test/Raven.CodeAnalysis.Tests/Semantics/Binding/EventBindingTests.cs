using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class EventBindingTests : DiagnosticTestBase
{
    [Fact]
    public void InvokingEventOutsideContainingType_ReportsDiagnostic()
    {
        const string code = """
class Button {
    public event Clicked: System.Action;
}

func Use(button: Button) -> unit {
    button.Clicked();
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV0201").WithAnySpan().WithArguments("Clicked")]);

        verifier.Verify();
    }

    [Fact]
    public void AssigningEventWithEquals_ReportsDiagnostic()
    {
        const string code = """
class Button {
    public event Clicked: System.Action;

    public Assign(handler: System.Action) -> unit {
        Clicked = handler;
    }
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV0201").WithAnySpan().WithArguments("Clicked")]);

        verifier.Verify();
    }

    [Fact]
    public void InvokingCustomEvent_ReportsDiagnostic()
    {
        const string code = """
class Button {
    public event Clicked: System.Action {
        add { }
        remove { }
    }

    public Fire() -> unit {
        Clicked();
    }
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV0202").WithAnySpan().WithArguments("Clicked")]);

        verifier.Verify();
    }
}
