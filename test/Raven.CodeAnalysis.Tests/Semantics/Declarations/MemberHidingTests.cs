using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MemberHidingTests : DiagnosticTestBase
{
    [Fact]
    public void MethodHidingWithoutNew_ReportsWarning()
    {
        const string source = """
open class Base {
    public Speak() -> unit {
        return
    }
}

class Derived : Base {
    public Speak() -> unit {
        return
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.MemberHidesInheritedMember.Id)
                    .WithAnySpan()
                    .WithArguments("Derived.Speak", "Base.Speak")
                    .WithSeverity(DiagnosticSeverity.Warning)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void FieldHidingWithoutNew_ReportsWarning()
    {
        const string source = """
open class Base {
    public val Value: int = 1;
}

class Derived : Base {
    public val Value: int = 2;
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.MemberHidesInheritedMember.Id)
                    .WithAnySpan()
                    .WithArguments("Derived.Value", "Base.Value")
                    .WithSeverity(DiagnosticSeverity.Warning)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MethodHidingWithNew_DoesNotReportWarning()
    {
        const string source = """
open class Base {
    public Speak() -> unit {
        return
    }
}

class Derived : Base {
    public new Speak() -> unit {
        return
    }
}
""";

        var verifier = CreateVerifier(source);

        verifier.Verify();
    }
}
