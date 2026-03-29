using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TargetTypedExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void TargetTypedMemberAccess_UsesParameterType()
    {
        string testCode = """
class Bar {
    init() {}
    static val Instance: Bar {
        get { return Bar() }
    }
}

class Program {
    static func Consume(x: Bar) -> unit {}
    static func Run() -> unit {
        Consume(.Instance)
    }
}
""";
        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void TargetTypedMethodBinding_UsesAssignmentType()
    {
        string testCode = """
class Program {
    static func Run() -> unit {
        val number: int = .Parse("42")
    }
}
""";
        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void TargetTypedMethodBinding_UsesAssignmentStatementType()
    {
        string testCode = """
enum Color { Red, Blue }

class Program {
    static func Run() -> unit {
        var color: Color = .Red
        color = .Blue
    }
}
""";
        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void TargetTypedMemberAccess_ParticipatesInOverloadResolution()
    {
        string testCode = """
import System.*
import System.Reflection.*

class Program {
    static func Run() -> unit {
        val members = typeof(System.Object).GetMembers(.NonPublic)
    }
}
""";
        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void TargetTypedMemberBinding_AllowsBinaryOperators()
    {
        string testCode = """
import System.Reflection.*

class Program {
    static func Use(flags: BindingFlags) -> unit {}

    static func Run() -> unit {
        val flags: BindingFlags = .NonPublic | .Static
        Use(.Public & .Static)
    }
}
""";
        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void TargetTypedMemberBinding_UsesConstructorParameterType()
    {
        string testCode = """
enum Value { A, B }

class Item {
    init(value: Value) {}
}

class Program {
    static func Run() -> unit {
        val item = Item(.A)
    }
}
""";
        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void TargetTypedMemberBinding_BindsStaticProperty_ForReferenceTypeParameter()
    {
        string testCode = """
import System.Text.*

class Program {
    static func Use(value: Encoding) -> unit {}

    static func Run() -> unit {
        Use(.UTF8)
    }
}
""";
        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void TargetTypedMemberBinding_BindsStaticProperty_ForNullableReferenceTypeParameter()
    {
        string testCode = """
import System.Text.*

class Program {
    static func Use(value: Encoding?) -> unit {}

    static func Run() -> unit {
        Use(.UTF8)
    }
}
""";
        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }
}
