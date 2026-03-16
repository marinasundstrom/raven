using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MissingReturnTypeAnnotationAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void MethodWithoutAnnotation_SuggestsInferredReturnType()
    {
        const string code = """
class C {
    func Test() {
        return 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [
                new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId)
                    .WithSpan(2, 10, 2, 14)
                    .WithArguments("Test", "int")
            ],
            disabledDiagnostics: ["RAV1503", CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void FunctionStatementWithoutAnnotation_SuggestsInferredReturnType()
    {
        const string code = """
func Test() {
    return 1
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [
                new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId)
                    .WithSpan(1, 6, 1, 10)
                    .WithArguments("Test", "int")
            ],
            disabledDiagnostics: ["RAV1503", CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodWithoutAnnotation_WithMultipleReturnTypes_SuggestsUnion()
    {
        const string code = """
class C {
    func Test(flag: bool) {
        if flag {
            return 1
        } else {
            return true
        }
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [
                new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId)
                    .WithSpan(2, 10, 2, 14)
                    .WithArguments("Test", "ValueType")
            ],
            disabledDiagnostics: ["RAV1503", CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodWithoutAnnotation_WithImplicitlyConvertibleReturnTypes_SuggestsCommonType()
    {
        const string code = """
class C {
    func GetInt() -> int => 1
    func GetLong() -> long => 2

    func Test(flag: bool) {
        if flag {
            return GetInt()
        } else {
            return GetLong()
        }
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [
                new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId)
                    .WithSpan(5, 10, 5, 14)
                    .WithArguments("Test", "ValueType")
            ],
            disabledDiagnostics: ["RAV1503", CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodWithoutAnnotation_WhenOnlyCommonTypeIsObject_SuggestsUnion()
    {
        const string code = """
class C {
    func Test(flag: bool) {
        if flag {
            return 1
        } else {
            return "x"
        }
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [
                new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId)
                    .WithSpan(2, 10, 2, 14)
                    .WithArguments("Test", "IComparable")
            ],
            disabledDiagnostics: ["RAV1503", CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodReturningUnitWithoutAnnotation_NoDiagnostic()
    {
        const string code = """
class C {
    func Test() {
        return ()
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1503", CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodWithBranchesReturningVoid_NoDiagnostic()
    {
        const string code = """
class C {
    func Test(input: object) {
        if input is int a {
            System.Console.WriteLine(a)
        } else if input is bool b {
            System.Console.WriteLine(b)
        }
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1503", CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodWithOnlyAssignmentStatement_NoDiagnostic()
    {
        const string code = """
class C {
    var name: string = "x"

    func Anonymize() {
        name = ""
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1503", CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodWithInvalidAssignmentStatement_NoReturnTypeDiagnostic()
    {
        const string code = """
class C {
    val name: string = "x"

    func Anonymize() {
        name = ""
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [],
            disabledDiagnostics: [
                "RAV1503",
                "RAV0200",
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id,
                CompilerDiagnostics.ReadOnlyFieldCannotBeAssignedTo.Id
            ]);

        verifier.Verify();
    }

    [Fact]
    public void OverrideMethodReturningUnit_DoesNotSuggestTrailingExpressionType()
    {
        const string code = """
class DbContextOptionsBuilder {
    func UseSqlite(connectionString: string) -> DbContextOptionsBuilder {
        return self
    }
}

abstract class DbContext {
    protected abstract func OnConfiguring(optionsBuilder: DbContextOptionsBuilder) -> unit
}

class AppDbContext : DbContext {
    protected override func OnConfiguring(optionsBuilder: DbContextOptionsBuilder) {
        optionsBuilder.UseSqlite("Data Source=app.db")
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
