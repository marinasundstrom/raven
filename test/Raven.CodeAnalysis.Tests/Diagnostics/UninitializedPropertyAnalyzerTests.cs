using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UninitializedPropertyAnalyzerTests : AnalyzerTestBase
{
    [Theory]
    [InlineData("init() { Name = \"ok\" }\ninit(value: int) { Name = \"other\" }", false)]
    [InlineData("init() { Name = \"ok\" }\ninit(value: int) { }", true)]
    [InlineData("init(value: bool) { if value { Name = \"ok\" } }", true)]
    [InlineData("init(value: bool) { if value { Name = \"ok\" } else { Name = \"other\" } }", false)]
    [InlineData("init(value: bool) { if value { return }\nName = \"ok\" }", true)]
    [InlineData("init(value: bool) { if value { throw System.Exception() }\nName = \"ok\" }", false)]
    [InlineData("init() { let action = () => { Name = \"ok\" } }", true)]
    [InlineData("init(other: C) { other.Name = \"ok\" }", true)]
    [InlineData("init(value: bool) { while value { Name = \"ok\" } }", true)]
    [InlineData("init() { try { return } finally { Name = \"ok\" } }", false)]
    [InlineData("init { Name = \"ok\" }\ninit(value: int) { }", false)]
    [InlineData("static init { }\ninit() { }", true)]
    [InlineData("init(value: bool) { loop { if value { Name = \"ok\"; break } else { throw System.Exception() } } }", false)]
    [InlineData("init(value: bool) { loop { if value { break }\nName = \"ok\" } }", true)]
    [InlineData("init(value: bool) { match value { true => { Name = \"yes\" }, false => { Name = \"no\" } } }", false)]
    [InlineData("init() { try { Name = \"ok\" } catch { } }", true)]
    [InlineData("init() { try { Name = \"ok\" } catch { Name = \"fallback\" } }", false)]
    [InlineData("init { if System.Environment.TickCount > 0 { Name = \"ok\" } }\ninit(value: int) { Name = \"ok\" }", true)]
    public void InitializationRequiresEveryNormalConstructorPath(string constructors, bool reportsDiagnostic)
    {
        var code = "class C {\n    var Name: string { get; set; }\n    " + constructors + "\n}";
        var expected = reportsDiagnostic
            ? new[] { new DiagnosticResult(UninitializedPropertyAnalyzer.DiagnosticId)
                .WithSpan(2, 9, 2, 13).WithArguments("Name") }
            : [];
        CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(code,
            expectedDiagnostics: expected,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]).Verify();
    }

    [Theory]
    [InlineData("init(value: char) { stored = value }", false)]
    [InlineData("init(value: char) { stored = value }\ninit() { }", true)]
    [InlineData("init(value: char, assign: bool) { if assign { stored = value } }", true)]
    public void PrivateMutableStorage_UsesInitializationDiagnostic(string constructors, bool reportsDiagnostic)
    {
        var code = "class CharacterBox {\n    private var stored: char\n" + constructors +
            "\n    val Value: char => stored\n}";
        var expected = reportsDiagnostic
            ? new[] { new DiagnosticResult(UninitializedPropertyAnalyzer.DiagnosticId)
                .WithSpan(2, 17, 2, 23).WithArguments("stored") }
            : [];
        CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(code,
            expectedDiagnostics: expected,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]).Verify();
    }

    [Theory]
    [InlineData("Name = \"other\"", false)]
    [InlineData("", true)]
    public void PartialTypeChecksConstructorsInEveryPart(string otherBody, bool reportsDiagnostic)
    {
        var code = "partial class C {\n    var Name: string { get; set; }\n    init() { Name = \"ok\" }\n}\n" +
            "partial class C { init(value: int) { " + otherBody + " } }";
        var expected = reportsDiagnostic
            ? new[] { new DiagnosticResult(UninitializedPropertyAnalyzer.DiagnosticId)
                .WithSpan(2, 9, 2, 13).WithArguments("Name") }
            : [];
        CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(code,
            expectedDiagnostics: expected,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]).Verify();
    }

    [Fact]
    public void PropertyWithoutInitializerOrConstructorAssignment_ReportsDiagnostic()
    {
        const string code = """
class C {
    var Name: string { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [
                new DiagnosticResult(UninitializedPropertyAnalyzer.DiagnosticId)
                    .WithSpan(2, 9, 2, 13)
                    .WithArguments("Name")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateStoredValPropertyWithoutInitializerOrConstructorAssignment_ReportsDiagnostic()
    {
        const string code = """
class UiStackPanel {
}

class UiWindow {
    private val title: string

    init(content: UiStackPanel, title: string) {
        Content = content
    }

    val Content: UiStackPanel
    val Title: string => title
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [
                new DiagnosticResult(UninitializedPropertyAnalyzer.DiagnosticId)
                    .WithSpan(5, 17, 5, 22)
                    .WithArguments("title")
            ],
            disabledDiagnostics: [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id,
                UnusedParameterAnalyzer.DiagnosticId
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateStoredValPropertyAssignedInConstructor_NoDiagnostic()
    {
        const string code = """
class UiWindow {
    private val title: string

    init(title: string) {
        self.title = title
    }

    val Title: string => title
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id,
                UnusedParameterAnalyzer.DiagnosticId
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyWithInitializer_NoDiagnostic()
    {
        const string code = """
class C {
    var Name: string { get; set; } = "ok"
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyAssignedInConstructor_NoDiagnostic()
    {
        const string code = """
class C {
    var Name: string { get; set; }

    init() {
        Name = "ok"
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void RequiredProperty_NoDiagnostic()
    {
        const string code = """
class C {
    required var Name: string { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyWithExplicitGetterImplementation_NoDiagnostic()
    {
        const string code = """
class C {
    val Name: string {
        get => "ok"
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyWithExplicitGetterBlockImplementation_NoDiagnostic()
    {
        const string code = """
class C {
    val Name: string {
        get {
            "ok"
        }
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyAssignedInPrimaryInitializerBlock_NoDiagnostic()
    {
        const string code = """
class C(name: string) {
    {
        Score = 1
    }

    var Score: int { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
