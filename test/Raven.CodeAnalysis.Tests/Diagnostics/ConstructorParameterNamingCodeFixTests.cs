using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class ConstructorParameterNamingCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void ConstructorParameterRename_UpdatesDeclarationAndBodyReferences()
    {
        const string code = """
class Foo {
    init(Name: int) {
        val doubled = Name + Name
    }
}
""";

        const string fixedCode = """
class Foo {
    init(name: int) {
        val doubled = name + name
    }
}
""";

        var verifier = CreateCodeFixVerifier<ConstructorParameterNamingAnalyzer, ConstructorParameterNamingCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(ConstructorParameterNamingAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructorPromotedParameterRename_UpdatesTypeMemberReferences()
    {
        const string code = """
class Foo(var name: string) {
    func GetName() -> string => name
}
""";

        const string fixedCode = """
class Foo(var Name: string) {
    func GetName() -> string => Name
}
""";

        var verifier = CreateCodeFixVerifier<ConstructorParameterNamingAnalyzer, ConstructorParameterNamingCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(ConstructorParameterNamingAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
