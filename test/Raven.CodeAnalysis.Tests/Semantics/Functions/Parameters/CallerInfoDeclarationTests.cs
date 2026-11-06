using Raven.CodeAnalysis.Syntax;

using CS = Microsoft.CodeAnalysis.CSharp;
using R = Microsoft.CodeAnalysis;
namespace Raven.CodeAnalysis.Tests;

public sealed class CallerInfoDeclarationTests
{
    public static IEnumerable<object[]> Cases()
    {
        (string Label, string CSharp, string Raven)[] cases = [
         ("required member","[CallerMemberName] string name","[CallerMemberName] name:string"),
 ("wrong line type","[CallerLineNumber] string line=\"\"","[CallerLineNumber] line:string=\"\""),
 ("wrong member type","[CallerMemberName] int member=0","[CallerMemberName] member:int=0"),
 ("invalid CAE sibling","int value, [CallerArgumentExpression(\"missing\")] string text=\"\"","value:int, [CallerArgumentExpression(\"missing\")] text:string=\"\""),
 ("self CAE sibling","int value, [CallerArgumentExpression(\"text\")] string text=\"\"","value:int, [CallerArgumentExpression(\"text\")] text:string=\"\""),
 ("required CAE","int value, [CallerArgumentExpression(\"value\")] string text","value:int, [CallerArgumentExpression(\"value\")] text:string"),
 ("caller precedence","[CallerMemberName,CallerFilePath,CallerLineNumber] object text=null","[CallerMemberName,CallerFilePath,CallerLineNumber] text:object?=null"),
 ("CAE vs member","int value, [CallerArgumentExpression(\"value\"),CallerMemberName] string text=\"\"","value:int, [CallerArgumentExpression(\"value\"),CallerMemberName] text:string=\"\""),
 ("nullable line","[CallerLineNumber] int? line=null","[CallerLineNumber] line:int?=null")
        ];
        string?[] ids = ["RAV0379", "RAV0380", "RAV0380", "RAV0381", "RAV0382", "RAV0379", "RAV0383", "RAV0383", null];
        string?[] csids = ["CS4022", "CS4017", "CS4019", "CS8963", "CS8965", "CS8964", "CS7081", "CS8962", null];
        for (var i = 0; i < cases.Length; i++) yield return [cases[i].CSharp, cases[i].Raven, ids[i]!, csids[i]!];
    }
    [Theory, MemberData(nameof(Cases)), Trait("Target", "DotNet")]
    public void DotNet_DeclarationMatchesCSharp(string csharp, string raven, string? id, string? csid)
    {
        var refs = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).Select(p => R.MetadataReference.CreateFromFile(p));
        var tree = CS.CSharpSyntaxTree.ParseText("using System.Runtime.CompilerServices; class Probe { static void Capture(" + csharp + ") {} }");
        var c = CS.CSharpCompilation.Create("CallerOracle", [tree], refs, new CS.CSharpCompilationOptions(R.OutputKind.DynamicallyLinkedLibrary));
        var oracle = c.GetDiagnostics().Where(d => d.Severity is R.DiagnosticSeverity.Error or R.DiagnosticSeverity.Warning).ToArray();
        if (csid is null) Assert.Empty(oracle); else Assert.Contains(oracle, d => d.Id == csid);
        var actual = Check(raven, id, TargetPlatform.DotNet);
        Assert.Equal(oracle.Count(d => d.Severity == R.DiagnosticSeverity.Error), actual.Count(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(oracle.Count(d => d.Severity == R.DiagnosticSeverity.Warning), actual.Count(d => d.Severity == DiagnosticSeverity.Warning));
    }
    [Theory, MemberData(nameof(Cases)), Trait("Target", "NeoCLR")]
    public void NeoCLR_DeclarationUsesSharedValidation(string csharp, string raven, string? id, string? csid) => Check(raven, id, TargetPlatform.NeoCLR);
    private static Diagnostic[] Check(string raven, string? id, TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("import System.Runtime.CompilerServices.*\nclass Probe { static func Capture(" + raven + ") {} }");
        var c = Compilation.Create("CallerDeclaration", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var diagnostics = c.GetSemanticModel(tree).GetDiagnostics().Where(d => d.Severity is DiagnosticSeverity.Warning or DiagnosticSeverity.Error).ToArray();
        if (id is null) Assert.Empty(diagnostics); else Assert.Contains(diagnostics, d => d.Id == id);
        var repeated = c.GetSemanticModel(tree).GetDiagnostics().Where(d => d.Severity is DiagnosticSeverity.Warning or DiagnosticSeverity.Error).ToArray();
        Assert.Equal(diagnostics.Select(d => d.ToString()).OrderBy(x => x), repeated.Select(d => d.ToString()).OrderBy(x => x));
        return diagnostics;
    }
}

public sealed class CallerParameterDeclarationFormsTests
{
    [Theory]
    [InlineData("class Probe { static func Capture([CallerMemberName] name:string) {} }")]
    [InlineData("class Probe { init([CallerMemberName] name:string) {} }")]
    [InlineData("func Capture([CallerMemberName] name:string) {}")]
    [InlineData("class Probe { static func Run() { func Capture([CallerMemberName] name:string) {} } }")]
    [InlineData("class Probe([CallerMemberName] name:string) {}")]
    [Trait("Target", "DotNet")]
    public void RequiredCallerParameterIsRejectedAcrossDeclarations(string declaration)
    {
        var tree = SyntaxTree.ParseText("import System.Runtime.CompilerServices.*\n" + declaration);
        var c = Compilation.Create("CallerForms", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(c.GetDiagnostics(), d => d.Id == "RAV0379");
    }
}
