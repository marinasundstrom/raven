using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExternSemanticTests : CompilationTestBase
{
    [Fact]
    public void ExternInstanceMethod_ReportsDiagnostic()
    {
        const string source = """
class Native {
    extern Sleep(milliseconds: int) -> unit;
}
""";

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, _) = CreateCompilation(source, options);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.ExternMemberMustBeStatic.Id);
    }

    [Fact]
    public void ExternFunctionWithBody_ReportsDiagnostic()
    {
        const string source = """
extern func NativeSleep(milliseconds: int) -> unit {
    return
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.ExternMemberCannotHaveBody.Id);
    }

    [Fact]
    public void ExternFunctionSymbol_IsMarkedExtern()
    {
        const string source = "extern func NativeSleep(milliseconds: int) -> unit;";
        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options);
        var model = compilation.GetSemanticModel(tree);
        var function = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(function));
        Assert.True(symbol.IsExtern);
    }

    [Fact]
    public void ExternDllImportMethod_EmitsPInvokeImpl()
    {
        const string source = """
import System.Runtime.InteropServices.*

class Native {
    [DllImport("kernel32", EntryPoint: "GetTickCount")]
    public extern static GetTickCount() -> uint;
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("native", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var nativeType = loaded.Assembly.GetType("Native", throwOnError: true)!;
        var method = nativeType.GetMethod("GetTickCount", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(method);
        Assert.True(method!.Attributes.HasFlag(MethodAttributes.PinvokeImpl));
    }
}
