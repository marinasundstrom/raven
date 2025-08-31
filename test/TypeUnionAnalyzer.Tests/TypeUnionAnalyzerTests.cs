using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;

using TypeUnionAnalyzer;

using Xunit;

namespace TypeUnionAnalyzer.Tests;

public class TypeUnionAnalyzerTests
{
    private static async Task<ImmutableArray<Diagnostic>> GetDiagnosticsAsync(string source, params MetadataReference[] additionalReferences)
    {
        var tree = CSharpSyntaxTree.ParseText(source);
        var references = new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) }
            .Concat(additionalReferences);
        var compilation = CSharpCompilation.Create(
            "Test",
            new[] { tree },
            references,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var analyzers = ImmutableArray.Create<DiagnosticAnalyzer>(new TypeUnionParameterAnalyzer());
        return await compilation.WithAnalyzers(analyzers).GetAnalyzerDiagnosticsAsync();
    }

    [Fact]
    public async Task PassingNullWithoutNullType_ReportsDiagnostic()
    {
        var source = @"
using System;
class Null {}
[AttributeUsage(AttributeTargets.Parameter | AttributeTargets.ReturnValue)]
class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params Type[] types) {} }
class C {
    static void M([TypeUnion(typeof(string))] object p) {}
    static void Test() { M(null); }
}";
        var diagnostics = await GetDiagnosticsAsync(source);
        Assert.Contains(diagnostics, d => d.Id == "TU002");
    }

    [Fact]
    public async Task PassingNullWithNullType_NoDiagnostic()
    {
        var source = @"
using System;
class Null {}
[AttributeUsage(AttributeTargets.Parameter | AttributeTargets.ReturnValue)]
class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params Type[] types) {} }
class C {
    static void M([TypeUnion(typeof(string), typeof(Null))] object p) {}
    static void Test() { M(null); }
}";
        var diagnostics = await GetDiagnosticsAsync(source);
        Assert.DoesNotContain(diagnostics, d => d.Id == "TU002");
    }

    [Fact]
    public async Task ParameterDiagnostic_ListsNullType()
    {
        var source = @"
using System;
class Null {}
[AttributeUsage(AttributeTargets.Parameter | AttributeTargets.ReturnValue)]
class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params Type[] types) {} }
class C {
    static void M([TypeUnion(typeof(string), typeof(Null))] object p) {}
}";
        var diagnostics = await GetDiagnosticsAsync(source);
        var info = Assert.Single(diagnostics.Where(d => d.Id == "TU001"));
        Assert.Contains("'null'", info.GetMessage());
    }

    [Fact]
    public async Task ExternalMethodWithNonObjectParameter_ReportsDiagnostic()
    {
        var libSource = @"
using System;
[AttributeUsage(AttributeTargets.Parameter)]
public class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params Type[] types) {} }
public static class External {
    public static void M([TypeUnion(typeof(int))] int p) {}
}";
        var libTree = CSharpSyntaxTree.ParseText(libSource);
        var libCompilation = CSharpCompilation.Create(
            "Lib",
            new[] { libTree },
            new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) },
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var ms = new MemoryStream();
        var emitResult = libCompilation.Emit(ms);
        Assert.True(emitResult.Success);
        ms.Seek(0, SeekOrigin.Begin);
        var libRef = MetadataReference.CreateFromStream(ms);

        var testSource = @"class C { static void Test() { External.M(1); } }";
        var diagnostics = await GetDiagnosticsAsync(testSource, libRef);
        Assert.Contains(diagnostics, d => d.Id == "TU005");
    }

    [Fact]
    public async Task VariableDeclarationAndUsage_ReportInfoDiagnostics()
    {
        var source = @"
using System;
[AttributeUsage(AttributeTargets.ReturnValue | AttributeTargets.Parameter)]
class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params Type[] types) {} }
class C {
    [return: TypeUnion(typeof(int), typeof(string))]
    static object M() => 1;
    static void Test() {
        object x = M();
        var y = x;
        Console.WriteLine(y);
    }
}";
        var diagnostics = await GetDiagnosticsAsync(source);
        Assert.Contains(diagnostics, d => d.Id == "TU001" && d.GetMessage().Contains("Variable 'x'"));
        Assert.Contains(diagnostics, d => d.Id == "TU001" && d.GetMessage().Contains("Variable 'y'"));
    }

    [Fact]
    public async Task LiteralValueUnion_DisplaysValuesAndChecksCompatibility()
    {
        var source = @"
using System;
[AttributeUsage(AttributeTargets.Parameter)]
class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params object[] types) {} }
class C {
    static void M([TypeUnion(""yes"", ""no"")] object p) {}
    static void Test() { M(""yes""); M(""maybe""); }
}";
        var diagnostics = await GetDiagnosticsAsync(source);
        Assert.Contains(diagnostics.Where(d => d.Id == "TU001"), d => d.GetMessage().Contains("\"yes\"") && d.GetMessage().Contains("\"no\""));
        Assert.Single(diagnostics.Where(d => d.Id == "TU002"));
    }
}
