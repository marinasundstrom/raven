using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;

using TypeUnionAnalyzer;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class TypeUnionAnalyzerNullTests
{
    private static async Task<ImmutableArray<Microsoft.CodeAnalysis.Diagnostic>> GetDiagnosticsAsync(string source)
    {
        var tree = CSharpSyntaxTree.ParseText(source);
        var references = new[] { Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
        var compilation = CSharpCompilation.Create(
            "Test",
            new[] { tree },
            references,
            new CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));

        var analyzer = new TypeUnionParameterAnalyzer();
        var analyzers = ImmutableArray.Create<DiagnosticAnalyzer>(analyzer);
        return await compilation.WithAnalyzers(analyzers).GetAnalyzerDiagnosticsAsync();
    }

    [Fact]
    public async Task PassingNullWithoutNullType_ReportsDiagnostic()
    {
        var source = @"
using System;
class Null {}
[AttributeUsage(AttributeTargets.Parameter)]
class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params Type[] types) {} }
class C {
    static void M([TypeUnion(typeof(string))] object p) {}
    static void Test() { M(null); }
}
";
        var diagnostics = await GetDiagnosticsAsync(source);
        Assert.Contains(diagnostics, d => d.Id == "TU002");
    }

    [Fact]
    public async Task PassingNullWithNullType_NoDiagnostic()
    {
        var source = @"
using System;
class Null {}
[AttributeUsage(AttributeTargets.Parameter)]
class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params Type[] types) {} }
class C {
    static void M([TypeUnion(typeof(string), typeof(Null))] object p) {}
    static void Test() { M(null); }
}
";
        var diagnostics = await GetDiagnosticsAsync(source);
        Assert.DoesNotContain(diagnostics, d => d.Id == "TU002");
    }

    [Fact]
    public async Task ParameterDiagnostic_ListsNullType()
    {
        var source = @"
using System;
class Null {}
[AttributeUsage(AttributeTargets.Parameter)]
class TypeUnionAttribute : Attribute { public TypeUnionAttribute(params Type[] types) {} }
class C {
    static void M([TypeUnion(typeof(string), typeof(Null))] object p) {}
}
";
        var diagnostics = await GetDiagnosticsAsync(source);
        var info = Assert.Single(diagnostics.Where(d => d.Id == "TU001"));
        Assert.Contains("'null'", info.GetMessage());
    }
}
