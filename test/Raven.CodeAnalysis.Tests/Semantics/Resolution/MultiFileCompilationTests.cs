using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MultiFileCompilationTests : CompilationTestBase
{
    [Fact]
    public void CrossFileTypeReferences_ShouldBindWithoutErrors()
    {
        var a = SyntaxTree.ParseText("""
        namespace Sample {
            public class A {
                val b: B = B()
                val nested: B.Nested = B.Nested()
            }
        }
        """, path: "a.rav");

        var b = SyntaxTree.ParseText("""
        namespace Sample {
            public class B {
                public class Nested {
                }

                val a: A = A()
            }
        }
        """, path: "b.rav");

        var c = SyntaxTree.ParseText("""
        namespace Sample {
            public class C {
                val nested: B.Nested = B.Nested()
            }
        }
        """, path: "c.rav");

        var compilation = CreateCompilation([a, b, c], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }
}
