using Raven.CodeAnalysis.Syntax;

using Cecil = Mono.Cecil;
using CSApi = Microsoft.CodeAnalysis;
using CSharp = Microsoft.CodeAnalysis.CSharp;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MalformedTupleMetadataTests
{
    [Theory]
    [InlineData("valid")]
    [InlineData("missing")]
    [InlineData("excess")]
    [InlineData("padding")]
    [InlineData("null")]
    [InlineData("empty")]
    [InlineData("truncated")]
    public void DotNet_TupleAttributeDecodingAgreesWithCSharp(string shape)
    {
        var references = TestMetadataReferences.Default.OfType<PortableExecutableReference>().ToArray();
        var csReferences = references.Select(r => CSApi.MetadataReference.CreateFromFile(r.FilePath!)).ToArray();
        var image = CreateLibrary(shape, csReferences);
        var reference = MetadataReference.CreateFromImage(image);
        var options = new CSharp.CSharpCompilationOptions(CSApi.OutputKind.DynamicallyLinkedLibrary);
        foreach (var access in new[] { "eighth", "Rest.Item1", "discard" })
        {
            var ravenSource = access == "discard" ? "class Probe { static func Read() { Vendor.Get() } }"
                : $"class Probe {{ static func Read() -> int => Vendor.Get().{access} }}";
            var csSource = access == "discard" ? "class Probe { static void Read(){ Vendor.Get(); } }"
                : $"class Probe {{ static int Read() => Vendor.Get().{access}; }}";
            var raven = Compilation.Create("Consumer", [SyntaxTree.ParseText(ravenSource)], [.. references, reference],
                CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            var csharp = CSharp.CSharpCompilation.Create("Consumer", [CSharp.CSharpSyntaxTree.ParseText(csSource)],
                [.. csReferences, CSApi.MetadataReference.CreateFromImage(image)], options);
            var ravenErrors = raven.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            var csErrors = csharp.GetDiagnostics().Where(d => d.Severity == CSApi.DiagnosticSeverity.Error).ToArray();

            if (shape is "missing" or "excess" or "empty")
            {
                Assert.Contains(ravenErrors, d => d.Id == "RAV0384");
                Assert.Contains(csErrors, d => d.Id == "CS0570");
            }
            else if (access == "eighth" && shape is ("null" or "truncated"))
            {
                Assert.NotEmpty(ravenErrors);
                Assert.DoesNotContain(ravenErrors, d => d.Id == "RAV0384");
                Assert.Contains(csErrors, d => d.Id == "CS1061");
            }
            else
            {
                Assert.Empty(ravenErrors);
                Assert.Empty(csErrors);
            }
        }
    }

    private static byte[] CreateLibrary(string shape, CSApi.MetadataReference[] references)
    {
        const string source = """
            public static class Vendor {
                public static (int a,int b,int c,int d,int e,int f,int g,int eighth) Get() => default;
            }
            """;
        var library = CSharp.CSharpCompilation.Create("ShapeLibrary", [CSharp.CSharpSyntaxTree.ParseText(source)], references,
            new CSharp.CSharpCompilationOptions(CSApi.OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        Assert.True(library.Emit(stream).Success);
        stream.Position = 0;
        using var module = Cecil.ModuleDefinition.ReadModule(stream);
        var method = module.GetType("Vendor").Methods.Single(m => m.Name == "Get");
        var attributes = method.MethodReturnType.CustomAttributes;
        var attribute = attributes.Single(a => a.AttributeType.Name == "TupleElementNamesAttribute");
        if (shape == "truncated")
        {
            attributes.Remove(attribute);
            attributes.Add(new Cecil.CustomAttribute(attribute.Constructor, [1, 0, 2, 0]));
        }
        else
        {
            var argument = attribute.ConstructorArguments[0];
            var values = ((Cecil.CustomAttributeArgument[])argument.Value).ToList();
            switch (shape)
            {
                case "missing": values.RemoveAt(values.Count - 1); break;
                case "excess": values.Add(new Cecil.CustomAttributeArgument(module.TypeSystem.String, null)); break;
                case "padding": values[^1] = new Cecil.CustomAttributeArgument(module.TypeSystem.String, "ignored"); break;
                case "empty": values.Clear(); break;
            }

            attribute.ConstructorArguments[0] = new Cecil.CustomAttributeArgument(argument.Type, shape == "null" ? null : values.ToArray());
        }

        using var output = new MemoryStream();
        module.Write(output);
        return output.ToArray();
    }
}
