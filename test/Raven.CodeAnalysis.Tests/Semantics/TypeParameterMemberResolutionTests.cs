using System;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics;

public class TypeParameterMemberResolutionTests
{
    [Fact]
    public void UnconstrainedTypeParameter_ResolvesObjectMembers()
    {
        const string source = """
import System.*

func format<T>(value: T) -> string {
    return value.ToString()
}
""";

        var syntaxTree = SyntaxTree.ParseText(source, path: "type-parameter-object-members.rav");
        var compilation = Compilation.Create(
                "type-parameter-object-members",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);
    }

    [Fact]
    public void StaticAbstractInterfaceMembers_AreAvailableOnTypeParameters()
    {
        const string source = """
import System.*

func parse<T: IParsable<T>>(text: string) -> T {
    return T.Parse(text, null)
}
""";

        var syntaxTree = SyntaxTree.ParseText(source, path: "type-parameter-static-abstract.rav");
        var compilation = Compilation.Create(
                "type-parameter-static-abstract",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
    }
}
