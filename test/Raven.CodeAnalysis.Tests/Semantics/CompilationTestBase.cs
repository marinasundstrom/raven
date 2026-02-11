using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public abstract class CompilationTestBase
{
    protected virtual MetadataReference[] GetMetadataReferences()
        => TestMetadataReferences.Default;

    protected virtual CompilationOptions GetCompilationOptions()
        => new(OutputKind.DynamicallyLinkedLibrary);

    protected Compilation CreateCompilation(CompilationOptions? options = null, MetadataReference[]? references = null, string assemblyName = "test")
        => Compilation.Create(assemblyName, Array.Empty<SyntaxTree>(), references ?? GetMetadataReferences(), options ?? GetCompilationOptions());

    protected Compilation CreateCompilation(SyntaxTree tree, CompilationOptions? options = null, MetadataReference[]? references = null, string assemblyName = "test")
        => Compilation.Create(assemblyName, [tree], references ?? GetMetadataReferences(), ResolveCompilationOptions([tree], options));

    protected Compilation CreateCompilation(IEnumerable<SyntaxTree> trees, CompilationOptions? options = null, MetadataReference[]? references = null, string assemblyName = "test")
    {
        var syntaxTrees = trees.ToArray();
        return Compilation.Create(assemblyName, syntaxTrees, references ?? GetMetadataReferences(), ResolveCompilationOptions(syntaxTrees, options));
    }

    protected (Compilation Compilation, SyntaxTree Tree) CreateCompilation(string source, CompilationOptions? options = null, MetadataReference[]? references = null, string assemblyName = "test")
    {
        var tree = SyntaxTree.ParseText(source);
        return (CreateCompilation(tree, options, references, assemblyName), tree);
    }

    private CompilationOptions ResolveCompilationOptions(IReadOnlyList<SyntaxTree> trees, CompilationOptions? options)
    {
        if (options is not null)
            return options;

        foreach (var tree in trees)
        {
            if (HasGlobalStatements(tree))
                return new CompilationOptions(OutputKind.ConsoleApplication);
        }

        return GetCompilationOptions();
    }

    private static bool HasGlobalStatements(SyntaxTree tree)
        => tree.GetRoot().Members.OfType<GlobalStatementSyntax>().Any();
}
