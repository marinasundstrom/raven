﻿using Raven;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using static Raven.AppHostBuilder;

// ravc test.rav [-o test.exe]
// dotnet run -- test.rav [-o test.exe]

var filePath = args.Length > 0 ? args[0] : "../../../test.rav";
filePath = Path.GetFullPath(filePath);

using var file = File.OpenRead(filePath);
var sourceText = SourceText.From(file);

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText, filePath: filePath);
var root = syntaxTree.GetRoot();

root.PrintSyntaxTree(includeNames: true, includeTokens: true, includeTrivia: true, includeSpans: false, includeLocation: true);

var assemblyName = Path.GetFileNameWithoutExtension(filePath);

var refAssembliesPath = ReferenceAssemblyPaths.GetReferenceAssemblyDir();

var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
    .AddSyntaxTrees(syntaxTree)
    .AddReferences([
        MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
        MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
    ])
    .AnalyzeCodeTemp(); // Temporary

// INFO: The sample will compile, but not all constructs are supported yet.
using (var stream = File.OpenWrite($"{compilation.AssemblyName}.dll"))
{
    var result = compilation.Emit(stream);
    result.Print();
}

//CreateAppHost(compilation);

ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Dark;

Console.WriteLine();
Console.WriteLine(root.WriteNodeToText(compilation));