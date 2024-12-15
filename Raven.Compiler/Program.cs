using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

var fileName = args.Length > 0 ? args[0] : "../../../test.rav";

using var file = File.OpenRead(fileName);

var sourceText = SourceText.From(file);

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText);

var root = syntaxTree.GetRoot();

Console.WriteLine(root.GetSyntaxTreeRepresentation(includeTokens: true, includeTrivia: false));

Console.WriteLine();

Console.WriteLine(root.ToFullString());
