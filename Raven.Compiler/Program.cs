using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

var fileName = args.Length > 0 ? args[0] : "../../../test.rav";

var sourceText = SourceText.From(File.OpenRead(fileName));

var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText);

var root = syntaxTree.GetRoot();

Console.WriteLine(root.GetSyntaxTreeRepresentation(includeTokens: true, includeTrivia: false));

Console.WriteLine();

Console.WriteLine(root.ToFullString());
