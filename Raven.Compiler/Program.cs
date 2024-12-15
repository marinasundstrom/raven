using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

var fileName = args.Length > 0 ? args[0] : "../../../test.rav";

var sourceText = SourceText.From(File.OpenRead(fileName));

var syntaxTree = SyntaxTree.ParseText(sourceText);

var root = syntaxTree.GetRoot();

//Console.WriteLine(root.ToFullString());

Console.WriteLine(root.GetSyntaxTreeRepresentation(includeTokens: true, includeTrivia: false));