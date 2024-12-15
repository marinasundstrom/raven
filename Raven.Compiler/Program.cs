using System;
using System.Text;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.Compiler;

class Program
{
    static void Main(string[] args)
    {
        var fileName = args.Length > 0 ? args[0] : "../../../test.rav";

        var sourceText = SourceText.From(File.OpenRead(fileName));

        var syntaxTree = SyntaxTree.ParseText(sourceText);

        var oldRoot = syntaxTree.GetRoot();

        //Console.WriteLine(oldRoot.ToFullString());

        Console.WriteLine(oldRoot.GetSyntaxTreeRepresentation(includeTokens: true, includeTrivia: false));
    }
}