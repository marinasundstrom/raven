using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal static class LocalMacroSyntaxClassifier
{
    private const string DeclarationMarkerName = "LocalMacro";
    private const string DeclarationMarkerAttributeName = "LocalMacroAttribute";
    private const string FileMarkerName = "LocalMacroPlugin";
    private const string FileMarkerAttributeName = "LocalMacroPluginAttribute";

    public static bool IsLocalMacroTree(SyntaxTree syntaxTree)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        return GetTopLevelTypeDeclarations(syntaxTree)
            .Any(static declaration => HasMarkerAttribute(
                declaration,
                FileMarkerName,
                FileMarkerAttributeName));
    }

    public static LocalMacroSyntaxPartition Partition(SyntaxTree syntaxTree)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        if (IsLocalMacroTree(syntaxTree))
            return new LocalMacroSyntaxPartition(null, syntaxTree);

        var declarations = GetTopLevelTypeDeclarations(syntaxTree)
            .Where(static declaration => declaration.Parent is CompilationUnitSyntax)
            .Where(static declaration => HasMarkerAttribute(
                declaration,
                DeclarationMarkerName,
                DeclarationMarkerAttributeName))
            .ToArray();
        if (declarations.Length == 0)
            return new LocalMacroSyntaxPartition(syntaxTree, null);

        return new LocalMacroSyntaxPartition(
            CreateConsumerProjection(syntaxTree, declarations),
            CreateMacroProjection(syntaxTree, declarations));
    }

    private static IEnumerable<TypeDeclarationSyntax> GetTopLevelTypeDeclarations(SyntaxTree syntaxTree)
        => syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<TypeDeclarationSyntax>()
            .Where(static declaration => !declaration.Ancestors().OfType<TypeDeclarationSyntax>().Any());

    private static bool HasMarkerAttribute(
        TypeDeclarationSyntax declaration,
        string markerName,
        string markerAttributeName)
        => declaration.AttributeLists
            .SelectMany(static list => list.Attributes)
            .Any(attribute => IsMarkerAttribute(attribute, markerName, markerAttributeName));

    private static bool IsMarkerAttribute(
        AttributeSyntax attribute,
        string markerName,
        string markerAttributeName)
    {
        if (attribute.HashToken.Kind != SyntaxKind.None)
            return false;

        var identifier = attribute.Name
            .DescendantTokens()
            .LastOrDefault(static token => token.Kind == SyntaxKind.IdentifierToken);
        return string.Equals(identifier.ValueText, markerName, StringComparison.Ordinal) ||
               string.Equals(identifier.ValueText, markerAttributeName, StringComparison.Ordinal);
    }

    private static SyntaxTree CreateConsumerProjection(
        SyntaxTree syntaxTree,
        IReadOnlyList<TypeDeclarationSyntax> declarations)
    {
        var text = syntaxTree.GetText()!.ToString().ToCharArray();
        foreach (var declaration in declarations)
            Mask(text, declaration.FullSpan);

        return ParseProjection(syntaxTree, text);
    }

    private static SyntaxTree CreateMacroProjection(
        SyntaxTree syntaxTree,
        IReadOnlyList<TypeDeclarationSyntax> declarations)
    {
        var source = syntaxTree.GetText()!.ToString();
        var text = source
            .Select(static character => character is '\r' or '\n' ? character : ' ')
            .ToArray();
        var root = syntaxTree.GetRoot();

        foreach (var import in root.Imports)
            Copy(source, text, import.FullSpan);
        foreach (var alias in root.Aliases)
            Copy(source, text, alias.FullSpan);
        foreach (var attributeList in root.AttributeLists)
            Copy(source, text, attributeList.FullSpan);
        foreach (var declaration in declarations)
            Copy(source, text, declaration.FullSpan);

        return ParseProjection(syntaxTree, text);
    }

    private static void Mask(char[] text, TextSpan span)
    {
        for (var index = span.Start; index < span.End; index++)
        {
            if (text[index] is not ('\r' or '\n'))
                text[index] = ' ';
        }
    }

    private static void Copy(string source, char[] destination, TextSpan span)
        => source.CopyTo(span.Start, destination, span.Start, span.Length);

    private static SyntaxTree ParseProjection(SyntaxTree sourceTree, char[] text)
        => SyntaxTree.ParseText(
            SourceText.From(new string(text), sourceTree.Encoding),
            sourceTree.Options,
            sourceTree.FilePath);
}

internal readonly record struct LocalMacroSyntaxPartition(
    SyntaxTree? ConsumerTree,
    SyntaxTree? MacroTree);
