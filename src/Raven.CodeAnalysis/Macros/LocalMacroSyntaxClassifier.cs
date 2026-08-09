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
    private const string CompilerPluginMarkerName = "RavenCompilerPlugin";
    private const string CompilerPluginMarkerAttributeName = "RavenCompilerPluginAttribute";
    private static readonly HashSet<string> s_macroInterfaceNames =
    [
        nameof(IAttachedDeclarationMacro),
        nameof(IFreestandingExpressionMacro),
        nameof(ITokenTreeExpressionMacro)
    ];

    public static bool IsLocalMacroTree(SyntaxTree syntaxTree)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        if (IsCompilerPluginTree(syntaxTree))
            return false;

        var members = syntaxTree.GetRoot().Members;
        return members.Count > 0 &&
            members.All(member =>
                member is MacroDeclarationSyntax ||
                member is TypeDeclarationSyntax declaration &&
                IsLocalMacroDeclaration(declaration));
    }

    public static bool IsCompilerPluginTree(SyntaxTree syntaxTree)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        return syntaxTree.GetRoot().AttributeLists
            .Where(static list => string.Equals(
                list.Target?.Identifier.ValueText,
                "assembly",
                StringComparison.Ordinal))
            .SelectMany(static list => list.Attributes)
            .Any(static attribute => IsMarkerAttribute(
                attribute,
                CompilerPluginMarkerName,
                CompilerPluginMarkerAttributeName));
    }

    public static LocalMacroSyntaxPartition Partition(SyntaxTree syntaxTree)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        if (IsCompilerPluginTree(syntaxTree))
        {
            var macroDeclarations = GetTopLevelMacroDeclarations(syntaxTree)
                .OfType<MacroDeclarationSyntax>()
                .Cast<MemberDeclarationSyntax>()
                .ToArray();
            if (macroDeclarations.Length == 0)
                return new LocalMacroSyntaxPartition(syntaxTree, null);

            return new LocalMacroSyntaxPartition(
                CreateConsumerProjection(syntaxTree, macroDeclarations),
                CreateMacroProjection(
                    syntaxTree,
                    macroDeclarations,
                    copyRootAttributes: false));
        }

        if (IsLocalMacroTree(syntaxTree))
            return new LocalMacroSyntaxPartition(null, syntaxTree);

        var declarations = GetTopLevelMacroDeclarations(syntaxTree)
            .ToArray();
        if (declarations.Length == 0)
            return new LocalMacroSyntaxPartition(syntaxTree, null);

        return new LocalMacroSyntaxPartition(
            CreateConsumerProjection(syntaxTree, declarations),
            CreateMacroProjection(syntaxTree, declarations));
    }

    public static bool IsLocalMacroPosition(SyntaxTree syntaxTree, int position)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        if ((uint)position > (uint)syntaxTree.Length)
            throw new ArgumentOutOfRangeException(nameof(position));

        if (IsCompilerPluginTree(syntaxTree))
        {
            return GetTopLevelMacroDeclarations(syntaxTree)
                .OfType<MacroDeclarationSyntax>()
                .Any(declaration =>
                    position >= declaration.FullSpan.Start &&
                    position < declaration.FullSpan.End);
        }

        return GetTopLevelMacroDeclarations(syntaxTree)
            .Any(declaration =>
                position >= declaration.FullSpan.Start &&
                position < declaration.FullSpan.End);
    }

    private static IEnumerable<TypeDeclarationSyntax> GetTopLevelTypeDeclarations(SyntaxTree syntaxTree)
        => syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<TypeDeclarationSyntax>()
            .Where(static declaration => !declaration.Ancestors().OfType<TypeDeclarationSyntax>().Any());

    private static IEnumerable<MemberDeclarationSyntax> GetTopLevelMacroDeclarations(SyntaxTree syntaxTree)
    {
        foreach (var declaration in GetTopLevelTypeDeclarations(syntaxTree)
            .Where(static declaration => declaration.Parent is CompilationUnitSyntax or BaseNamespaceDeclarationSyntax)
            .Where(IsLocalMacroDeclaration))
        {
            yield return declaration;
        }

        foreach (var declaration in syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Where(static declaration =>
                declaration.Parent is CompilationUnitSyntax or BaseNamespaceDeclarationSyntax))
        {
            yield return declaration;
        }
    }

    private static bool HasMarkerAttribute(
        TypeDeclarationSyntax declaration,
        string markerName,
        string markerAttributeName)
        => declaration.AttributeLists
            .SelectMany(static list => list.Attributes)
            .Any(attribute => IsMarkerAttribute(attribute, markerName, markerAttributeName));

    private static bool IsLocalMacroDeclaration(TypeDeclarationSyntax declaration)
        => HasMarkerAttribute(
                declaration,
                DeclarationMarkerName,
                DeclarationMarkerAttributeName) ||
            ImplementsMacroInterface(declaration);

    private static bool ImplementsMacroInterface(TypeDeclarationSyntax declaration)
    {
        var baseList = declaration switch
        {
            ClassDeclarationSyntax classDeclaration => classDeclaration.BaseList,
            RecordDeclarationSyntax recordDeclaration => recordDeclaration.BaseList,
            StructDeclarationSyntax structDeclaration => structDeclaration.BaseList,
            _ => null
        };
        if (baseList is null)
            return false;

        return baseList.Types.Any(baseType =>
        {
            var identifier = baseType.Type
                .DescendantTokens()
                .LastOrDefault(static token => token.Kind == SyntaxKind.IdentifierToken);
            return s_macroInterfaceNames.Contains(identifier.ValueText);
        });
    }

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
        IReadOnlyList<MemberDeclarationSyntax> declarations)
    {
        var text = syntaxTree.GetText()!.ToString().ToCharArray();
        foreach (var declaration in declarations)
            Mask(text, declaration.FullSpan);

        return ParseProjection(syntaxTree, text);
    }

    private static SyntaxTree CreateMacroProjection(
        SyntaxTree syntaxTree,
        IReadOnlyList<MemberDeclarationSyntax> declarations,
        bool copyRootAttributes = true)
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
        if (copyRootAttributes)
        {
            foreach (var attributeList in root.AttributeLists)
                Copy(source, text, attributeList.FullSpan);
        }
        foreach (var declaration in declarations)
        {
            foreach (var namespaceDeclaration in declaration.Ancestors()
                         .OfType<BaseNamespaceDeclarationSyntax>()
                         .Reverse())
            {
                CopyNamespaceEnvelope(source, text, namespaceDeclaration);
            }

            Copy(source, text, declaration.FullSpan);
        }

        return ParseProjection(syntaxTree, text);
    }

    private static void CopyNamespaceEnvelope(
        string source,
        char[] destination,
        BaseNamespaceDeclarationSyntax declaration)
    {
        var firstMemberStart = declaration.Members.Count > 0
            ? declaration.Members[0].FullSpan.Start
            : declaration.FullSpan.End;
        Copy(
            source,
            destination,
            TextSpan.FromBounds(declaration.FullSpan.Start, firstMemberStart));

        if (declaration is NamespaceDeclarationSyntax blockNamespace)
        {
            Copy(
                source,
                destination,
                TextSpan.FromBounds(
                    blockNamespace.CloseBraceToken.FullSpan.Start,
                    blockNamespace.FullSpan.End));
        }
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
