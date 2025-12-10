
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class NamespaceDeclarationParser : SyntaxParser
{
    public NamespaceDeclarationParser(ParseContext context) : base(context)
    {

    }

    private enum MemberOrder
    {
        Imports,
        Aliases,
        Members
    }

    public MemberDeclarationSyntax ParseNamespaceDeclaration()
    {
        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
        var modifiers = ParseModifiers();

        List<ImportDirectiveSyntax> importDirectives = [];
        List<AliasDirectiveSyntax> aliasDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        var namespaceKeyword = ReadToken();

        var name = new NameSyntaxParser(this).ParseName();

        if (ConsumeToken(SyntaxKind.OpenBraceToken, out var openBraceToken))
        {
            var order = MemberOrder.Imports;

            while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken) && nextToken.Kind != SyntaxKind.CloseBraceToken)
            {
                ParseNamespaceMemberDeclarations(nextToken, importDirectives, aliasDirectives, memberDeclarations, ref order);
            }

            if (!ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken))
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.CharacterExpected,
                        GetEndOfLastToken(),
                        ['}']
                    ));
            }

            TryConsumeTerminator(out var terminatorToken);

            return NamespaceDeclaration(
                attributeLists,
                modifiers,
                namespaceKeyword, name, openBraceToken,
                new SyntaxList(importDirectives.ToArray()), new SyntaxList(aliasDirectives.ToArray()), new SyntaxList(memberDeclarations.ToArray()),
                closeBraceToken, terminatorToken, Diagnostics);
        }

        return ParseFileScopedNamespaceDeclarationCore(attributeLists, modifiers, namespaceKeyword, name, importDirectives, aliasDirectives, memberDeclarations);
    }

    private MemberDeclarationSyntax ParseFileScopedNamespaceDeclarationCore(SyntaxList attributeLists, SyntaxList modifiers, SyntaxToken namespaceKeyword, NameSyntax name, List<ImportDirectiveSyntax> importDirectives, List<AliasDirectiveSyntax> aliasDirectives, List<MemberDeclarationSyntax> memberDeclarations)
    {
        SetTreatNewlinesAsTokens(true);

        TryConsumeTerminator(out var terminatorToken);

        SetTreatNewlinesAsTokens(false);

        var order = MemberOrder.Imports;

        while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken, importDirectives, aliasDirectives, memberDeclarations, ref order);

            SetTreatNewlinesAsTokens(false);
        }

        return FileScopedNamespaceDeclaration(
            attributeLists,
            modifiers,
            namespaceKeyword, name, terminatorToken,
            List(importDirectives), List(aliasDirectives), List(memberDeclarations), Diagnostics);
    }

    private void ParseNamespaceMemberDeclarations(
        SyntaxToken nextToken,
        List<ImportDirectiveSyntax> importDirectives,
        List<AliasDirectiveSyntax> aliasDirectives,
        List<MemberDeclarationSyntax> memberDeclarations,
        ref MemberOrder order)
    {
        if (nextToken.IsKind(SyntaxKind.ImportKeyword))
        {
            var start = Position;
            var importDirective = new ImportDirectiveSyntaxParser(this).ParseImportDirective();

            if (order > MemberOrder.Imports)
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.ImportDirectiveOutOfOrder,
                        GetActualTextSpan(start, importDirective)));
            }

            importDirectives.Add(importDirective);
            order = MemberOrder.Imports;
        }
        else if (nextToken.IsKind(Raven.CodeAnalysis.Syntax.SyntaxKind.AliasKeyword))
        {
            var start = Position;
            var aliasDirective = new AliasDirectiveSyntaxParser(this).ParseAliasDirective();

            if (order > MemberOrder.Aliases)
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.AliasDirectiveOutOfOrder,
                        GetActualTextSpan(start, aliasDirective)));
            }

            aliasDirectives.Add(aliasDirective);
            order = MemberOrder.Aliases;
        }
        else if (nextToken.IsKind(SyntaxKind.NamespaceKeyword))
        {
            var namespaceDeclaration = new NamespaceDeclarationParser(this).ParseNamespaceDeclaration();

            memberDeclarations.Add(namespaceDeclaration);
            order = MemberOrder.Members;
        }
        else if (nextToken.IsKind(SyntaxKind.EnumKeyword) ||
                 nextToken.IsKind(SyntaxKind.UnionKeyword) ||
                 nextToken.IsKind(SyntaxKind.StructKeyword) || nextToken.IsKind(SyntaxKind.ClassKeyword) || nextToken.IsKind(SyntaxKind.InterfaceKeyword) || nextToken.IsKind(SyntaxKind.ExtensionKeyword) ||
                 nextToken.IsKind(SyntaxKind.PublicKeyword) || nextToken.IsKind(SyntaxKind.PrivateKeyword) ||
                 nextToken.IsKind(SyntaxKind.InternalKeyword) || nextToken.IsKind(SyntaxKind.ProtectedKeyword) ||
                 nextToken.IsKind(SyntaxKind.StaticKeyword) || nextToken.IsKind(SyntaxKind.AbstractKeyword) ||
                 nextToken.IsKind(SyntaxKind.SealedKeyword) || nextToken.IsKind(SyntaxKind.OpenKeyword) ||
                 nextToken.IsKind(SyntaxKind.PartialKeyword) || nextToken.IsKind(SyntaxKind.OverrideKeyword) ||
                 nextToken.IsKind(SyntaxKind.OpenBracketToken))
        {
            var checkpoint = CreateCheckpoint();
            var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
            var modifiers = ParseModifiers();

            var tokenAfterModifiers = PeekToken();

            if (tokenAfterModifiers.IsKind(SyntaxKind.NamespaceKeyword))
            {
                checkpoint.Dispose();

                var namespaceDeclaration = new NamespaceDeclarationParser(this).ParseNamespaceDeclaration();

                memberDeclarations.Add(namespaceDeclaration);
                order = MemberOrder.Members;
                return;
            }

            var typeKeywordKind = tokenAfterModifiers.Kind;

            if (typeKeywordKind == SyntaxKind.EnumKeyword)
            {
                checkpoint.Dispose();

                var enumDeclaration = new EnumDeclarationParser(this).Parse();

                memberDeclarations.Add(enumDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind == SyntaxKind.UnionKeyword)
            {
                checkpoint.Dispose();

                var unionDeclaration = new UnionDeclarationParser(this).Parse();

                memberDeclarations.Add(unionDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind == SyntaxKind.ExtensionKeyword)
            {
                var extensionDeclaration = new ExtensionDeclarationParser(this).Parse(attributeLists, modifiers);

                memberDeclarations.Add(extensionDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind is SyntaxKind.ClassKeyword or SyntaxKind.InterfaceKeyword or SyntaxKind.StructKeyword)
            {
                checkpoint.Dispose();

                var typeDeclaration = new TypeDeclarationParser(this).Parse();

                memberDeclarations.Add(typeDeclaration);
                order = MemberOrder.Members;
                return;
            }

            var statement = new StatementSyntaxParser(this).ParseStatement();

            if (statement is null)
                return;

            var globalStatement = GlobalStatement(attributeLists, modifiers, statement, Diagnostics);

            memberDeclarations.Add(globalStatement);
            order = MemberOrder.Members;
        }
        else
        {
            // Should warn (?)

            var statement = new StatementSyntaxParser(this).ParseStatement();

            if (statement is null)
                return;

            var globalStatement = GlobalStatement(SyntaxList.Empty, SyntaxList.Empty, statement, Diagnostics);

            memberDeclarations.Add(globalStatement);
            order = MemberOrder.Members;
        }
    }

    private SyntaxList ParseModifiers()
    {
        SyntaxList modifiers = SyntaxList.Empty;

        var loopProgress = StartLoopProgress("ParseNamespaceModifiers");

        while (true)
        {
            loopProgress.EnsureProgress();

            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.PublicKeyword or
                     SyntaxKind.PrivateKeyword or
                     SyntaxKind.InternalKeyword or
                     SyntaxKind.ProtectedKeyword or
                     SyntaxKind.StaticKeyword or
                     SyntaxKind.AbstractKeyword or
                     SyntaxKind.SealedKeyword or
                     SyntaxKind.PartialKeyword or
                     SyntaxKind.VirtualKeyword or
                     SyntaxKind.AsyncKeyword or
                     SyntaxKind.OpenKeyword or
                     SyntaxKind.OverrideKeyword)
            {
                modifiers = modifiers.Add(ReadToken());
            }
            else
            {
                break;
            }
        }

        return modifiers;
    }
}
