
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;
using System.Linq;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

using GreenNode = Raven.CodeAnalysis.Syntax.GreenNode;

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
                var start = Position;
                var importCount = importDirectives.Count;
                var aliasCount = aliasDirectives.Count;
                var memberCount = memberDeclarations.Count;

                ParseNamespaceMemberDeclarations(nextToken, importDirectives, aliasDirectives, memberDeclarations, ref order);

                if (Position == start)
                {
                    if (importDirectives.Count == importCount &&
                        aliasDirectives.Count == aliasCount &&
                        memberDeclarations.Count == memberCount)
                    {
                        var statement = new StatementSyntaxParser(this).ParseStatement();
                        if (statement is not null && Position > start)
                        {
                            var globalStatement = CreateGlobalStatement(SyntaxList.Empty, SyntaxList.Empty, statement);
                            memberDeclarations.Add(globalStatement);
                            order = MemberOrder.Members;
                        }
                        else if (!PeekToken().IsKind(SyntaxKind.EndOfFileToken) && !PeekToken().IsKind(SyntaxKind.CloseBraceToken))
                        {
                            ReadToken();
                        }
                    }
                    else if (!PeekToken().IsKind(SyntaxKind.EndOfFileToken) && !PeekToken().IsKind(SyntaxKind.CloseBraceToken))
                    {
                        ReadToken();
                    }
                }
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
                closeBraceToken, terminatorToken);
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
            var start = Position;
            var importCount = importDirectives.Count;
            var aliasCount = aliasDirectives.Count;
            var memberCount = memberDeclarations.Count;

            ParseNamespaceMemberDeclarations(nextToken, importDirectives, aliasDirectives, memberDeclarations, ref order);

            if (Position == start)
            {
                if (importDirectives.Count == importCount &&
                    aliasDirectives.Count == aliasCount &&
                    memberDeclarations.Count == memberCount &&
                    !PeekToken().IsKind(SyntaxKind.EndOfFileToken))
                {
                    ReadToken();
                }
                else if (!PeekToken().IsKind(SyntaxKind.EndOfFileToken))
                {
                    ReadToken();
                }
            }

            SetTreatNewlinesAsTokens(false);
        }

        return FileScopedNamespaceDeclaration(
            attributeLists,
            modifiers,
            namespaceKeyword, name, terminatorToken,
            List(importDirectives), List(aliasDirectives), List(memberDeclarations));
    }

    private void ParseNamespaceMemberDeclarations(
        SyntaxToken nextToken,
        List<ImportDirectiveSyntax> importDirectives,
        List<AliasDirectiveSyntax> aliasDirectives,
        List<MemberDeclarationSyntax> memberDeclarations,
        ref MemberOrder order)
    {
        static bool IsLikelyNamespaceMemberStart(SyntaxToken token)
        {
            return token.Kind is
                SyntaxKind.ImportKeyword or
                SyntaxKind.AliasKeyword or
                SyntaxKind.NamespaceKeyword or
                SyntaxKind.EnumKeyword or
                SyntaxKind.UnionKeyword or
                SyntaxKind.DelegateKeyword or
                SyntaxKind.StructKeyword or
                SyntaxKind.ClassKeyword or
                SyntaxKind.InterfaceKeyword or
                SyntaxKind.ExtensionKeyword or
                SyntaxKind.TraitKeyword or
                SyntaxKind.OpenBracketToken or
                SyntaxKind.PublicKeyword or
                SyntaxKind.PrivateKeyword or
                SyntaxKind.InternalKeyword or
                SyntaxKind.ProtectedKeyword or
                SyntaxKind.StaticKeyword or
                SyntaxKind.AbstractKeyword or
                SyntaxKind.FinalKeyword or
                SyntaxKind.SealedKeyword or
                SyntaxKind.OpenKeyword or
                SyntaxKind.RecordKeyword or
                SyntaxKind.PartialKeyword or
                SyntaxKind.OverrideKeyword or
                SyntaxKind.AsyncKeyword or
                SyntaxKind.ExternKeyword;
        }

        void AddMemberDeclarationWithSeparatorValidation(MemberDeclarationSyntax member)
        {
            memberDeclarations.Add(member);
            ValidateDeclarationSeparator(member);
        }

        void ValidateDeclarationSeparator(MemberDeclarationSyntax member)
        {
            if (member is GlobalStatementSyntax or IncompleteMemberDeclarationSyntax)
                return;

            var next = PeekToken();
            if (!IsLikelyNamespaceMemberStart(next))
                return;

            if (HasLeadingEndOfLineTrivia(next))
                return;

            if (!TryGetDeclarationTerminatorKind(member, out var terminatorKind))
                return;

            if (terminatorKind == SyntaxKind.None)
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.ExpectedNewLineBetweenDeclarations,
                        GetInsertionSpanBeforePeekedToken()));
            }
        }

        static bool TryGetDeclarationTerminatorKind(MemberDeclarationSyntax member, out SyntaxKind terminatorKind)
        {
            switch (member)
            {
                case BaseTypeDeclarationSyntax typeDeclaration:
                    terminatorKind = typeDeclaration.TerminatorToken.Kind;
                    return true;
                case DelegateDeclarationSyntax delegateDeclaration:
                    terminatorKind = delegateDeclaration.TerminatorToken.Kind;
                    return true;
                case NamespaceDeclarationSyntax namespaceDeclaration:
                    terminatorKind = namespaceDeclaration.TerminatorToken.Kind;
                    return true;
                case FileScopedNamespaceDeclarationSyntax fileScopedNamespaceDeclaration:
                    terminatorKind = fileScopedNamespaceDeclaration.TerminatorToken.Kind;
                    return true;
                default:
                    terminatorKind = SyntaxKind.None;
                    return false;
            }
        }

        SyntaxToken ParseIncompleteNamespaceMemberTokens()
        {
            var span = GetSpanOfPeekedToken();
            var skippedTokens = ConsumeSkippedTokensUntil(token =>
                token.Kind is SyntaxKind.CloseBraceToken or SyntaxKind.EndOfFileToken ||
                IsLikelyNamespaceMemberStart(token) ||
                StatementSyntaxParser.IsTokenPotentialStatementStart(token));
            return CreateSkippedToken(skippedTokens, span);
        }

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

            AddMemberDeclarationWithSeparatorValidation(namespaceDeclaration);
            order = MemberOrder.Members;
        }
        else if (nextToken.IsKind(SyntaxKind.EnumKeyword) ||
                 nextToken.IsKind(SyntaxKind.UnionKeyword) ||
                 nextToken.IsKind(SyntaxKind.DelegateKeyword) ||
                 nextToken.IsKind(SyntaxKind.StructKeyword) || nextToken.IsKind(SyntaxKind.ClassKeyword) || nextToken.IsKind(SyntaxKind.InterfaceKeyword) || nextToken.IsKind(SyntaxKind.ExtensionKeyword) || nextToken.IsKind(SyntaxKind.TraitKeyword) ||
                 nextToken.IsKind(SyntaxKind.PublicKeyword) || nextToken.IsKind(SyntaxKind.PrivateKeyword) ||
                 nextToken.IsKind(SyntaxKind.InternalKeyword) || nextToken.IsKind(SyntaxKind.ProtectedKeyword) ||
                 nextToken.IsKind(SyntaxKind.StaticKeyword) || nextToken.IsKind(SyntaxKind.AbstractKeyword) ||
                 nextToken.IsKind(SyntaxKind.FinalKeyword) || nextToken.IsKind(SyntaxKind.SealedKeyword) ||
                 nextToken.IsKind(SyntaxKind.OpenKeyword) || nextToken.IsKind(SyntaxKind.RecordKeyword) ||
                 nextToken.IsKind(SyntaxKind.PartialKeyword) || nextToken.IsKind(SyntaxKind.OverrideKeyword) ||
                 nextToken.IsKind(SyntaxKind.ExternKeyword) ||
                 nextToken.IsKind(SyntaxKind.OpenBracketToken))
        {
            var checkpoint = CreateCheckpoint();
            var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
            var modifiers = ParseModifiers();

            var tokenAfterModifiers = PeekToken();

            if (tokenAfterModifiers.IsKind(SyntaxKind.NamespaceKeyword))
            {
                checkpoint.Rewind();

                var namespaceDeclaration = new NamespaceDeclarationParser(this).ParseNamespaceDeclaration();

                AddMemberDeclarationWithSeparatorValidation(namespaceDeclaration);
                order = MemberOrder.Members;
                return;
            }

            var typeKeywordKind = tokenAfterModifiers.Kind;

            if (typeKeywordKind == SyntaxKind.EnumKeyword)
            {
                checkpoint.Rewind();

                var enumDeclaration = new EnumDeclarationParser(this).Parse();

                AddMemberDeclarationWithSeparatorValidation(enumDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind == SyntaxKind.UnionKeyword)
            {
                checkpoint.Rewind();

                var unionDeclaration = new UnionDeclarationParser(this).Parse();

                AddMemberDeclarationWithSeparatorValidation(unionDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind == SyntaxKind.DelegateKeyword)
            {
                checkpoint.Rewind();

                var delegateDeclaration = new TypeDeclarationParser(this).ParseDelegateDeclaration(attributeLists, modifiers);

                AddMemberDeclarationWithSeparatorValidation(delegateDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind is SyntaxKind.ExtensionKeyword or SyntaxKind.TraitKeyword)
            {
                var extensionDeclaration = new ExtensionDeclarationParser(this).Parse(attributeLists, modifiers);

                AddMemberDeclarationWithSeparatorValidation(extensionDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind is SyntaxKind.ClassKeyword or SyntaxKind.InterfaceKeyword or SyntaxKind.StructKeyword)
            {
                checkpoint.Rewind();

                var typeDeclaration = new TypeDeclarationParser(this).Parse();

                AddMemberDeclarationWithSeparatorValidation(typeDeclaration);
                order = MemberOrder.Members;
                return;
            }

            var statement = new StatementSyntaxParser(this).ParseStatement();

            if (statement is null)
                return;

            var globalStatement = CreateGlobalStatement(attributeLists, modifiers, statement);

            AddMemberDeclarationWithSeparatorValidation(globalStatement);
            order = MemberOrder.Members;
        }
        else
        {
            var statementStart = Position;
            var statement = new StatementSyntaxParser(this).ParseStatement();

            if (statement is not null && Position > statementStart)
            {
                var globalStatement = CreateGlobalStatement(SyntaxList.Empty, SyntaxList.Empty, statement);
                AddMemberDeclarationWithSeparatorValidation(globalStatement);
                order = MemberOrder.Members;
                return;
            }

            var skippedToken = ParseIncompleteNamespaceMemberTokens();
            TryConsumeTerminator(out var fallbackTerminatorToken);
            var incompleteMember = IncompleteMemberDeclaration(SyntaxList.Empty, SyntaxList.Empty, skippedToken, fallbackTerminatorToken);
            AddMemberDeclarationWithSeparatorValidation(incompleteMember);
            order = MemberOrder.Members;
        }
    }

    private SyntaxList ParseModifiers()
    {
        SyntaxList modifiers = SyntaxList.Empty;

        while (true)
        {
            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.PublicKeyword or
                     SyntaxKind.PrivateKeyword or
                     SyntaxKind.InternalKeyword or
                     SyntaxKind.ProtectedKeyword or
                     SyntaxKind.StaticKeyword or
                     SyntaxKind.AbstractKeyword or
                     SyntaxKind.FinalKeyword or
                     SyntaxKind.SealedKeyword or
                     SyntaxKind.PartialKeyword or
                     SyntaxKind.VirtualKeyword or
                     SyntaxKind.AsyncKeyword or
                     SyntaxKind.OpenKeyword or
                     SyntaxKind.RecordKeyword or
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

    private static GlobalStatementSyntax CreateGlobalStatement(
        SyntaxList attributeLists,
        SyntaxList modifiers,
        StatementSyntax statement)
    {
        if (statement is FunctionStatementSyntax functionStatement)
        {
            var mergedAttributes = ConcatenateSyntaxLists(attributeLists, functionStatement.AttributeLists);
            var mergedModifiers = ConcatenateSyntaxLists(modifiers, functionStatement.Modifiers);

            statement = functionStatement.Update(
                mergedAttributes,
                mergedModifiers,
                functionStatement.FuncKeyword,
                functionStatement.Identifier,
                functionStatement.TypeParameterList,
                functionStatement.ParameterList,
                functionStatement.ReturnType,
                functionStatement.ConstraintClauses,
                functionStatement.Body,
                functionStatement.ExpressionBody,
                functionStatement.TerminatorToken);

            attributeLists = SyntaxList.Empty;
            modifiers = SyntaxList.Empty;
        }

        return GlobalStatement(attributeLists, modifiers, statement, Token(SyntaxKind.None));
    }

    private static SyntaxList ConcatenateSyntaxLists(SyntaxList first, SyntaxList second)
    {
        if (!first.GetChildren().Any())
            return second;

        if (!second.GetChildren().Any())
            return first;

        var nodes = new List<GreenNode>();
        nodes.AddRange(first.GetChildren());
        nodes.AddRange(second.GetChildren());
        return List(nodes);
    }
}
