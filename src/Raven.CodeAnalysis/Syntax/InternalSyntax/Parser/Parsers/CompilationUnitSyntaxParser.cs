namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;
using System.Linq;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

using GreenNode = Raven.CodeAnalysis.Syntax.GreenNode;

internal class CompilationUnitSyntaxParser : SyntaxParser
{
    public CompilationUnitSyntaxParser(ParseContext parent) : base(parent)
    {

    }

    private enum MemberOrder
    {
        Imports,
        Aliases,
        Members
    }

    public CompilationUnitSyntax Parse()
    {
        _stopwatch.Reset();
        _stopwatch.Start();

        SetTreatNewlinesAsTokens(false);

        List<GreenNode> compilationAttributeLists = [];
        while (TryParseCompilationAttributeList(out var attributeList))
        {
            compilationAttributeLists.Add(attributeList);
        }

        List<ImportDirectiveSyntax> importDirectives = [];
        List<AliasDirectiveSyntax> aliasDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        SyntaxToken nextToken;

        var order = MemberOrder.Imports;

        while (!ConsumeToken(SyntaxKind.EndOfFileToken, out nextToken))
        {
            var start = Position;
            var importCount = importDirectives.Count;
            var aliasCount = aliasDirectives.Count;
            var memberCount = memberDeclarations.Count;

            ParseNamespaceMemberDeclarations(nextToken, importDirectives, aliasDirectives, memberDeclarations, ref order);

            if (Position == start &&
                importDirectives.Count == importCount &&
                aliasDirectives.Count == aliasCount &&
                memberDeclarations.Count == memberCount &&
                !PeekToken().IsKind(SyntaxKind.EndOfFileToken))
            {
                ReadToken();
            }

            SetTreatNewlinesAsTokens(false);
        }

        var attributeLists = List(compilationAttributeLists);

        var cu = CompilationUnit(attributeLists, List(importDirectives), List(aliasDirectives), List(memberDeclarations), nextToken);

        _stopwatch.Stop();

        return cu;
    }

    private bool TryParseCompilationAttributeList(out AttributeListSyntax attributeList)
    {
        attributeList = default!;

        if (!PeekToken().IsKind(SyntaxKind.OpenBracketToken))
            return false;

        var checkpoint = CreateCheckpoint();
        var parsedAttributeList = AttributeDeclarationParser.ParseAttributeList(this);

        if (IsAssemblyAttributeList(parsedAttributeList))
        {
            attributeList = parsedAttributeList;
            return true;
        }

        checkpoint.Rewind();
        return false;
    }

    private static bool IsAssemblyAttributeList(AttributeListSyntax attributeList)
    {
        if (attributeList.Target is not AttributeTargetSpecifierSyntax target)
            return false;

        return string.Equals(target.Identifier.GetValueText(), "assembly", StringComparison.Ordinal);
    }

    private static bool IsPossibleCompilationUnitMemberStart(SyntaxToken token)
    {
        return token.Kind is SyntaxKind.ImportKeyword or SyntaxKind.AliasKeyword or SyntaxKind.NamespaceKeyword or
            SyntaxKind.EnumKeyword or SyntaxKind.UnionKeyword or SyntaxKind.DelegateKeyword or SyntaxKind.StructKeyword or SyntaxKind.ClassKeyword or
            SyntaxKind.InterfaceKeyword or SyntaxKind.ExtensionKeyword or SyntaxKind.TraitKeyword or SyntaxKind.OpenBracketToken or
            SyntaxKind.PublicKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.InternalKeyword or SyntaxKind.ProtectedKeyword or
            SyntaxKind.StaticKeyword or SyntaxKind.AbstractKeyword or SyntaxKind.FinalKeyword or SyntaxKind.SealedKeyword or
            SyntaxKind.OpenKeyword or SyntaxKind.RecordKeyword or
            SyntaxKind.PartialKeyword or SyntaxKind.OverrideKeyword or SyntaxKind.AsyncKeyword or SyntaxKind.ExternKeyword;
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

            AddMemberDeclaration(memberDeclarations, namespaceDeclaration);
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
                 nextToken.IsKind(SyntaxKind.PartialKeyword) ||
                 nextToken.IsKind(SyntaxKind.OverrideKeyword) ||
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

                AddMemberDeclaration(memberDeclarations, namespaceDeclaration);
                order = MemberOrder.Members;
                return;
            }

            var typeKeywordKind = tokenAfterModifiers.Kind;

            if (typeKeywordKind == SyntaxKind.EnumKeyword)
            {
                checkpoint.Rewind();

                var enumDeclaration = new EnumDeclarationParser(this).Parse();

                AddMemberDeclaration(memberDeclarations, enumDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind == SyntaxKind.UnionKeyword)
            {
                checkpoint.Rewind();

                var unionDeclaration = new UnionDeclarationParser(this).Parse();

                AddMemberDeclaration(memberDeclarations, unionDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind == SyntaxKind.DelegateKeyword)
            {
                checkpoint.Rewind();

                var delegateDeclaration = new TypeDeclarationParser(this).ParseDelegateDeclaration(attributeLists, modifiers);

                AddMemberDeclaration(memberDeclarations, delegateDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind is SyntaxKind.ExtensionKeyword or SyntaxKind.TraitKeyword)
            {
                var extensionDeclaration = new ExtensionDeclarationParser(this).Parse(attributeLists, modifiers);

                AddMemberDeclaration(memberDeclarations, extensionDeclaration);
                order = MemberOrder.Members;
                return;
            }

            if (typeKeywordKind is SyntaxKind.ClassKeyword or SyntaxKind.InterfaceKeyword or SyntaxKind.StructKeyword)
            {
                checkpoint.Rewind();

                var typeDeclaration = new TypeDeclarationParser(this).Parse();

                AddMemberDeclaration(memberDeclarations, typeDeclaration);
                order = MemberOrder.Members;
                return;
            }

            var statementStartAfterModifiers = StatementSyntaxParser.IsTokenPotentialStatementStart(tokenAfterModifiers);

            if (!statementStartAfterModifiers)
            {
                var skippedToken = ParseIncompleteMemberTokens();
                TryConsumeTerminator(out var terminatorToken);
                var incompleteMember = IncompleteMemberDeclaration(attributeLists, modifiers, skippedToken, terminatorToken);

                AddMemberDeclaration(memberDeclarations, incompleteMember);
                order = MemberOrder.Members;
                return;
            }

            var statement = new StatementSyntaxParser(this).ParseStatement();

            if (statement is null)
                return;

            AddGlobalMember(memberDeclarations, attributeLists, modifiers, statement);
            order = MemberOrder.Members;
        }
        else
        {
            var statementStart = StatementSyntaxParser.IsTokenPotentialStatementStart(nextToken);

            if (!statementStart)
            {
                var skippedToken = ParseIncompleteMemberTokens();
                TryConsumeTerminator(out var terminatorToken);
                var incompleteMember = IncompleteMemberDeclaration(SyntaxList.Empty, SyntaxList.Empty, skippedToken, terminatorToken);

                AddMemberDeclaration(memberDeclarations, incompleteMember);
                order = MemberOrder.Members;
                return;
            }

            var statement = new StatementSyntaxParser(this).ParseStatement();

            if (statement is null)
                return;

            AddGlobalMember(memberDeclarations, SyntaxList.Empty, SyntaxList.Empty, statement);
            order = MemberOrder.Members;
        }
    }

    private void AddGlobalMember(
        List<MemberDeclarationSyntax> memberDeclarations,
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

        var globalStatement = GlobalStatement(attributeLists, modifiers, statement, Token(SyntaxKind.None));

        memberDeclarations.Add(globalStatement);
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

    private void AddMemberDeclaration(List<MemberDeclarationSyntax> memberDeclarations, MemberDeclarationSyntax declaration)
    {
        memberDeclarations.Add(declaration);
        ValidateDeclarationSeparator(declaration);
    }

    private void ValidateDeclarationSeparator(MemberDeclarationSyntax member)
    {
        if (member is GlobalStatementSyntax or IncompleteMemberDeclarationSyntax)
            return;

        var next = PeekToken();
        if (!IsPossibleCompilationUnitMemberStart(next))
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

    private static bool TryGetDeclarationTerminatorKind(MemberDeclarationSyntax member, out SyntaxKind terminatorKind)
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

    private SyntaxToken ParseIncompleteMemberTokens()
    {
        var peek = PeekToken();
        var span = GetSpanOfPeekedToken();

        var skippedTokens = ConsumeSkippedTokensUntil(token =>
            token.Kind is SyntaxKind.CloseBraceToken or SyntaxKind.EndOfFileToken ||
            IsPossibleCompilationUnitMemberStart(token) ||
            StatementSyntaxParser.IsTokenPotentialStatementStart(token));

        return CreateSkippedToken(skippedTokens, span);
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
}
