namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

using static SyntaxFactory;

internal class UnionDeclarationParser : SyntaxParser
{
    public UnionDeclarationParser(ParseContext context) : base(context)
    {
    }

    internal UnionDeclarationSyntax Parse()
    {
        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
        var modifiers = ParseModifiers();

        var unionKeyword = ExpectToken(SyntaxKind.UnionKeyword);
        var classOrStructKeyword = ConsumeClassOrStructKeyword();

        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        TypeParameterListSyntax? typeParameterList = null;
        if (PeekToken().IsKind(SyntaxKind.LessThanToken))
        {
            typeParameterList = new TypeDeclarationParser(this).ParseTypeParameterList();
        }

        UnionMemberTypeListSyntax? memberTypes = null;
        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            memberTypes = ParseUnionMemberTypeList();
        }

        var constraintClauses = new ConstrainClauseListParser(this).ParseConstraintClauseList();

        SyntaxToken openBraceToken;
        SyntaxToken closeBraceToken;
        List<GreenNode> members = new();

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            openBraceToken = ReadToken();
            var memberParser = new TypeDeclarationParser(this);

            while (true)
            {
                var next = PeekToken();

                if (next.IsKind(SyntaxKind.CloseBraceToken) || next.IsKind(SyntaxKind.EndOfFileToken))
                {
                    break;
                }

                var memberStart = memberParser.Position;
                var member = memberParser.ParseMember();

                if (memberParser.Position == memberStart)
                {
                    var skippedToken = ParseIncompleteTypeMemberTokens();
                    TryConsumeTerminator(out var memberTerminatorToken);
                    members.Add(IncompleteMemberDeclaration(SyntaxList.Empty, SyntaxList.Empty, skippedToken, memberTerminatorToken));
                }
                else
                {
                    members.Add(member);
                }

                SetTreatNewlinesAsTokens(false);
                if (PeekToken().IsKind(SyntaxKind.CommaToken))
                {
                    members.Add(ReadToken());
                }
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out closeBraceToken);
        }
        else
        {
            openBraceToken = Token(SyntaxKind.None);
            closeBraceToken = Token(SyntaxKind.None);
        }

        var terminatorToken = ConsumeOptionalTypeTerminator();

        return UnionDeclaration(
            attributeLists,
            modifiers,
            unionKeyword,
            classOrStructKeyword,
            identifier,
            typeParameterList,
            memberTypes,
            constraintClauses,
            openBraceToken,
            List(members),
            closeBraceToken,
            terminatorToken);
    }

    private SyntaxToken ConsumeClassOrStructKeyword()
    {
        if (PeekToken().Kind is SyntaxKind.ClassKeyword or SyntaxKind.StructKeyword)
            return ReadToken();

        return Token(SyntaxKind.None);
    }

    private UnionMemberTypeListSyntax ParseUnionMemberTypeList()
    {
        var openParenToken = ExpectToken(SyntaxKind.OpenParenToken);
        var types = new List<GreenNode>();
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        SyntaxToken closeParenToken;

        try
        {
            var parsedTypes = 0;
            while (true)
            {
                var current = PeekToken();

                if (parsedTypes > 0)
                {
                    if (current.IsKind(SyntaxKind.BarToken))
                    {
                        types.Add(ReadToken());
                        current = PeekToken();
                    }
                    else if (!current.IsKind(SyntaxKind.CloseParenToken) &&
                             !current.IsKind(SyntaxKind.EndOfFileToken))
                    {
                        types.Add(MissingToken(SyntaxKind.BarBarToken));
                    }
                }

                while (current.Kind is SyntaxKind.LineFeedToken or SyntaxKind.CarriageReturnToken or SyntaxKind.CarriageReturnLineFeedToken)
                {
                    ReadToken();
                    current = PeekToken();
                }

                if (current.IsKind(SyntaxKind.CloseParenToken) || current.IsKind(SyntaxKind.EndOfFileToken))
                    break;

                var typeName = new NameSyntaxParser(this).ParseTypeName();
                if (typeName is null or { IsMissing: true })
                {
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.IdentifierExpected,
                            GetSpanOfPeekedToken()));
                    typeName = IdentifierName(MissingToken(SyntaxKind.IdentifierToken));
                }

                types.Add(typeName);
                parsedTypes++;
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out closeParenToken);
        }
        finally
        {
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }

        return UnionMemberTypeList(openParenToken, List(types), closeParenToken);
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
                    SyntaxKind.FileprivateKeyword or
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

    private SyntaxToken ConsumeOptionalTypeTerminator()
    {
        var current = PeekToken();

        if (current.Kind == SyntaxKind.SemicolonToken)
            return ReadToken();

        if (HasLineBreakBeforePeekToken() || current.Kind is SyntaxKind.EndOfFileToken or SyntaxKind.CloseBraceToken)
            return Token(SyntaxKind.None);

        return Token(SyntaxKind.None);
    }

    private SyntaxToken ParseIncompleteTypeMemberTokens()
    {
        var span = GetSpanOfPeekedToken();

        var skippedTokens = ConsumeSkippedTokensUntil(
            token => token.Kind is SyntaxKind.CloseBraceToken or SyntaxKind.EndOfFileToken or SyntaxKind.CommaToken ||
                     IsPossibleTypeMemberStart(token),
            stopAtImplicitLineBreak: true);

        return CreateSkippedToken(skippedTokens, span);
    }

    private static bool IsPossibleTypeMemberStart(SyntaxToken token)
    {
        return token.Kind is
            SyntaxKind.OpenBracketToken or
            SyntaxKind.HashToken or
            SyntaxKind.OpenBraceToken or
            SyntaxKind.PublicKeyword or
            SyntaxKind.PrivateKeyword or
            SyntaxKind.InternalKeyword or
            SyntaxKind.ProtectedKeyword or
            SyntaxKind.FileprivateKeyword or
            SyntaxKind.StaticKeyword or
            SyntaxKind.ReadonlyKeyword or
            SyntaxKind.AbstractKeyword or
            SyntaxKind.FinalKeyword or
            SyntaxKind.FinallyKeyword or
            SyntaxKind.SealedKeyword or
            SyntaxKind.PartialKeyword or
            SyntaxKind.VirtualKeyword or
            SyntaxKind.AsyncKeyword or
            SyntaxKind.ExternKeyword or
            SyntaxKind.OpenKeyword or
            SyntaxKind.RecordKeyword or
            SyntaxKind.OverrideKeyword or
            SyntaxKind.ClassKeyword or
            SyntaxKind.StructKeyword or
            SyntaxKind.InterfaceKeyword or
            SyntaxKind.ExtensionKeyword or
            SyntaxKind.TraitKeyword or
            SyntaxKind.EnumKeyword or
            SyntaxKind.UnionKeyword or
            SyntaxKind.DelegateKeyword or
            SyntaxKind.EventKeyword or
            SyntaxKind.FieldKeyword or
            SyntaxKind.ValKeyword or
            SyntaxKind.VarKeyword or
            SyntaxKind.ConstKeyword or
            SyntaxKind.FuncKeyword or
            SyntaxKind.InitKeyword or
            SyntaxKind.SelfKeyword or
            SyntaxKind.CaseKeyword or
            SyntaxKind.IdentifierToken;
    }
}
