﻿namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxToken : GreenNode
{
    private readonly object _value;

    public string Text => GetValueText()!;

    public SyntaxToken(
        SyntaxKind kind,
        string text,
        SyntaxTriviaList leadingTrivia = null,
        SyntaxTriviaList trailingTrivia = null)
    : this(kind, text, text.Length, leadingTrivia, trailingTrivia)
    {

    }

    public SyntaxToken(
        SyntaxKind kind,
        object value,
        int width,
        SyntaxTriviaList leadingTrivia = null,
        SyntaxTriviaList trailingTrivia = null)
        : base(kind, 0,
        width,
        (leadingTrivia?.FullWidth ?? 0) + width + (trailingTrivia?.FullWidth ?? 0))
    {
        _value = value;
        LeadingTrivia = leadingTrivia ?? SyntaxTriviaList.Empty;
        TrailingTrivia = trailingTrivia ?? SyntaxTriviaList.Empty;
    }

    public bool IsMissing { get; private set; }

    public override GreenNode GetSlot(int index) => throw new InvalidOperationException("SyntaxToken has no children.");

    public override object? GetValue() => _value;

    public override string? GetValueText() => _value.ToString();

    public SyntaxToken WithLeadingTrivia(IEnumerable<SyntaxTrivia> trivias)
    {
        return new SyntaxToken(Kind, Text, SyntaxTriviaList.Create(trivias.ToArray()), TrailingTrivia); ;
    }

    public SyntaxToken WithTrailingTrivia(IEnumerable<SyntaxTrivia> trivias)
    {
        return new SyntaxToken(Kind, Text, LeadingTrivia, SyntaxTriviaList.Create(trivias.ToArray())); ;
    }

    internal static SyntaxToken Missing(SyntaxKind kind)
    {
        return new SyntaxToken(kind, string.Empty) { IsMissing = true };
    }

    public static explicit operator Syntax.SyntaxToken(InternalSyntax.SyntaxToken token)
    {
        return new Syntax.SyntaxToken(token, null!);
    }
}