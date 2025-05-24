namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class TypeSyntax : ExpressionSyntax
{
    protected TypeSyntax(SyntaxKind kind, GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}


internal abstract partial class NameSyntax : TypeSyntax
{
    protected NameSyntax(SyntaxKind kind, GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}

internal abstract partial class SimpleNameSyntax : NameSyntax
{
    protected SimpleNameSyntax(SyntaxKind kind, GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}

internal partial class IdentifierNameSyntax : SimpleNameSyntax
{
    public IdentifierNameSyntax(SyntaxToken nameToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.IdentifierName,
        [
            nameToken ?? throw new ArgumentNullException(nameof(nameToken))
        ],
        diagnostics)
    {
    }
}

internal partial class GenericNameSyntax : SimpleNameSyntax
{
    public GenericNameSyntax(SyntaxToken nameToken, TypeArgumentListSyntax typeArgumentList,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.GenericName,
        [
            nameToken ?? throw new ArgumentNullException(nameof(nameToken)),
            typeArgumentList  ?? throw new ArgumentNullException(nameof(typeArgumentList))
        ],
     diagnostics)
    {
    }
}

internal partial class QualifiedNameSyntax : NameSyntax
{
    public QualifiedNameSyntax(NameSyntax left, SyntaxToken dotToken, SimpleNameSyntax name,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.QualifiedName,
        [
            left ?? throw new ArgumentNullException(nameof(left)),
            dotToken ?? throw new ArgumentNullException(nameof(dotToken)),
            name ?? throw new ArgumentNullException(nameof(name))
        ],
        diagnostics)
    {
    }
}

internal partial class AliasQualifiedNameSyntax : NameSyntax
{
    public AliasQualifiedNameSyntax(IdentifierNameSyntax alias, SyntaxToken colonColonToken, SimpleNameSyntax name,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.AliasQualifiedName,
        [
            alias ?? throw new ArgumentNullException(nameof(alias)),
            colonColonToken  ?? throw new ArgumentNullException(nameof(colonColonToken)),
            name  ?? throw new ArgumentNullException(nameof(name))
        ],
        diagnostics)
    {
    }
}


internal partial class NullableTypeSyntax : TypeSyntax
{
    public NullableTypeSyntax(TypeSyntax elementType, SyntaxToken questionToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.NullableType,
        [
            elementType ?? throw new ArgumentNullException(nameof(elementType)),
            questionToken  ?? throw new ArgumentNullException(nameof(questionToken)),
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static IdentifierNameSyntax IdentifierName(
        SyntaxToken nameToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(nameToken, diagnostics);

    public static GenericNameSyntax GenericName(
        SyntaxToken nameToken,
        TypeArgumentListSyntax typeArgumentList,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(nameToken, typeArgumentList, diagnostics);

    public static QualifiedNameSyntax QualifiedName(
        NameSyntax left,
        SyntaxToken dotToken,
        SimpleNameSyntax name,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(left, dotToken, name, diagnostics);

    public static AliasQualifiedNameSyntax AliasQualifiedName(
        IdentifierNameSyntax alias,
        SyntaxToken colonColonToken,
        SimpleNameSyntax name,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(alias, colonColonToken, name, diagnostics);

    public static NullableTypeSyntax NullableType(
        TypeSyntax elementType,
        SyntaxToken questionToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(elementType, questionToken, diagnostics);
}