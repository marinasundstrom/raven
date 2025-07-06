using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ExpressionSyntax : ExpressionOrPatternSyntax
{
    internal class Missing : ExpressionSyntax
    {
        public Missing(SyntaxKind kind = SyntaxKind.None,
            IEnumerable<DiagnosticInfo>? diagnostics = null)
            : base(kind, [], diagnostics)
        {
            LeadingTrivia = SyntaxTriviaList.Empty;
            TrailingTrivia = SyntaxTriviaList.Empty;
        }

        public override bool IsMissing => true;

        public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
        {
            return new Syntax.ExpressionSyntax.Missing(this, parent, position);
        }

        internal override void Accept(SyntaxVisitor visitor)
        {
            visitor.Visit(this);
        }

        internal override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
        {
            return visitor.Visit(this);
        }
    }
}
