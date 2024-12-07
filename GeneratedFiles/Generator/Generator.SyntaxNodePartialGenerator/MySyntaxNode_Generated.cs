using System;

namespace Raven.CodeAnalysis.Syntax
{
    public partial class MySyntaxNode
    {
        Raven.CodeAnalysis.Syntax.SyntaxNode _exampleProperty;
        public partial Raven.CodeAnalysis.Syntax.SyntaxNode ExampleProperty
        {
            get
            {
                return default;
            }
        }

        public override SyntaxNode? GetNodeSlot(int index)
        {
            return (SyntaxNode? )(index switch
            {
                0 => ((SyntaxNode)this).GetRed<SyntaxNode>(ref _exampleProperty, 0),
                _ => null
            });
        }
    }
}