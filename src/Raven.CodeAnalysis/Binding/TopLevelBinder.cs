using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class TopLevelBinder : BlockBinder
{
    public TopLevelBinder(Binder parent) : base(parent) { }
}
