using System;

namespace Raven.CodeAnalysis.Macros;

[Flags]
public enum MacroTarget
{
    None = 0,
    Type = 1 << 0,
    Method = 1 << 1,
    Property = 1 << 2,
    Field = 1 << 3,
    Event = 1 << 4,
    Parameter = 1 << 5,
    Accessor = 1 << 6,
    Constructor = 1 << 7,
}
