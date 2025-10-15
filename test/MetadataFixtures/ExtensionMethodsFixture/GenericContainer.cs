using System;

namespace Raven.MetadataFixtures.Generics;

public class GenericContainer<TBase>
{
    public static TBase Coerce<TDerived>(TDerived value)
        where TDerived : TBase
    {
        return value;
    }
}
