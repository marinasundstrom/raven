namespace Raven.ExtensionMethodsFixture;

public static class ScopedParameterFixture
{
    public static void Consume(scoped Span<int> value)
    {
    }

    public static void ConsumeRef(scoped ref int value)
    {
    }

    public static void ConsumeUnscoped(Span<int> value)
    {
    }
}
