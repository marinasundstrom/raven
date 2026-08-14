namespace Raven.CodeAnalysis.Testing;

public static class TestTargetFramework
{
#if NET11_0
    public const string Default = "net11.0";
#else
    public const string Default = "net10.0";
#endif
}
