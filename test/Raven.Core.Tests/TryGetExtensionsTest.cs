using System.Reflection;

using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public sealed class TryGetExtensionsTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void DictionaryTryGetValueProjection_BindsFromRavenCore()
    {
        const string code = """
import System.*
import System.Collections.Generic.*

func Find(values: Dictionary<string, int>, key: string) -> Option<int> {
    return values.TryGetValue(key)
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void DictionaryTryGetValueProjection_DistinguishesMissingAndPresentNull()
    {
        var assembly = LoadRavenCoreAssembly();
        var extensions = assembly.GetType("System.Collections.Generic.DictionaryExtensions", throwOnError: true)!;
        var tryGetValue = extensions.GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Single(method => method.Name == "TryGetValue")
            .MakeGenericMethod(typeof(string), typeof(string));
        var values = new Dictionary<string, string?>
        {
            ["value"] = "hello",
            ["null"] = null
        };

        var present = tryGetValue.Invoke(null, [values, "value"])!;
        var presentNull = tryGetValue.Invoke(null, [values, "null"])!;
        var missing = tryGetValue.Invoke(null, [values, "missing"])!;

        Assert.Equal("hello", GetSomePayload(present));
        Assert.Null(GetSomePayload(presentNull));
        var none = missing.GetType().GetProperty("Value")!.GetValue(missing);
        Assert.NotNull(none);
        Assert.Contains("None", none!.GetType().Name, StringComparison.Ordinal);
    }

    private static object? GetSomePayload(object option)
    {
        var some = option.GetType().GetProperty("Value")!.GetValue(option);
        Assert.NotNull(some);
        return some!.GetType().GetProperty("Value")!.GetValue(some);
    }
}
