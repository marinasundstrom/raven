using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public static class RavenPrelude
{
    public const string DefaultSource = """
        global {
            import System.*
            import System.Collections.*
            import System.Collections.Generic.*
            import System.IO.*
            import System.Linq.*
            import System.Net.Http.*
            import System.Threading.*
            import System.Threading.Tasks.*
            import System.Result.*
            import System.Option.*
        }
        """;

    public static SourceText CreateDefaultSourceText()
        => SourceText.From(DefaultSource);
}
