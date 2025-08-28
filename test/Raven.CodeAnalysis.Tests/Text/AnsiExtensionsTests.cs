using Raven.CodeAnalysis.Text;
using Xunit;

namespace Raven.CodeAnalysis.Text.Tests;

public class AnsiExtensionsTests
{
    [Fact]
    public void StripAnsiCodes_RemovesEscapeSequences()
    {
        var text = "\u001b[31mhello\u001b[0m";
        var result = text.StripAnsiCodes();
        Assert.Equal("hello", result);
    }
}
