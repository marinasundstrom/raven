using System.Security.Cryptography;
using System.Text;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Text.Tests;

[Trait("Target", "DotNet")]
public sealed class SourceTextIOTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FromStream_PreservesContentAndEncodingAndDisposesStream(bool unicode)
    {
        var encoding = unicode ? Encoding.Unicode : Encoding.UTF8;
        const string content = "Raven 🐦\r\ncompiler";
        var stream = new MemoryStream(encoding.GetBytes(content));
        var text = SourceText.From(stream, encoding);

        Assert.Equal(content, text.ToString());
        Assert.Same(encoding, text.Encoding);
        Assert.False(stream.CanRead);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Checksums_HashTheSelectedEncodingWithoutMutatingSnapshot(bool unicode)
    {
        var encoding = unicode ? Encoding.Unicode : Encoding.UTF8;
        const string content = "Raven 🐦\n";
        var text = SourceText.From(content, encoding);
        var expected = SHA256.HashData(encoding.GetBytes(content));

        Assert.Equal(expected, text.GetChecksum().ToArray());
        Assert.Equal(expected, text.GetContentHash().ToArray());
        Assert.Equal(content, text.ToString());
        Assert.NotEqual(expected, text.Replace(0, 1, "r").GetChecksum().ToArray());
    }

    [Fact]
    public void FromStream_UsesUtf8ByDefault()
    {
        using var stream = new MemoryStream(Encoding.UTF8.GetBytes("å🐦"));
        var text = SourceText.From(stream);
        Assert.Equal("å🐦", text.ToString());
        Assert.Same(Encoding.UTF8, text.Encoding);
    }

    [Fact]
    public void LineCollection_SupportsGenericAndNonGenericEnumeration()
    {
        var text = SourceText.From("first\nsecond");
        var lines = text.GetLines();
        var generic = lines.ToArray();
        var nongeneric = ((System.Collections.IEnumerable)lines).Cast<TextLine>().ToArray();

        Assert.Equal(new[] { "first", "second" }, generic.Select(line => line.ToString()));
        Assert.Equal(generic, nongeneric);
        Assert.Same(generic[1], lines[1]);
    }
}
