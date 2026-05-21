using OmniSharp.Extensions.LanguageServer.Protocol;

using Raven.LanguageServer;

using Shouldly;

namespace Raven.Editor.Tests;

public sealed class LatestDocumentRequestTrackerTests
{
    [Fact]
    public void Begin_CancelsPreviousRequestForSameDocument()
    {
        var tracker = new LatestDocumentRequestTracker();
        var uri = DocumentUri.From("file:///tmp/main.rvn");

        var first = tracker.Begin(uri, CancellationToken.None);
        var second = tracker.Begin(uri, CancellationToken.None);

        first.IsSuperseded.ShouldBeTrue();
        first.Token.IsCancellationRequested.ShouldBeTrue();
        second.IsSuperseded.ShouldBeFalse();
        second.Token.IsCancellationRequested.ShouldBeFalse();

        tracker.Complete(second);
        tracker.Complete(first);
    }

    [Fact]
    public void Begin_DoesNotCancelCompletedRequestForSameDocument()
    {
        var tracker = new LatestDocumentRequestTracker();
        var uri = DocumentUri.From("file:///tmp/main.rvn");

        var first = tracker.Begin(uri, CancellationToken.None);
        tracker.Complete(first);

        var second = tracker.Begin(uri, CancellationToken.None);

        first.IsSuperseded.ShouldBeFalse();
        second.IsSuperseded.ShouldBeFalse();
        second.Token.IsCancellationRequested.ShouldBeFalse();

        tracker.Complete(second);
    }
}
