namespace Raven.MetadataFixtures.StaticExtensions;

public sealed class Widget
{
    public Widget(int value)
    {
        Value = value;
    }

    public int Value { get; }
}

public static class WidgetExtensions
{
    public static int Double(this Widget widget)
        => widget.Value * 2;

    public static Widget Create(int value)
        => new Widget(value);
}
