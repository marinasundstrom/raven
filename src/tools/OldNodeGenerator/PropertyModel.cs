namespace Raven.Generators;

public class PropertyModel
{
    public string Name { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
    public bool Nullable { get; set; }
    public bool Inherited { get; set; }
    public bool Abstract { get; set; }
}
