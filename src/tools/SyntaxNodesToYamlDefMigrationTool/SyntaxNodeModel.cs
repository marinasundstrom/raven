public class SyntaxNodeModel
{
    public string Name { get; set; } = string.Empty;
    public string Base { get; set; } = string.Empty;
    public bool Abstract { get; set; }
    public bool ExplicitKind { get; set; }
    public List<PropertyModel> Properties { get; set; } = new();
}
