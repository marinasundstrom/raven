namespace NodesShared;

public class SyntaxNodeModel
{
    public string Name { get; set; } = string.Empty;
    public string Inherits { get; set; } = string.Empty;
    public bool IsAbstract { get; set; }
    public bool HasExplicitKind { get; set; }
    public List<SlotModel> Slots { get; set; } = new();
}

public class SlotModel
{
    public string Name { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
    public string? ElementType { get; set; }
    public string? DefaultToken { get; set; }

    public string FullTypeName
    {
        get
        {
            if (Type == "List" || Type == "SeparatedList")
            {
                return $"{Type}<{ElementType}>";
            }

            return Type;
        }
    }

    public bool IsNullable { get; set; }
    public bool IsInherited { get; set; }
    public bool IsAbstract { get; set; }
}