namespace Raven.CodeAnalysis;

// Names shared by compiler conveniences for the platform Optional convention.
// Ordinary user unions and CLR optional parameters have independent semantics.
internal static class OptionalFacts
{
    public const string TypeName = "Optional";
    public const string QualifiedTypeName = "System." + TypeName;
    public const string MetadataName = QualifiedTypeName + "`1";
    public const string SomeCaseName = "Some";
    public const string NoneCaseName = "None";
}
