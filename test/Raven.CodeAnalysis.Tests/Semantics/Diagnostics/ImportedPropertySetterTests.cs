using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

public class ImportedPropertySetterTests
{
    [Theory]
    [InlineData("value[0] = 1", true)]
    [InlineData("value[0] += 1", true)]
    [InlineData("value[\"key\"] = 1", false)]
    [InlineData("value.Restricted = 1", true)]
    [InlineData("value.Restricted += 1", true)]
    [InlineData("value.Restricted++", true)]
    [InlineData("++value.Restricted", true)]
    [InlineData("1 |> value.Restricted", true)]
    [InlineData("1 |> value.Writable", false)]
    [InlineData("value.Writable = 1", false)]
    [InlineData("value.Writable += 1", false)]
    [InlineData("value.Writable++", false)]
    public void ImportedSetterAccessibilityIsChecked(string statement, bool rejects)
    {
        var tree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.Semantics.Diagnostics.*
            func Update(value: ImportedPropertyFixture) -> int {
            """ + "\n" + statement + "\nreturn value.Restricted\n}");
        var compilation = Compilation.Create("SetterConsumer", [tree],
            TestMetadataReferences.Default.Concat([MetadataReference.CreateFromFile(typeof(ImportedPropertyFixture).Assembly.Location)]).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (rejects && statement.Contains('['))
            Assert.NotEmpty(errors);
        else if (rejects)
            Assert.Contains(errors, d => d.Descriptor == CompilerDiagnostics.PropertyOrIndexerCannotBeAssignedIsReadOnly);
        else
            Assert.Empty(errors);
    }
}

public class ImportedPropertyFixture
{
    public int this[int index] { get => 0; private set { } }
    public int this[string index] { get => 0; set { } }
    public int Restricted { get; private set; }
    public int Writable { get; set; }
}
