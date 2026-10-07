namespace Raven.CodeAnalysis.Tests;

public sealed class ErasedValueOwnershipTests
{
    [Theory]
    [InlineData("Library", true)]
    [InlineData("Other", false)]
    public void CarrierOwnerMustBeDeclaredAndIsNotACliSpecialType(string owner, bool valid)
    {
        var path = Path.GetTempFileName();
        try
        {
            File.WriteAllText(path, $$$"""
                {"version":1,"libraries":[{"assemblyName":"Library","sources":["Value.rvn"],"types":["System.Value"]}],
                 "nativePrimitives":{"System.Value":"{{{owner}}}"}}
                """);
            if (!valid)
            {
                Assert.Throws<InvalidDataException>(() => BootstrapOwnershipManifest.Read(path));
                return;
            }
            var manifest = BootstrapOwnershipManifest.Read(path);
            foreach (var output in new[] { "Library", "Consumer" })
            {
                var options = manifest.Apply(CompilationOptions.NeoCLR, output, "Core");
                Assert.Empty(options.MetadataImportOptions!.PrimitiveAssemblies);
            }
            Assert.Throws<InvalidDataException>(() => manifest.Apply(new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), "Library", "Core"));
        }
        finally { File.Delete(path); }
    }
}
