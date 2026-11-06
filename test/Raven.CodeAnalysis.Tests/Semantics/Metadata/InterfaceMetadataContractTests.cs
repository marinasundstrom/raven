using System.Reflection;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class InterfaceMetadataContractTests
{
    public interface IContract<T> { }
    public class Box<T> : IContract<T?> where T : class { }
    public class Nested : IContract<Dictionary<string, string?[]?>?> { }

    [Theory]
    [InlineData(typeof(Nested))]
    [InlineData(typeof(Box<>))]
    public void DotNet_ReadsDirectInterfaceTypesAndNullableFlags(Type fixture)
    {
        var path = fixture.Assembly.Location;
        var paths = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!).Split(Path.PathSeparator);
        using var context = new MetadataLoadContext(new PathAssemblyResolver(paths.Append(path)), typeof(object).Assembly.GetName().Name);
        var assembly = context.LoadFromAssemblyPath(path);
        var type = assembly.GetType(fixture.FullName!, true)!.GetTypeInfo();
        var decoder = new DotNetInterfaceMetadata();
        Assert.True(decoder.Read(type, context).IsDefault);

        decoder.Capture(assembly, File.ReadAllBytes(path));
        var declarations = decoder.Read(type, context);
        var declaration = Assert.Single(declarations);
        Assert.Equal(type.GetInterfaces().Single(), declaration.Type);
        Assert.Contains((byte)2, declaration.NullableFlags);
        Assert.Equal(declarations, decoder.Read(type, context));

        if (fixture == typeof(Nested))
            Assert.Equal(new byte[] { 0, 2, 1, 2, 2 }, declaration.NullableFlags.ToArray());
    }
}
