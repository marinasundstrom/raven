using System.Security.Cryptography;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

namespace NeoClrMetadataProbe;

internal static class NativeDocFixture
{
    internal static void Write(string corePath, string output)
    {
        Directory.CreateDirectory(output);
        var core = AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(corePath)).Identity;
        var models = new AssemblyBuilder(new("Models", new(1, 0, 0, 0)), core);
        var baseBook = models.AddClass("Example", "Book");
        baseBook.AddField("Code", PrimitiveType.Int32, FieldVisibility.Public);
        var getter = baseBook.AddInstanceMethod("get_Count", new(PrimitiveType.Int32, []));
        getter.GetILGenerator().LoadConstant(1);
        getter.GetILGenerator().Return();
        baseBook.AddProperty("Count", PrimitiveType.Int32, getter);
        models.AddClass("Example", "SpecialBook", baseBook);
        var box = models.AddGenericClass("Example", "Box", ["T"]);
        box.AddField("Value", SignatureType.TypeParameter(0), FieldVisibility.Public);
        var contract = models.AddGenericInterface("Example", "Reader", ["T"]);
        contract.AddInterfaceMethod("Read", new(SignatureType.TypeParameter(0), []));
        var bytes = RuntimeAssemblyContainer.WriteLibraryBinary(models);
        // Deliberately differ from the declaring identity: documentation must use Models.
        File.WriteAllBytes(Path.Combine(output, "Renamed.dll"), bytes);
        File.WriteAllText(Path.Combine(output, "Renamed.xml"), "<doc><members><member name=\"T:Example.Book\"><summary>A native book with preserved documentation.</summary></member></members></doc>");
        var api = new AssemblyBuilder(new("Library", new(1, 0, 0, 0)), core);
        var book = api.CreateTypeReference(models.Identity, core, Convert.ToHexString(SHA256.HashData(bytes)), "Example", "Book");
        api.AddConstant(new("Example", "Scale", 1.5));
        var function = api.AddFunction("Example", "Score", new(PrimitiveType.Int32, [PrimitiveType.Int32]));
        function.SetParameterName(0, "value");
        function.GetILGenerator().LoadArgument(0);
        function.GetILGenerator().Return();
        var global = api.AddFunction("GlobalScore", new(PrimitiveType.Int32, []));
        global.GetILGenerator().LoadConstant(42);
        global.GetILGenerator().Return();
        var books = api.AddType("Example", "Books");
        var method = books.AddMethod("Echo", new(book, [book]));
        method.SetParameterName(0, "book");
        var overload = books.AddMethod("Echo", new(PrimitiveType.Int32, [PrimitiveType.Int32]));
        overload.SetParameterName(0, "number");
        overload.GetILGenerator().LoadArgument(0);
        overload.GetILGenerator().Return();
        var marker = api.AddClass("System.Runtime.CompilerServices", "ExtensionAttribute", TypeVisibility.Internal)
            .AddConstructor(new MethodSignature(PrimitiveType.Void, []));
        marker.GetILGenerator().Return();
        var extensions = api.AddType("Example", "BookExtensions");
        extensions.AddCustomAttribute(new(marker.Definition, []));
        var extension = extensions.AddMethod("Rating", new(PrimitiveType.Int32, [book]));
        extension.SetParameterName(0, "self");
        extension.GetILGenerator().LoadConstant(5);
        extension.GetILGenerator().Return();
        method.GetILGenerator().LoadArgument(0);
        method.GetILGenerator().Return();
        File.WriteAllBytes(Path.Combine(output, "Library.dll"), RuntimeAssemblyContainer.WriteLibraryBinary(api));
        File.WriteAllText(Path.Combine(output, "Library.xml"), "<doc><members><member name=\"M:Example.Score(System.Int32)\"><summary>Assembly function documentation.</summary></member><member name=\"F:Example.Scale\"><summary>Assembly constant documentation.</summary></member><member name=\"M:Example.Books.Echo(Example.Book)\"><summary>Returns the supplied native book.</summary><param name=\"book\">The original book.</param><returns>The same book instance.</returns></member></members></doc>");
        var conflict = new AssemblyBuilder(new("Conflict", new(1, 0, 0, 0)), core);
        conflict.AddClass("Example", "Book");
        File.WriteAllBytes(Path.Combine(output, "Conflict.dll"), RuntimeAssemblyContainer.WriteLibraryBinary(conflict));
        Console.WriteLine("PASS native documentation fixture authored");
    }
}
