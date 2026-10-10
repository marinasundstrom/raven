using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FriendAssemblyAccessTests
{
    [Theory]
    [InlineData("FriendConsumer", true)]
    [InlineData("UnrelatedConsumer", false)]
    public void RepeatedFriendDecisionsDoNotAllocateOrChangeAcrossThreads(string name, bool expected)
    {
        var provider = Create("CachedProvider", Library);
        var consumer = Create(name, "public class Consumer {}");
        Assert.Empty(provider.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Empty(consumer.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var declaring = provider.Assembly;
        var requesting = consumer.Assembly;
        System.Threading.Tasks.Parallel.For(0, 100, _ =>
            Assert.Equal(expected, FriendAssemblyAccess.IsGranted(declaring, requesting)));
        for (var i = 0; i < 1000; i++)
            FriendAssemblyAccess.IsGranted(declaring, requesting);
        var before = GC.GetAllocatedBytesForCurrentThread();
        var granted = 0;
        for (var i = 0; i < 100000; i++)
            if (FriendAssemblyAccess.IsGranted(declaring, requesting)) granted++;
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        Assert.Equal(expected ? 100000 : 0, granted);
        Assert.Equal(0, allocated);
        // The same assembly name in a different snapshot must not reuse a grant.
        var otherProvider = Create("CachedProvider", "public class Provider {}");
        Assert.Empty(otherProvider.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.False(FriendAssemblyAccess.IsGranted(otherProvider.Assembly, requesting));
    }

    private const string Library = """
import System.Runtime.CompilerServices.*
[assembly: InternalsVisibleTo("FriendConsumer")]
[assembly: InternalsVisibleTo("SecondFriend")]
namespace FriendLibrary
internal class Hidden {
    internal init() {}
    internal var Number: int = 40
    internal val Next: int => Number + 1
    internal func Answer() -> int => Next + 1
}
public class Visible {
    private static func Secret() -> int => 7
    internal static func InternalAnswer() -> int => 42
}
""";

    [Theory]
    [InlineData("FriendConsumer")]
    [InlineData("SecondFriend")]
    [InlineData("friendconsumer")]
    public void NamedFriendCanUseInternalTypeConstructorFieldPropertyAndMethod(string name)
    {
        var reference = TestMetadataFactory.CreateFileReferenceFromSource(Library, "FriendProvider");
        var compilation = Create(name, """
import FriendLibrary.*
public class Entry {
    public static func Run() -> int {
        let value = Hidden()
        value.Number = 40
        return value.Next + value.Answer()
    }
}
""", reference);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        Assert.Equal(83, loaded.Assembly.GetType("Entry")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("Unrelated", "Hidden().Answer()")]
    [InlineData("Unrelated", "Visible.InternalAnswer()")]
    [InlineData("FriendConsumer", "Visible.Secret()")]
    public void UnrelatedAndPrivateAccessRemainDenied(string name, string expression)
    {
        var reference = TestMetadataFactory.CreateFileReferenceFromSource(Library, "FriendProvider");
        var compilation = Create(name, "import FriendLibrary.*\npublic func Read() -> int => " + expression, reference);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0500");
    }

    [Theory]
    [InlineData("Friend", true)]
    [InlineData("friend", true)]
    [InlineData("Other", false)]
    [InlineData("Friend, Version=1.0.0.0", false)]
    [InlineData("Friend, Culture=neutral", false)]
    [InlineData("Friend, PublicKeyToken=null", false)]
    [InlineData("Friend, ProcessorArchitecture=MSIL", false)]
    [InlineData("Friend, PublicKey=garbage", false)]
    [InlineData("", false)]
    public void IdentityQualifiersNeverDegradeToSimpleNameMatching(string grant, bool expected)
    {
        Assert.Equal(expected, FriendAssemblyAccess.Matches(new AssemblyName("Provider"), new AssemblyName("Friend"), grant));
    }

    [Fact]
    public void SignedIdentitiesRequireTheFullMatchingPublicKey()
    {
        var key = typeof(object).Assembly.GetName().GetPublicKey()!;
        Assert.NotEmpty(key);
        var provider = new AssemblyName("Provider");
        provider.SetPublicKey(key);
        var friend = new AssemblyName("Friend");
        friend.SetPublicKey(key);
        var grant = "Friend, PublicKey=" + Convert.ToHexString(key);
        Assert.True(FriendAssemblyAccess.Matches(provider, friend, grant));
        Assert.False(FriendAssemblyAccess.Matches(provider, friend, "Friend"));
        Assert.False(FriendAssemblyAccess.Matches(provider, new AssemblyName("Friend"), grant));
        var otherKey = (byte[])key.Clone();
        otherKey[^1] ^= 1;
        friend.SetPublicKey(otherKey);
        Assert.False(FriendAssemblyAccess.Matches(provider, friend, grant));
    }

    [Fact]
    public void ImportedFriendAttributePreservesAssemblyUsage()
    {
        var compilation = Create("Usage", "public class C {}");
        var type = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.InternalsVisibleToAttribute")!;
        var usage = type.GetAttributes().Single(attribute => attribute.AttributeClass.Name == "AttributeUsageAttribute");
        Assert.Equal(1, Convert.ToInt32(usage.ConstructorArguments[0].Value));
        Assert.Contains(usage.NamedArguments, argument => argument.Key == "AllowMultiple" && Equals(argument.Value.Value, true));
    }

    [Fact]
    public void FriendshipIsNeitherReciprocalNorTransitive()
    {
        var provider = TestMetadataFactory.CreateFileReferenceFromSource(Library, "FriendProvider");
        var friend = TestMetadataFactory.CreateFileReferenceFromSource("""
import System.Runtime.CompilerServices.*
[assembly: InternalsVisibleTo("ThirdConsumer")]
namespace MiddleLibrary
public class Middle {
    internal static func Answer() -> int => 42
}
""", "FriendConsumer");
        var third = Create("ThirdConsumer", """
import FriendLibrary.*
public func Read() -> int => Visible.InternalAnswer()
""", provider, friend);
        Assert.Contains(third.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0500");
        var reverse = Create("FriendProvider", """
import System.Runtime.CompilerServices.*
[assembly: InternalsVisibleTo("FriendConsumer")]
import MiddleLibrary.*
public func Read() -> int => Middle.Answer()
""", friend);
        Assert.Contains(reverse.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0500");
    }

    [Fact]
    public void FriendAccessDoesNotMakeAnInternalTypePublic()
    {
        var reference = TestMetadataFactory.CreateFileReferenceFromSource(Library, "FriendProvider");
        var compilation = Create("FriendConsumer", """
import FriendLibrary.*
public func Read() -> Hidden => Hidden()
""", reference);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0501");
    }

    [Fact]
    public void AssemblyGrantCannotBeAppliedToAType()
    {
        var compilation = Create("InvalidTarget", """
import System.Runtime.CompilerServices.*
[InternalsVisibleTo("FriendConsumer")]
public class WrongTarget {}
""");
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0502");
    }

    [Fact]
    public void SourceAssemblyGrantsAreAvailableToSemanticAccessibility()
    {
        var provider = Create("SourceProvider", Library);
        Assert.DoesNotContain(provider.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var tree = provider.SyntaxTrees.Single();
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First();
        var hidden = Assert.IsAssignableFrom<INamedTypeSymbol>(provider.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.Same(provider.Assembly, hidden.ContainingAssembly);
        var friend = Create("FriendConsumer", "public class C {}");
        var other = Create("Other", "public class C {}");
        _ = friend.GetDiagnostics();
        _ = other.GetDiagnostics();
        Assert.NotNull(friend.Assembly);
        Assert.NotNull(other.Assembly);
        Assert.True(AccessibilityUtilities.IsAccessible(hidden, friend.Assembly));
        Assert.False(AccessibilityUtilities.IsAccessible(hidden, other.Assembly));
    }

    [Theory]
    [InlineData("FriendConsumer", true, "Both", true)]
    [InlineData("FriendConsumer", false, "Both", false)]
    [InlineData("Other", true, "Both", false)]
    [InlineData("FriendConsumer", false, "Either", true)]
    [InlineData("Other", true, "Either", true)]
    public void ProtectedCombinationsKeepTheirInheritanceRequirements(string name, bool derived, string member, bool allowed)
    {
        var reference = TestMetadataFactory.CreateFileReferenceFromSource("""
import System.Runtime.CompilerServices.*
[assembly: InternalsVisibleTo("FriendConsumer")]
public open class Parent {
    private protected func Both() -> int => 42
    protected internal func Either() -> int => 42
}
""", "ProtectionProvider");
        var source = derived
            ? "public class Child : Parent { public func Read() -> int => " + member + "() }"
            : "public func Read() -> int => Parent()." + member + "()";
        var errors = Create(name, source, reference).GetDiagnostics().Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ToArray();
        if (allowed)
            Assert.Empty(errors);
        else
            Assert.Contains(errors, diagnostic => diagnostic.Id == "RAV0500");
    }

    [Fact]
    public void QuotedSimpleNamesAreComparedAsAssemblyNames()
    {
        Assert.True(FriendAssemblyAccess.Matches(new AssemblyName("Provider"),
            new AssemblyName { Name = "Friend, Part" }, "\"Friend, Part\""));
    }

    private static Compilation Create(string name, string source, params MetadataReference[] references) =>
        Compilation.Create(name, [SyntaxTree.ParseText(source)], TestMetadataReferences.Default.Concat(references).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
}
