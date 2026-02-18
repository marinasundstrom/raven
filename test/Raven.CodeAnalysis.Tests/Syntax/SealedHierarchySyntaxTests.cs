using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class SealedHierarchySyntaxTests
{
    [Fact]
    public void Parse_SealedClass_WithPermitsClause()
    {
        var source = """
sealed class Expr permits Lit, Add {}
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var classDecl = Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        Assert.NotNull(classDecl.PermitsClause);
        Assert.Equal(2, classDecl.PermitsClause!.Types.Count);
    }

    [Fact]
    public void Parse_SealedClass_WithoutPermitsClause()
    {
        var source = """
sealed class Expr {}
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var classDecl = Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        Assert.Null(classDecl.PermitsClause);
    }

    [Fact]
    public void Parse_SealedRecordClass_WithPermitsClause()
    {
        var source = """
sealed record class Expr permits Lit, Add {}
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var recordDecl = Assert.IsType<RecordDeclarationSyntax>(root.Members[0]);
        Assert.NotNull(recordDecl.PermitsClause);
        Assert.Equal(2, recordDecl.PermitsClause!.Types.Count);
    }

    [Fact]
    public void Parse_SealedRecordClass_WithoutPermitsClause()
    {
        var source = """
sealed record class Expr {}
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var recordDecl = Assert.IsType<RecordDeclarationSyntax>(root.Members[0]);
        Assert.Null(recordDecl.PermitsClause);
    }

    [Fact]
    public void BodylessClass_ParsesSuccessfully()
    {
        var source = """
class Marker
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var classDecl = Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        Assert.Equal("Marker", classDecl.Identifier.Text);
    }

    [Fact]
    public void BodylessStruct_ParsesSuccessfully()
    {
        var source = """
struct Unit
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var structDecl = Assert.IsType<StructDeclarationSyntax>(root.Members[0]);
        Assert.Equal("Unit", structDecl.Identifier.Text);
    }

    [Fact]
    public void BodylessInterface_ParsesSuccessfully()
    {
        var source = """
interface IMarker
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var ifaceDecl = Assert.IsType<InterfaceDeclarationSyntax>(root.Members[0]);
        Assert.Equal("IMarker", ifaceDecl.Identifier.Text);
    }

    [Fact]
    public void BodylessRecordClass_ParsesSuccessfully()
    {
        var source = """
record class Event
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var recordDecl = Assert.IsType<RecordDeclarationSyntax>(root.Members[0]);
        Assert.Equal("Event", recordDecl.Identifier.Text);
    }
}
