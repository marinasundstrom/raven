using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Collections.Generic;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Markdig;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

class Program
{
    private const string TargetFramework = "net9.0";
    private static readonly string rootdir = "_docs";

    static void Main()
    {
        try { Directory.Delete(rootdir, recursive: true); } catch { }
        try { Directory.CreateDirectory(rootdir); } catch { }

        Docs();
    }

    static void Docs()
    {
        string sourceCode = """
        namespace Samples

        import System.Console.*
        import System.Collections.Generic.List<>
        alias StringList = System.Collections.Generic.List<string>
        alias PrintLine = System.Console.WriteLine

        WriteLine("Hello"); val x = 2

        val user = Person(42)
        user.AddRole("admin")

        val user2 = Person.WithName("John")
            .AddRole("admin");

        PrintLine(user2.Name)

        PrintLine(user2(2003));
        PrintLine(user2("test"));

        open class Base {}

        class Person : Base {
            val species = "Homo sapiens"
            var age: int = 0
            var name: string
            var roles: StringList = []

            // Primary constructor
            public init(age: int) {
                self.age = age
            }

            // Named constructor
            public init WithName(name: string) {
                self.name = name
            }

            // Regular method
            public AddRole(role: string) -> Person {
                roles.Add(role)
                self
            }

            public Test() -> int => 2

            public Test2: int => 2

            // Computed property
            public Name: string {
                get {
                    name
                }
                set {
                    name = value
                }
            }

            // Indexer: e.g., person[0]
            public self[index: int]: string {
                get => roles[index];
                set => roles[index] = value
            }

            /// Invocation operator: e.g., person(2025)
            public self(year: int) -> string {
                "Name: $name, Age in $year: ${year - (System.DateTime.Now.Year - age)}"
            }

            public self(str: string) {

            }
        }
        """;

        SyntaxTree syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceCode);

        var compilation = Compilation.Create("test", [syntaxTree],
            options: new CompilationOptions(OutputKind.ConsoleApplication));

        var version = TargetFrameworkResolver.ResolveVersion(TargetFramework);
        var refDir = TargetFrameworkResolver.GetDirectoryPath(version);

        var references = new[]
        {
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.dll")),
            MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Collections.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.Extensions.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.IO.FileSystem.dll")),
        };

        compilation = compilation.AddReferences(references);

        compilation.GetDiagnostics();

        var tree = compilation.SyntaxTrees.First();
        var sem = compilation.GetSemanticModel(tree);

        var globalNamespace = compilation.GetSourceGlobalNamespace();

        ProcessSymbol(globalNamespace);
    }

    private static void ProcessSymbol(ISymbol symbol)
    {
        if (symbol is ITypeSymbol typeSymbol)
        {
            GenerateTypePage(typeSymbol);
        }
        else if (symbol is INamespaceSymbol namespaceSymbol)
        {
            GenerateNamespacePage(namespaceSymbol);
        }

        if (symbol is INamespaceOrTypeSymbol namespaceOrTypeSymbol)
        {
            foreach (var member in namespaceOrTypeSymbol.GetMembers())
            //.Where(x => x.DeclaredAccessibility == Accessibility.Public))
            {
                ProcessSymbol(member);
            }
        }
    }

    // ----------------------------
    // Path + link helpers
    // ----------------------------

    private static string RootDir => rootdir;

    private static string ToUrlPath(string path)
        => path.Replace(Path.DirectorySeparatorChar, '/');

    private static string GetNamespaceDir(INamespaceSymbol ns)
    {
        if (ns is null || ns.IsGlobalNamespace)
            return RootDir;

        var segments = new Stack<string>();
        var cur = ns;
        while (cur is not null && !cur.IsGlobalNamespace)
        {
            if (!string.IsNullOrWhiteSpace(cur.Name))
                segments.Push(cur.Name);
            cur = cur.ContainingNamespace;
        }

        return Path.Combine(new[] { RootDir }.Concat(segments).ToArray());
    }

    private static string GetTypeDir(ITypeSymbol type)
    {
        var nsDir = GetNamespaceDir(type.ContainingNamespace);

        var segments = new Stack<string>();
        var cur = type;
        while (cur is not null)
        {
            segments.Push(cur.Name);
            cur = cur.ContainingType;
        }

        return Path.Combine(new[] { nsDir }.Concat(segments).ToArray());
    }

    private static string GetNamespaceIndexPath(INamespaceSymbol ns)
        => Path.Combine(GetNamespaceDir(ns), "index.html");

    private static string GetTypeIndexPath(ITypeSymbol type)
        => Path.Combine(GetTypeDir(type), "index.html");

    private static string GetMemberPath(ISymbol member)
    {
        var typeDir = GetTypeDir(member.ContainingType!);
        return Path.Combine(typeDir, member.Name + ".html");
    }

    private static void EnsureDirForFile(string filePath)
    {
        var dir = Path.GetDirectoryName(filePath);
        if (!string.IsNullOrWhiteSpace(dir))
            Directory.CreateDirectory(dir);
    }

    private static string RelLink(string fromDirectory, string toFileOrDirectory)
    {
        var rel = Path.GetRelativePath(fromDirectory, toFileOrDirectory);
        return ToUrlPath(rel);
    }

    // ----------------------------
    // Page generators
    // ----------------------------

    private static void GenerateTypePage(ITypeSymbol typeSymbol)
    {
        var comment = typeSymbol.GetDocumentationComment();

        var indexPath = GetTypeIndexPath(typeSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        var sb = new StringBuilder();

        sb.AppendLine($"# {typeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
        if (typeSymbol.BaseType is not null)
        {
            var baseTypeSymbol = typeSymbol.BaseType;
            var target = GetTypeIndexPath(baseTypeSymbol);
            sb.AppendLine($"**Base type**: [{baseTypeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})<br />");
        }
        if (typeSymbol.ContainingType is not null)
        {
            var containingType = typeSymbol.ContainingType!;
            var target = GetTypeIndexPath(containingType);
            sb.AppendLine($"**Containing type**: [{containingType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})<br />");
        }
        if (typeSymbol.ContainingNamespace is not null)
        {
            var containingNamespace = typeSymbol.ContainingNamespace!;
            var target = GetNamespaceIndexPath(containingNamespace);
            sb.AppendLine($"**Namespace**: [{containingNamespace.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})<br />");
        }
        sb.AppendLine();

        if (comment is not null)
            sb.Append(comment.Content);

        sb.AppendLine();
        sb.AppendLine("## Members");

        foreach (var member in typeSymbol.GetMembers()
            .Where(x => x is not IMethodSymbol ms || ms.AssociatedSymbol is null).OrderBy(x => x.Name))
        //.Where(x => x.DeclaredAccessibility == Accessibility.Public))
        {
            if (member is ITypeSymbol nestedType)
            {
                // Nested type -> its own folder + index.html
                var target = GetTypeIndexPath(nestedType);
                sb.AppendLine($"* [{nestedType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})");
                GenerateTypePage(nestedType);
            }
            else
            {
                // Normal member -> <TypeDir>/<MemberName>.html
                var target = GetMemberPath(member);
                sb.AppendLine($"* [{member.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})");
                GenerateMemberPage(member);
            }
        }

        File.WriteAllText(indexPath, Markdown.ToHtml(sb.ToString()));
    }

    private static void GenerateMemberPage(ISymbol member)
    {
        var comment = member.GetDocumentationComment();

        var filePath = GetMemberPath(member);
        EnsureDirForFile(filePath);
        var currentDir = Path.GetDirectoryName(filePath)!;

        var sb = new StringBuilder();

        sb.AppendLine($"# {member.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
        if (member.ContainingType is not null)
        {
            var containingType = member.ContainingType!;
            var target = GetTypeIndexPath(containingType);
            sb.AppendLine($"**Type**: [{containingType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})<br />");
        }
        if (member.ContainingNamespace is not null)
        {
            var containingNamespace = member.ContainingNamespace!;
            var target = GetNamespaceIndexPath(containingNamespace);
            sb.AppendLine($"**Namespace**: [{containingNamespace.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})<br />");
        }
        sb.AppendLine();

        if (comment is not null)
            sb.Append(comment.Content);

        File.WriteAllText(filePath, Markdown.ToHtml(sb.ToString()));
    }

    private static void GenerateNamespacePage(INamespaceSymbol namespaceSymbol)
    {
        var comment = namespaceSymbol.GetDocumentationComment();

        var indexPath = GetNamespaceIndexPath(namespaceSymbol);
        EnsureDirForFile(indexPath);
        var currentDir = Path.GetDirectoryName(indexPath)!;

        var sb = new StringBuilder();

        sb.AppendLine($"# {namespaceSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
        sb.AppendLine();

        if (comment is not null)
            sb.Append(comment.Content);

        sb.AppendLine();
        sb.AppendLine("## Members");

        foreach (var member in namespaceSymbol.GetMembers()
            .DistinctBy(x => x.Name)
            .OrderBy(x => x.Name)
            .Where(x => x.Locations.Any(x => x.IsInSource)))
        //.Where(x => x.DeclaredAccessibility == Accessibility.Public))
        {
            if (member is INamespaceSymbol ns2)
            {
                // Namespace -> its folder + index.html
                var target = GetNamespaceIndexPath(ns2);
                sb.AppendLine($"* [{ns2.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})");
                GenerateNamespacePage(ns2);
            }
            else if (member is ITypeSymbol t2)
            {
                // Type -> its folder + index.html
                var target = GetTypeIndexPath(t2);
                sb.AppendLine($"* [{t2.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})");
                GenerateTypePage(t2);
            }
            else
            {
                // Usually not applicable, but safe
                var target = GetMemberPath(member);
                sb.AppendLine($"* [{member.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly))}]({RelLink(currentDir, target)})");
                GenerateMemberPage(member);
            }
        }

        File.WriteAllText(indexPath, Markdown.ToHtml(sb.ToString()));
    }
}
