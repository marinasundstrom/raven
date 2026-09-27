using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests.Documentation;

public sealed class RavenDocGenerationTests : CompilationTestBase
{
    [Fact]
    public void NamespaceNavigationDoesNotSelectEveryFunctionInTheCurrentDirectory()
    {
        var (compilation, _) = CreateCompilation("namespace Functions\npublic func Abs(value: int) -> int => value\npublic func Cos(value: int) -> int => value", assemblyName: "NavigationFixture");
        compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ShouldBeEmpty();
        var output = Path.Combine(Path.GetTempPath(), "ravendoc-tests", Guid.NewGuid().ToString("N"));
        try
        {
            DocumentationGenerator.ProcessCompilation(compilation, output);
            var html = File.ReadAllText(Path.Combine(output, "Functions/index.html"));
            html.ShouldContain("href=\"index.html\" aria-current=\"location\"");
            html.ShouldContain("href=\"method_Abs.html\"");
            html.ShouldNotContain("href=\"method_Abs.html\" aria-current");
            html.ShouldNotContain("href=\"method_Cos.html\" aria-current");
        }
        finally { if (Directory.Exists(output)) Directory.Delete(output, true); }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MemberStructureComesFromSymbolsAndDocumentationEnhancesIt(bool metadata)
    {
        const string source = """
            namespace Contracts
            public class Item { }
            public class Box<T> { }
            public class Service {
                public val Current: Item => Item()
                public val Optional: Item? => null
                /// Wraps a value.
                /// @param value The item to wrap.
                /// @returns The wrapped item.
                public func Wrap(value: Item) -> Box<Item> => Box<Item>()
                public func Wrap(values: Item[]) -> Box<Item> => Box<Item>()
            }
            """;
        var (compilation, _) = CreateCompilation(source, assemblyName: "ContractsFixture");
        IAssemblySymbol? assembly = null;
        if (metadata)
        {
            var reference = (PortableExecutableReference)TestMetadataFactory.CreateFileReferenceFromSource(source, "ContractsFixture");
            File.WriteAllText(Path.ChangeExtension(reference.FilePath, ".xml"), """
                <doc><members><member name="M:Contracts.Service.Wrap(Contracts.Item)">
                <summary>Wraps a Box&lt;Item&gt;.</summary><param name="value">The item to wrap.</param>
                <returns>The wrapped item.</returns><remarks>Use Box&lt;Item&gt; &amp; Item.</remarks></member></members></doc>
                """);
            compilation = Compilation.Create("ContractHost", options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddReferences(TestMetadataReferences.Default).AddReferences(reference);
            _ = compilation.GetDiagnostics();
            assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
        }
        var output = Path.Combine(Path.GetTempPath(), "ravendoc-tests", Guid.NewGuid().ToString("N"));
        try
        {
            if (assembly is null) DocumentationGenerator.ProcessCompilation(compilation, output);
            else DocumentationGenerator.ProcessAssembly(compilation, assembly, output);
            var property = File.ReadAllText(Path.Combine(output, "Contracts/Service/property_Current.html"));
            property.ShouldContain("Declaring type");
            property.ShouldContain("id=\"property-value\"");
            property.ShouldContain("<a href=\"../Item/index.html\">Item</a>");
            var nullable = File.ReadAllText(Path.Combine(output, "Contracts/Service/property_Optional.html"));
            nullable.ShouldContain("Item</a>?");
            var method = File.ReadAllText(Path.Combine(output, "Contracts/Service/method_Wrap.html"));
            method.ShouldContain("<th>Type</th>");
            method.ShouldContain("<a href=\"../Item/index.html\">Item</a>");
            method.ShouldContain("Item</a>[]");
            method.ShouldContain("The item to wrap.");
            method.ShouldContain("The wrapped item.");
            if (metadata)
            {
                method.ShouldContain("Use Box&lt;Item&gt; &amp; Item.");
                var typePage = File.ReadAllText(Path.Combine(output, "Contracts/Service/index.html"));
                typePage.ShouldContain("Wraps a Box&lt;Item&gt;.");
                typePage.ShouldNotContain("Box&amp;lt;");
            }
            method.ShouldContain("<h4 id=\"return-value\">Return value</h4>");
            method.ShouldContain("Box</a>&lt;<a href=\"../Item/index.html\">Item</a>&gt;");
            method.Split("<th>Type</th>").Length.ShouldBe(3);
        }
        finally { if (Directory.Exists(output)) Directory.Delete(output, true); }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void TypeRelationshipsAndEnrichmentUseSemanticContracts(bool metadata)
    {
        const string source = """
            namespace Relationships
            public static class Factory { public static func Create() -> int => 1 }
            public interface IContract {
                func Run() -> int
                func DefaultRun() -> int => 8
            }
            public sealed class Base permits Derived {
                public func Read() -> int => 1
                public static func CreateCount() -> int => 0
                private func Hidden() -> int => 2
            }
            public class Derived : Base, IContract {
                public func Run() -> int => 3
            }
            public open class InheritedProvider : IContract {
                public virtual func Run() -> int => 5
            }
            public class InheritingProvider : InheritedProvider { }
            public class OverridingProvider : InheritedProvider {
                public override func Run() -> int => 6
            }
            public interface IChild : IContract { }
            public class ChildImplementation : IChild { public func Run() -> int => 9 }
            internal class HiddenImplementation : IContract { public func Run() -> int => 0 }
            public interface IGeneric<T> { func Echo(value: T) -> T }
            public class Generic<T> : IGeneric<T> { public func Echo(value: T) -> T => value }
            public class Concrete : IGeneric<int> { public func Echo(value: int) -> int => value }
            public extension GenericExtras<T> for IGeneric<T> {
                func Identity(value: T) -> T => value
                func Convert<U>(value: T, result: U) -> U => result
            }
            public extension Extras for Derived {
                func Extra() -> int => 4
                val ExtraValue: int => 7
            }
            """;
        var (compilation, _) = CreateCompilation(source, assemblyName: "RelationshipFixture");
        compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ShouldBeEmpty();
        IAssemblySymbol? assembly = null;
        if (metadata)
        {
            var reference = TestMetadataFactory.CreateFileReferenceFromSource(source, "RelationshipFixture");
            compilation = Compilation.Create("RelationshipHost", options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddReferences(TestMetadataReferences.Default).AddReferences(reference);
            _ = compilation.GetDiagnostics();
            assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
        }
        var root = Path.Combine(Path.GetTempPath(), "ravendoc-tests", Guid.NewGuid().ToString("N"));
        var content = Path.Combine(root, "content");
        var output = Path.Combine(root, "output");
        Directory.CreateDirectory(content);
        File.WriteAllText(Path.Combine(content, "type.md"), "---\nuid: T:Relationships.Derived\n---\n## Usage\n\nType usage example.");
        File.WriteAllText(Path.Combine(content, "member.md"), "---\nuid: M:Relationships.Derived.Run\n---\n## Example\n\nMember usage example.");
        try
        {
            var options = new DocumentationSiteOptions([], ExtensionNamespaces: ["Relationships"], ApiContent: content);
            if (assembly is null) DocumentationGenerator.ProcessCompilation(compilation, output, options);
            else DocumentationGenerator.ProcessAssembly(compilation, assembly, output, options);
            var factory = File.ReadAllText(Path.Combine(output, "Relationships/Factory/index.html"));
            factory.ShouldContain("Create()");
            factory.ShouldNotContain("data-member-inherited=\"true\"");
            factory.ShouldNotContain("GetHashCode()");
            factory.ShouldNotContain("ToString()");
            var page = File.ReadAllText(Path.Combine(output, "Relationships/Derived/index.html"));
            page.ShouldContain("Inherited from");
            page.ShouldContain("../Base/method_Read.html");
            page.ShouldNotContain("Hidden()");
            page.ShouldNotContain("CreateCount()");
            page.ShouldContain("Defined by");
            page.ShouldContain("../IContract/method_Run.html");
            page.ShouldContain("member-origin\">Implements <a href=\"../IContract/method_Run.html\">IContract.Run</a>");
            File.ReadAllText(Path.Combine(output, "Relationships/IContract/method_Run.html"))
                .ShouldContain("Required implementation (no default).");
            File.ReadAllText(Path.Combine(output, "Relationships/IContract/method_DefaultRun.html"))
                .ShouldContain("Default implementation provided.");
            page.ShouldContain("Part of closed hierarchy");
            page.ShouldNotContain("Extension methods");
            page.ShouldContain("symbol-extension-marker\">E</span>");
            page.ShouldContain("id=\"show-inherited-members\"");
            page.ShouldContain("id=\"show-extension-members\"");
            page.ShouldContain("data-member-extension=\"true\"");
            page.ShouldContain("Extras.Extra");
            page.ShouldNotContain("Extension properties");
            page.ShouldContain("Extras.ExtraValue");
            File.ReadAllText(Path.Combine(output, "Relationships/Generic`1/index.html"))
                .ShouldContain("IGeneric&lt;T&gt;.Echo</a>");
            var genericPage = File.ReadAllText(Path.Combine(output, "Relationships/Generic`1/index.html"));
            genericPage.ShouldContain(metadata ? "GenericExtras.Identity" : "GenericExtras&lt;T&gt;.Identity");
            genericPage.ShouldContain("data-member-extension=\"true\"");
            genericPage.ShouldContain("Identity(value: T)");
            genericPage.ShouldContain("Convert&lt;U&gt;(value: T, result: U)");
            genericPage.ShouldNotContain("Identity&lt;T&gt;");
            genericPage.ShouldNotContain("Convert&lt;T, U&gt;");
            var concretePage = File.ReadAllText(Path.Combine(output, "Relationships/Concrete/index.html"));
            concretePage.ShouldContain("Identity(value: int)");
            concretePage.ShouldContain("Convert&lt;U&gt;(value: int, result: U)");
            concretePage.ShouldNotContain("Identity&lt;T&gt;");
            var containerPage = File.ReadAllText(Path.Combine(output, metadata ? "Relationships/GenericExtras/index.html" : "Relationships/GenericExtras`1/index.html"));
            containerPage.ShouldContain("Identity");
            containerPage.ShouldContain("symbol-extension-marker\">E</span>");
            containerPage.ShouldNotContain("data-member-extension=\"true\"");
            page.ShouldContain("Extra()");
            page.ShouldContain("Type usage example.");
            page.ShouldContain("Uses default implementation from");
            var inherited = File.ReadAllText(Path.Combine(output, "Relationships/InheritingProvider/index.html"));
            inherited.ShouldContain("Inherited from");
            inherited.ShouldContain("../InheritedProvider/method_Run.html");
            var overridden = File.ReadAllText(Path.Combine(output, "Relationships/OverridingProvider/index.html"));
            overridden.ShouldContain("Overrides <a");
            overridden.ShouldContain("Overrides implementation of");
            var member = File.ReadAllText(Path.Combine(output, "Relationships/Derived/method_Run.html"));
            member.ShouldContain("Return value");
            member.ShouldContain("Member usage example.");
            var parent = File.ReadAllText(Path.Combine(output, "Relationships/Base/index.html"));
            parent.ShouldContain("id=\"derived-types\"");
            var contractPage = File.ReadAllText(Path.Combine(output, "Relationships/IContract/index.html"));
            contractPage.ShouldContain("id=\"derived-interfaces\"");
            contractPage.ShouldContain("id=\"implementing-types\"");
            contractPage.ShouldContain("../IChild/index.html");
            File.ReadAllText(Path.Combine(output, "Relationships/IChild/index.html"))
                .ShouldContain("<strong>Inherits</strong>");
            contractPage.ShouldContain("../ChildImplementation/index.html");
            contractPage.ShouldContain("InheritingProvider</a> (indirect)");
            contractPage.ShouldNotContain("HiddenImplementation");
            File.ReadAllText(Path.Combine(output, "Relationships/IGeneric`1/index.html"))
                .ShouldContain("../Generic`1/index.html");
            parent.ShouldContain("Permitted direct subtypes");
            parent.ShouldContain("CreateCount()");
            parent.ShouldContain("../Derived/index.html");

            var selected = new DocumentationSiteOptions([], Types: ["Relationships.IContract", "Relationships.Derived"]);
            if (assembly is null) DocumentationGenerator.ProcessCompilation(compilation, output, selected);
            else DocumentationGenerator.ProcessAssembly(compilation, assembly, output, selected);
            var selectedContract = File.ReadAllText(Path.Combine(output, "Relationships/IContract/index.html"));
            selectedContract.ShouldContain("../Derived/index.html");
            selectedContract.ShouldNotContain("ChildImplementation");
            selectedContract.ShouldNotContain("id=\"derived-interfaces\"");
        }
        finally { if (Directory.Exists(root)) Directory.Delete(root, true); }
    }

    [Fact]
    public void DelegateNavigationUsesNominalNames()
    {
        var (compilation, _) = CreateCompilation("public class Host { }", assemblyName: "DelegateHost");
        var assembly = compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly!;
        var output = Path.Combine(Path.GetTempPath(), "ravendoc-tests", Guid.NewGuid().ToString("N"));
        try
        {
            DocumentationGenerator.ProcessAssembly(compilation, assembly, output,
                new DocumentationSiteOptions([], Types: ["System.Func`2"]));
            var page = File.ReadAllText(Path.Combine(output, "System/Func`2/index.html"));
            page.ShouldContain("<h1>Func&lt;T, TResult&gt;</h1>");
            page.ShouldContain("title=\"Func&lt;T, TResult&gt;\"");
            page.ShouldContain("delegate");
            page.ShouldNotContain("Func&lt;&gt;");
        }
        finally { if (Directory.Exists(output)) Directory.Delete(output, true); }
    }

    [Theory]
    [InlineData("hierarchical", false)]
    [InlineData("flat", false)]
    [InlineData("hierarchical", true)]
    [InlineData("flat", true)]
    public void NamespaceNavigationPreservesTypesAndUrls(string style, bool showEmptyNamespaces)
    {
        var (compilation, _) = CreateCompilation("""
            namespace Example { public class Root { } }
            namespace Example.Web { public class Request { public class Header { } } }
            namespace Example.Networking { public class Socket { } }
            namespace Example.Runtime.CompilerServices { public class Marker { } }
            """, assemblyName: "Navigation.Sample");
        var output = Path.Combine(Path.GetTempPath(), "ravendoc-tests", Guid.NewGuid().ToString("N"));
        try
        {
            DocumentationGenerator.ProcessCompilation(compilation, output,
                new DocumentationSiteOptions([], NamespaceNavigation: style, ShowEmptyNamespaces: showEmptyNamespaces));
            var page = File.ReadAllText(Path.Combine(output, "Example/Web/Request/index.html"));
            var treeHtml = System.Text.RegularExpressions.Regex.Match(page,
                "<nav class=\"api-navigation-panel\"[^>]*>(<ul>.*?</ul>)<p", System.Text.RegularExpressions.RegexOptions.Singleline).Groups[1].Value;
            var tree = System.Xml.Linq.XElement.Parse(treeHtml.Replace(" open>", " open=\"open\">"));
            var labels = tree.Elements("li").Select(li => li.Element("details")?.Element("summary")?.Value).ToArray();
            labels.ShouldBe(style == "flat"
                ? showEmptyNamespaces
                    ? new[] { "Example", "Example.Networking", "Example.Runtime", "Example.Runtime.CompilerServices", "Example.Web" }
                    : new[] { "Example", "Example.Networking", "Example.Runtime.CompilerServices", "Example.Web" }
                : new[] { "Example" });
            var runtime = tree.Descendants("details").SingleOrDefault(node => node.Element("summary")?.Value == "Example.Runtime");
            if (showEmptyNamespaces)
                runtime!.Descendants("a").ShouldContain(link => link.Value == "Namespace overview");
            else
                runtime.ShouldBeNull();
            tree.Descendants("details").ShouldContain(node => node.Element("summary")!.Value == "Example.Runtime.CompilerServices");
            File.Exists(Path.Combine(output, "Example/Runtime/index.html")).ShouldBeTrue();
            var web = tree.Descendants("details").Single(node => node.Element("summary")?.Value == "Example.Web");
            web.Attribute("open").ShouldNotBeNull();
            web.Descendants("a").ShouldContain(link => (string?)link.Attribute("href") == "../index.html");
            var request = web.Descendants("details").Single(node => node.Element("summary")?.Attribute("title")?.Value == "Request");
            request.Descendants("a").ShouldContain(link => (string?)link.Attribute("href") == "Header/index.html");
            page.ShouldContain("aria-current=\"location\"");
        }
        finally { if (Directory.Exists(output)) Directory.Delete(output, true); }
    }

    [Fact]
    public void BuiltInTypePagesAndNavigationUseDeclaredNames()
    {
        var (compilation, _) = CreateCompilation("public class Host { }", assemblyName: "NameHost");
        var assembly = compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly!;
        var output = Path.Combine(Path.GetTempPath(), "ravendoc-tests", Guid.NewGuid().ToString("N"));
        try
        {
            DocumentationGenerator.ProcessAssembly(compilation, assembly, output,
                new DocumentationSiteOptions([], Types: ["System.Object", "System.String", "System.Char"]));
            foreach (var name in new[] { "Object", "String", "Char" })
            {
                var page = File.ReadAllText(Path.Combine(output, "System", name, "index.html"));
                page.ShouldContain($"<h1>{name}</h1>");
                page.ShouldContain($"title=\"{name}\"");
            }
        }
        finally { if (Directory.Exists(output)) Directory.Delete(output, true); }
    }

    [Fact]
    public void CompactListsRetainOverloadTypesAndStaticIcons()
    {
        var (compilation, _) = CreateCompilation("""
            namespace Browser.Sample
            /// Readable contract.
            public interface IReadable { val Size: int { get; } }
            public enum Mode { On, Off }
            public struct Point { }
            public delegate Callback(value: int) -> ()
            /// Widget.
            public class Widget {
                /// A label.
                public val Name: string => "Sample"
                /// Accepts a number.
                public static func Run(value: int) -> () { }
                /// Accepts text.
                public static func Run(value: string) -> () { }
            }
            """, assemblyName: "Browser.Sample");
        var output = Path.Combine(Path.GetTempPath(), "ravendoc-tests", Guid.NewGuid().ToString("N"));
        try
        {
            DocumentationGenerator.ProcessCompilation(compilation, output);
            var page = File.ReadAllText(Path.Combine(output, "Browser/Sample/Widget/index.html"));
            page.ShouldNotContain("googletagmanager.com");
            page.ShouldContain("member-name\">Name: string</span>");
            page.ShouldContain("member-name\">Run(value: int) -&gt; ()</span>");
            page.ShouldContain("member-name\">Run(value: string) -&gt; ()</span>");
            foreach (var (name, kind) in new[] { ("Mode", "enum"), ("Point", "struct"), ("Callback", "delegate") })
                File.ReadAllText(Path.Combine(output, $"Browser/Sample/{name}/index.html")).ShouldContain($"symbol-icon--{kind}");
            page.ShouldContain("symbol-static-marker");
            page.ShouldContain("Static member");
            page.ShouldNotContain("member-name\">static");
            File.ReadAllText(Path.Combine(output, "Browser/Sample/index.html")).ShouldContain("symbol-icon--interface");
            var detail = File.ReadAllText(Path.Combine(output, "Browser/Sample/Widget/method_Run.html"));
            detail.ShouldContain("static func Run");
            DocumentationGenerator.ProcessCompilation(compilation, output, new DocumentationSiteOptions([], MemberListStyle: "signatures", Types: ["Browser.Sample.Widget"], ShowToc: false));
            var full = File.ReadAllText(Path.Combine(output, "Browser/Sample/Widget/index.html"));
            full.ShouldContain("member-signature\">static func Run");
            full.ShouldNotContain("aria-label=\"On this page\"");
            File.Exists(Path.Combine(output, "Browser/Sample/IReadable/index.html")).ShouldBeFalse();
        }
        finally { if (Directory.Exists(output)) Directory.Delete(output, true); }
    }

    [Fact]
    public void SourceCompilation_GeneratesRavenApiSite()
    {
        var (compilation, _) = CreateCompilation("""
            namespace Samples.Docs

            /// A documented Raven type from version {{ productVersion }}.
            /// Published at [the API root]({{apiRoot}}).
            /// The unresolved {{futureValue}} remains visible.
            ///
            /// ## Usage
            ///
            /// Call [GetTitle](xref:M:Samples.Docs.Widget.GetTitle).
            ///
            /// ```raven
            /// let widget = Widget()
            /// ```
            public class Widget {
                /// Returns the current title.
                public func GetTitle() -> string => "Raven"

                /// Returns the supplied reference.
                /// @typeparam T The reference type to preserve.
                public func Echo<T>(value: T) -> T where T: class => value

                val PublicInit: string { init; }
                val PrivateSet: string { private set; }
                val ProtectedSet: string { protected set; }
            }
            """, assemblyName: "RavenDoc.Sample");
        var outputPath = Path.Combine(
            Path.GetTempPath(),
            "ravendoc-tests",
            Guid.NewGuid().ToString("N"));
        var siteRootPath = Path.GetDirectoryName(outputPath)!;

        try
        {
            DocumentationGenerator.ProcessCompilation(
                compilation,
                outputPath,
                new DocumentationSiteOptions(
                    [
                        new DocumentationSiteLink(
                            "Raven documentation",
                            "https://example.com/raven/")
                    ],
                    new Dictionary<string, string>
                    {
                        ["productVersion"] = "1.2.3+build.7",
                        ["apiRoot"] = "../reference/"
                    },
                    siteRootPath, MemberListStyle: "signatures"));

            var typePagePath = Path.Combine(outputPath, "Samples", "Docs", "Widget", "index.html");
            var memberPagePath = Path.Combine(outputPath, "Samples", "Docs", "Widget", "method_GetTitle.html");
            var genericMemberPagePath = Path.Combine(outputPath, "Samples", "Docs", "Widget", "method_Echo.html");

            File.Exists(typePagePath).ShouldBeTrue();
            File.Exists(memberPagePath).ShouldBeTrue();
            File.Exists(genericMemberPagePath).ShouldBeTrue();

            var typePage = File.ReadAllText(typePagePath);
            typePage.ShouldContain("Raven API reference");
            typePage.ShouldContain("href=\"../../../../index.html\"");
            typePage.ShouldContain("api-hero");
            typePage.ShouldContain("member-card");
            typePage.ShouldContain("symbol-icon--function");
            typePage.ShouldContain("id=\"methods\"");
            typePage.ShouldContain("A documented Raven type from version 1.2.3+build.7.");
            typePage.ShouldContain("href=\"../reference/\"");
            typePage.ShouldContain("The unresolved {{futureValue}} remains visible.");
            typePage.ShouldNotContain("{{ productVersion }}");
            typePage.ShouldContain("method_GetTitle.html");
            typePage.ShouldContain("language-raven");
            typePage.ShouldContain("class Widget");
            typePage.ShouldNotContain("public class Widget");
            typePage.ShouldContain("val PublicInit: string { init; }");
            typePage.ShouldContain("val PrivateSet: string");
            typePage.ShouldNotContain("private set;");
            typePage.ShouldContain("val ProtectedSet: string { protected set; }");
            typePage.ShouldContain("site.js");
            typePage.ShouldContain("raven-theme.css");
            typePage.ShouldContain("Generated by RavenDoc");
            typePage.ShouldContain("Raven documentation");
            typePage.ShouldContain("https://example.com/raven/");
            typePage.ShouldNotContain("System/Object/index.html");
            typePage.Split(">Usage</h2>", StringSplitOptions.None).Length.ShouldBe(2);

            var memberPage = File.ReadAllText(memberPagePath);
            memberPage.ShouldContain("func GetTitle() -&gt; string");
            memberPage.ShouldContain("Returns the current title.");
            var genericMemberPage = File.ReadAllText(genericMemberPagePath);
            genericMemberPage.ShouldContain("func Echo&lt;T&gt;(value: T) -&gt; T where T: class");
            genericMemberPage.ShouldContain("Returns the supplied reference.");
            genericMemberPage.ShouldContain("Type parameters");
            genericMemberPage.ShouldContain("The reference type to preserve.");
            genericMemberPage.ShouldNotContain("@typeparam");
            File.ReadAllText(Path.Combine(outputPath, "site.js"))
                .ShouldContain("hljs.highlightElement(code)");
            File.ReadAllText(Path.Combine(outputPath, "raven-language.js")).ShouldContain("title.function.invoke");
            File.ReadAllText(Path.Combine(outputPath, "raven-highlight.css")).ShouldContain(".hljs-type");
            File.ReadAllText(Path.Combine(outputPath, "highlight-core.js")).ShouldContain("11.11.1");
            File.ReadAllText(Path.Combine(outputPath, "highlight-LICENSE")).ShouldContain("BSD");
            typePage.ShouldContain("<script type=\"module\"");
            var sharedTheme = File.ReadAllText(Path.Combine(outputPath, "raven-theme.css"));
            sharedTheme.ShouldContain("--raven-accent");
            sharedTheme.ShouldContain("--raven-syntax-keyword: #569cd6");
            sharedTheme.ShouldContain("--raven-code-bg: #1e1e1e");
            File.ReadAllText(Path.Combine(outputPath, "style.css"))
                .ShouldContain("color: var(--raven-syntax-keyword)");
        }
        finally
        {
            if (Directory.Exists(outputPath))
                Directory.Delete(outputPath, recursive: true);
        }
    }

    [Fact]
    public void Macro_GeneratesNamespaceMemberPage()
    {
        var (compilation, _) = CreateCompilation("""
            namespace Samples.Macros

            /// Quotes a Raven expression.
            ///
            /// See the [syntax-tree API](https://example.com/syntax-tree).
            public macro Quote() { }

            /// Returns a greeting from the namespace.
            public func Greet() -> string => "Hello"
            """, assemblyName: "RavenDoc.Macros");
        var outputPath = Path.Combine(
            Path.GetTempPath(),
            "ravendoc-tests",
            Guid.NewGuid().ToString("N"));

        try
        {
            DocumentationGenerator.ProcessCompilation(compilation, outputPath);

            var namespacePagePath = Path.Combine(
                outputPath,
                "Samples",
                "Macros",
                "index.html");
            var macroPagePath = Path.Combine(
                outputPath,
                "Samples",
                "Macros",
                "macro_Quote.html");
            var functionPagePath = Path.Combine(
                outputPath,
                "Samples",
                "Macros",
                "method_Greet.html");

            File.Exists(namespacePagePath).ShouldBeTrue();
            File.Exists(macroPagePath).ShouldBeTrue();
            File.Exists(functionPagePath).ShouldBeTrue();

            var namespacePage = File.ReadAllText(namespacePagePath);
            namespacePage.ShouldContain("Macros");
            namespacePage.ShouldContain("macro_Quote.html");
            namespacePage.ShouldContain("Functions");
            namespacePage.ShouldContain("method_Greet.html");

            var macroPage = File.ReadAllText(macroPagePath);
            macroPage.ShouldContain("Quotes a Raven expression.");
            macroPage.ShouldContain("https://example.com/syntax-tree");
            macroPage.ShouldContain("RavenDoc.Macros.dll");
            macroPage.ShouldContain("Samples.Macros");

            var functionPage = File.ReadAllText(functionPagePath);
            functionPage.ShouldContain("Returns a greeting from the namespace.");
            functionPage.ShouldContain("CLR container (for .NET interop)");
        }
        finally
        {
            if (Directory.Exists(outputPath))
                Directory.Delete(outputPath, recursive: true);
        }
    }

    [Fact]
    public void MetadataExtension_HidesCompilerGeneratedGroupingTypes()
    {
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
            namespace Samples.Extensions

            public extension TextExtensions for string {
                func WordCount() -> int { 0 }
            }
            """,
            assemblyName: $"RavenDoc.Extensions.{Guid.NewGuid():N}");
        var hostCompilation = Compilation.Create(
                "RavenDoc.MetadataHost",
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(metadataReference);
        _ = hostCompilation.GetDiagnostics();
        var assembly = Assert.IsAssignableFrom<IAssemblySymbol>(
            hostCompilation.GetAssemblyOrModuleSymbol(metadataReference));
        var outputPath = Path.Combine(
            Path.GetTempPath(),
            "ravendoc-tests",
            Guid.NewGuid().ToString("N"));

        try
        {
            DocumentationGenerator.ProcessAssembly(hostCompilation, assembly, outputPath);

            var extensionPagePath = Path.Combine(
                outputPath,
                "Samples",
                "Extensions",
                "TextExtensions",
                "index.html");
            File.Exists(extensionPagePath).ShouldBeTrue();

            var extensionPage = File.ReadAllText(extensionPagePath);
            extensionPage.ShouldNotContain("<>__RavenExtensionGrouping");
            extensionPage.ShouldNotContain("<>__RavenExtensionMarker");

            Directory.EnumerateFileSystemEntries(outputPath, "*", SearchOption.AllDirectories)
                .ShouldNotContain(path =>
                    path.Contains("<>__RavenExtensionGrouping", StringComparison.Ordinal) ||
                    path.Contains("<>__RavenExtensionMarker", StringComparison.Ordinal));
        }
        finally
        {
            if (Directory.Exists(outputPath))
                Directory.Delete(outputPath, recursive: true);
        }
    }

    [Fact]
    public void ReferencedLibrary_DoesNotBecomeLocalNavigationOrLocalTypeLinks()
    {
        var dependency = TestMetadataFactory.CreateFileReferenceFromSource(
            """
            namespace External.Library
            public class Base { }
            """, assemblyName: $"RavenDoc.Dependency.{Guid.NewGuid():N}");
        var (compilation, _) = CreateCompilation("""
            namespace Samples.Owned
            /// Uses [the external base](xref:T:External.Library.Base).
            public class Widget : External.Library.Base { }
            """, references: TestMetadataReferences.Default.Concat([dependency]).ToArray());
        var output = Path.Combine(Path.GetTempPath(), "ravendoc-tests", Guid.NewGuid().ToString("N"));
        try
        {
            DocumentationGenerator.ProcessCompilation(compilation, output);
            var page = File.ReadAllText(Path.Combine(output, "Samples/Owned/Widget/index.html"));
            page.ShouldContain("Base");
            page.ShouldNotContain("External/Library/Base/index.html");
            page.ShouldNotContain(">External.Library</a>");
            page.ShouldNotContain("System/index.html");
            Directory.Exists(Path.Combine(output, "External")).ShouldBeFalse();
        }
        finally
        {
            if (Directory.Exists(output))
                Directory.Delete(output, recursive: true);
        }
    }

    [Fact]
    public void CaseDeclaredUnion_GroupsProjectedCasesUnderUnion()
    {
        var (compilation, _) = CreateCompilation("""
            namespace Samples.Unions

            /// A result with named cases.
            public union Outcome<T, E> {
                /// Contains a successful value.
                case Success(value: T)

                /// Contains an error value.
                case Failure(error: E)
            }

            /// A union of member types without named cases.
            public union Scalar(int | string)
            """, assemblyName: "RavenDoc.Unions");
        var outputPath = Path.Combine(
            Path.GetTempPath(),
            "ravendoc-tests",
            Guid.NewGuid().ToString("N"));

        try
        {
            DocumentationGenerator.ProcessCompilation(compilation, outputPath);

            var outcomePagePath = Path.Combine(
                outputPath,
                "Samples",
                "Unions",
                "Outcome`2",
                "index.html");
            var scalarPagePath = Path.Combine(
                outputPath,
                "Samples",
                "Unions",
                "Scalar",
                "index.html");

            var outcomePage = File.ReadAllText(outcomePagePath);
            outcomePage.ShouldContain("symbol-icon--union");
            outcomePage.ShouldContain("reference-navigation");
            outcomePage.ShouldContain("Success/index.html");
            outcomePage.ShouldContain("Failure/index.html");
            var casePage = File.ReadAllText(Path.Combine(Path.GetDirectoryName(
                Directory.GetFiles(outputPath, "index.html", SearchOption.AllDirectories)
                    .Single(path => path.EndsWith("Outcome`2/index.html")))!, "Success/index.html"));
            casePage.ShouldContain("case Success(value: T)");
            casePage.ShouldContain("value");
            outcomePage.ShouldContain("<h1>Outcome&lt;T, E&gt;</h1>");
            outcomePage.ShouldNotContain("<summary title=\"Outcome&lt;T, E&gt;\"");
            outcomePage.ShouldContain("id=\"cases\"");
            outcomePage.ShouldContain("case Success(value: T)");
            outcomePage.ShouldContain("Contains a successful value.");
            outcomePage.ShouldContain("case Failure(error: E)");
            outcomePage.ShouldContain("Contains an error value.");
            outcomePage.ShouldNotContain("Outcome_Success");
            outcomePage.ShouldNotContain("Outcome_Failure");

            DocumentationGenerator.ExportXrefs(outputPath)["T:Samples.Unions.Outcome"].ShouldBe("Samples/Unions/Outcome`2/index.html");
            var scalarPage = File.ReadAllText(scalarPagePath);
            scalarPage.ShouldNotContain("id=\"cases\"");
            scalarPage.ShouldNotContain(">Cases<");

            Directory.EnumerateFileSystemEntries(outputPath, "*", SearchOption.AllDirectories)
                .ShouldNotContain(path =>
                    path.Contains("Outcome_Success", StringComparison.Ordinal) ||
                    path.Contains("Outcome_Failure", StringComparison.Ordinal));
        }
        finally
        {
            if (Directory.Exists(outputPath))
                Directory.Delete(outputPath, recursive: true);
        }
    }

    [Fact]
    public void MetadataUnion_ProjectsLogicalCaseNamesOnUnionPage()
    {
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
            namespace Samples.MetadataUnions

            public union Outcome<T, E> {
                case Success(value: T)
                case Failure(error: E)
            }
            """,
            assemblyName: $"RavenDoc.MetadataUnions.{Guid.NewGuid():N}");
        var hostCompilation = Compilation.Create(
                "RavenDoc.MetadataHost",
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(metadataReference);
        _ = hostCompilation.GetDiagnostics();
        var assembly = Assert.IsAssignableFrom<IAssemblySymbol>(
            hostCompilation.GetAssemblyOrModuleSymbol(metadataReference));
        var outputPath = Path.Combine(
            Path.GetTempPath(),
            "ravendoc-tests",
            Guid.NewGuid().ToString("N"));

        try
        {
            DocumentationGenerator.ProcessAssembly(hostCompilation, assembly, outputPath);

            var outcomePage = File.ReadAllText(Path.Combine(
                outputPath,
                "Samples",
                "MetadataUnions",
                "Outcome`2",
                "index.html"));
            outcomePage.ShouldContain("symbol-icon--union");
            outcomePage.ShouldContain("reference-navigation");
            outcomePage.ShouldContain("Success/index.html");
            outcomePage.ShouldContain("Failure/index.html");
            var casePage = File.ReadAllText(Path.Combine(Path.GetDirectoryName(
                Directory.GetFiles(outputPath, "index.html", SearchOption.AllDirectories)
                    .Single(path => path.EndsWith("Outcome`2/index.html")))!, "Success/index.html"));
            casePage.ShouldContain("case Success(value: T)");
            casePage.ShouldContain("value");
            outcomePage.ShouldContain("<h1>Outcome&lt;T, E&gt;</h1>");
            outcomePage.ShouldNotContain("<summary title=\"Outcome&lt;T, E&gt;\"");
            outcomePage.ShouldContain("id=\"cases\"");
            outcomePage.ShouldContain("case Success(value: T)");
            outcomePage.ShouldContain("case Failure(error: E)");
            outcomePage.ShouldNotContain("Outcome_Success");
            outcomePage.ShouldNotContain("Outcome_Failure");

            var namespacePage = File.ReadAllText(Path.Combine(
                outputPath,
                "Samples",
                "MetadataUnions",
                "index.html"));
            namespacePage.ShouldNotContain("Outcome_Success");
            namespacePage.ShouldNotContain("Outcome_Failure");

            Directory.EnumerateFileSystemEntries(outputPath, "*", SearchOption.AllDirectories)
                .ShouldNotContain(path =>
                    path.Contains("Outcome_Success", StringComparison.Ordinal) ||
                    path.Contains("Outcome_Failure", StringComparison.Ordinal));
        }
        finally
        {
            if (Directory.Exists(outputPath))
                Directory.Delete(outputPath, recursive: true);
        }
    }
}
