namespace Generator.Tests;

using System.Threading.Tasks;

using Microsoft.CodeAnalysis.CSharp.Testing;
using Microsoft.CodeAnalysis.Testing;

using Xunit;

using static Microsoft.CodeAnalysis.Testing.ReferenceAssemblies;

public class GeneratorTests
{
    [Fact]
    public async Task TestSourceGenerator()
    {
        var test = new CSharpSourceGeneratorTest<SyntaxNodePartialGenerator, DefaultVerifier>
        {
            TestState =
            {
                Sources = { """
namespace MyNamespace;

public abstract class SyntaxNode { }

public struct SyntaxToken { }

public partial class MySyntaxNode : SyntaxNode
{
    public partial MyNamespace.SyntaxNode ExampleProperty { get; }
}
""" },
                GeneratedSources =
                {
                    (typeof(SyntaxNodePartialGenerator), "MySyntaxNode_Generated.cs", """
using System;

namespace MyNamespace
{
    public partial class MySyntaxNode
    {
        public partial MyNamespace.SyntaxNode ExampleProperty
        {
            get
            {
                return null;
            }
        }
    }
}
""")
                }
            }
        };

        await test.RunAsync();
    }


    [Fact]
    public async Task TestSourceGenerator2()
    {
        var test = new CSharpSourceGeneratorTest<SyntaxNodePartialGenerator, DefaultVerifier>
        {
            TestState =
            {
                Sources = { """
namespace MyNamespace;

public abstract class SyntaxNode { }

public struct SyntaxToken { }

public partial class CustomSyntaxNode : MyBaseNode
{
    public partial SyntaxNode ChildNode { get; }
    public partial SyntaxToken IdentifierToken { get; }
}

public abstract class MyBaseNode : SyntaxNode
{
    // Base class logic
}
""" },
                GeneratedSources =
                {
                    (typeof(SyntaxNodePartialGenerator), "CustomSyntaxNode_Generated.cs", """
using System;

namespace MyNamespace
{
    public partial class CustomSyntaxNode
    {
        public partial MyNamespace.SyntaxNode ChildNode
        {
            get
            {
                return null;
            }
        }

        public partial MyNamespace.SyntaxToken IdentifierToken
        {
            get
            {
                return default;
            }
        }
    }
}
""")
                }
            }
        };

        //test.TestState.AdditionalReferences.Add(MetadataReference.CreateFromFile(typeof(ThrowsAttribute).Assembly.Location));
        test.TestState.ReferenceAssemblies = Net.Net90;

        await test.RunAsync();
    }
}