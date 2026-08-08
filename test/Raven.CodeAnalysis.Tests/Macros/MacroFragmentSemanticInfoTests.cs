using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroFragmentSemanticInfoTests
{
    [Fact]
    public void GetMacroFragmentSemanticInfo_ResolvesCallerLocalAndMember()
    {
        const string code = """
            import Raven.CodeAnalysis.Tests.Macros.*

            class Customer {
                val Name: string => "Ada"
            }

            func Main() {
                let customer = Customer()
                let value = fragmentHover! { customer.Name }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentHover",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new FragmentHoverMacro()));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var customerPosition = code.LastIndexOf("customer.Name", StringComparison.Ordinal) + 1;
        var namePosition = code.LastIndexOf("Name", StringComparison.Ordinal) + 1;

        var customerInfo = compilation.GetMacroFragmentSemanticInfo(expression, customerPosition);
        var nameInfo = compilation.GetMacroFragmentSemanticInfo(expression, namePosition);

        var customer = Assert.IsAssignableFrom<ILocalSymbol>(customerInfo?.SymbolInfo.Symbol);
        Assert.Equal("customer", customer.Name);
        Assert.Equal("Customer", customer.Type.Name);
        Assert.Equal("customer", code.Substring(customerInfo!.Span.Start, customerInfo.Span.Length));

        var name = Assert.IsAssignableFrom<IPropertySymbol>(nameInfo?.SymbolInfo.Symbol);
        Assert.Equal("Name", name.Name);
        Assert.Equal(SpecialType.System_String, name.Type.SpecialType);
        Assert.Equal("Name", code.Substring(nameInfo!.Span.Start, nameInfo.Span.Length));
    }

    [Fact]
    public void GetMacroFragmentSemanticInfo_ResolvesMacroIntroducedLocalAndMember()
    {
        const string code = """
            import Raven.CodeAnalysis.Tests.Macros.*

            class Customer {
                val Name: string => "Ada"
            }

            func Main() {
                let value = fragmentLocalHover! { item.Name }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentLocalHover",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new FragmentLocalHoverMacro()));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var itemPosition = code.LastIndexOf("item.Name", StringComparison.Ordinal) + 1;
        var namePosition = code.LastIndexOf("Name", StringComparison.Ordinal) + 1;

        var itemInfo = compilation.GetMacroFragmentSemanticInfo(expression, itemPosition);
        var nameInfo = compilation.GetMacroFragmentSemanticInfo(expression, namePosition);

        var item = Assert.IsAssignableFrom<ILocalSymbol>(itemInfo?.SymbolInfo.Symbol);
        Assert.Equal("item", item.Name);
        Assert.Equal("Customer", item.Type.Name);
        Assert.Equal("item", code.Substring(itemInfo!.Span.Start, itemInfo.Span.Length));

        var name = Assert.IsAssignableFrom<IPropertySymbol>(nameInfo?.SymbolInfo.Symbol);
        Assert.Equal("Name", name.Name);
        Assert.Equal(SpecialType.System_String, name.Type.SpecialType);
    }

    [Fact]
    public void GetMacroFragmentSemanticInfo_ResolvesCollectionComprehensionSymbols()
    {
        const string code = """
            import Raven.CodeAnalysis.Tests.Macros.*

            class Customer {
                val Name: string => "Ada"
            }

            func Main() {
                let customers = [Customer()]
                let values = fragmentHover! {
                    [for customer in customers if customer.Name.Length > 0 => customer.Name]
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentComprehensionHover",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new FragmentHoverMacro()));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var sourcePosition = code.LastIndexOf("customers if", StringComparison.Ordinal) + 1;
        var localPosition = code.LastIndexOf("customer.Name", StringComparison.Ordinal) + 1;
        var memberPosition = code.LastIndexOf("Name]", StringComparison.Ordinal) + 1;

        var sourceInfo = compilation.GetMacroFragmentSemanticInfo(expression, sourcePosition);
        var localInfo = compilation.GetMacroFragmentSemanticInfo(expression, localPosition);
        var memberInfo = compilation.GetMacroFragmentSemanticInfo(expression, memberPosition);

        var source = Assert.IsAssignableFrom<ILocalSymbol>(sourceInfo?.SymbolInfo.Symbol);
        Assert.Equal("customers", source.Name);
        Assert.Equal("ImmutableList<Customer>", source.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var local = Assert.IsAssignableFrom<ILocalSymbol>(localInfo?.SymbolInfo.Symbol);
        Assert.Contains("customer", local.Name, StringComparison.Ordinal);
        Assert.Equal("Customer", local.Type.Name);

        var member = Assert.IsAssignableFrom<IPropertySymbol>(memberInfo?.SymbolInfo.Symbol);
        Assert.Equal("Name", member.Name);
        Assert.Equal(SpecialType.System_String, member.Type.SpecialType);
    }

    private sealed class FragmentHoverMacro : ITokenTreeExpressionMacro, IMacroFragmentProvider
    {
        public string Name => "fragmentHover";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(0, context.BodySpan.Length))
            ];
    }

    private sealed class FragmentLocalHoverMacro : ITokenTreeExpressionMacro, IMacroFragmentProvider
    {
        public string Name => "fragmentLocalHover";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
        {
            var customerType = context.Compilation.GetTypeByMetadataName("Customer")!;
            var item = context.CreateFragmentLocal("item", customerType);
            return
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(0, context.BodySpan.Length),
                    [item])
            ];
        }
    }
}
