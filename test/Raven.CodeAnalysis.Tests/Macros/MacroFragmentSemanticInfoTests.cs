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
        Assert.Equal("customer", local.Name);
        Assert.Equal("Customer", local.Type.Name);

        var member = Assert.IsAssignableFrom<IPropertySymbol>(memberInfo?.SymbolInfo.Symbol);
        Assert.Equal("Name", member.Name);
        Assert.Equal(SpecialType.System_String, member.Type.SpecialType);
    }

    [Fact]
    public void GetMacroFragmentSemanticInfo_ResolvesNestedMacroFragmentSymbols()
    {
        const string code = """
            import Raven.CodeAnalysis.Tests.Macros.*

            class Customer {
                val Name: string => "Ada"
            }

            func Main() {
                let customer = Customer()
                let value = fragmentHover! {
                    fragmentHover! { customer.Name }
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "NestedMacroFragmentHover",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new FragmentHoverMacro()));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .First();
        var customerPosition = code.LastIndexOf("customer.Name", StringComparison.Ordinal) + 1;
        var namePosition = code.LastIndexOf("Name", StringComparison.Ordinal) + 1;

        var customerInfo = compilation.GetMacroFragmentSemanticInfo(expression, customerPosition);
        var nameInfo = compilation.GetMacroFragmentSemanticInfo(expression, namePosition);

        var customer = Assert.IsAssignableFrom<ILocalSymbol>(customerInfo?.SymbolInfo.Symbol);
        Assert.Equal("customer", customer.Name);
        Assert.Equal("Customer", customer.Type.Name);

        var name = Assert.IsAssignableFrom<IPropertySymbol>(nameInfo?.SymbolInfo.Symbol);
        Assert.Equal("Name", name.Name);
        Assert.Equal(SpecialType.System_String, name.Type.SpecialType);
    }

    [Fact]
    public void GetMacroFragmentSemanticInfo_UsesExpressionTargetTypeForLambdaParameters()
    {
        const string code = """
            import Raven.CodeAnalysis.Tests.Macros.*

            func Main() {
                let value = targetTypedFragment! { (value) => value.ToString() }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentTargetTypedHover",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TargetTypedFragmentMacro()));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var parameterPosition = code.LastIndexOf("(value)", StringComparison.Ordinal) + 2;
        var referencePosition = code.LastIndexOf("value.ToString", StringComparison.Ordinal) + 2;

        var parameterInfo = compilation.GetMacroFragmentSemanticInfo(expression, parameterPosition);
        var referenceInfo = compilation.GetMacroFragmentSemanticInfo(expression, referencePosition);

        var lambda = Assert.IsAssignableFrom<ILambdaSymbol>(parameterInfo?.SymbolInfo.Symbol);
        var parameter = Assert.Single(lambda.Parameters);
        Assert.Equal("value", parameter.Name);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);

        var reference = Assert.IsAssignableFrom<IParameterSymbol>(referenceInfo?.SymbolInfo.Symbol);
        Assert.Equal("value", reference.Name);
        Assert.Equal(SpecialType.System_Int32, reference.Type.SpecialType);
    }

    [Fact]
    public void VisibleValueLookup_DoesNotRequireDetachedFragmentSyntaxTree()
    {
        var syntaxTree = SyntaxTree.ParseText("func Main() { }");
        var compilation = Compilation.Create(
                "DetachedMacroFragment",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(syntaxTree);
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var fragment = Assert.IsAssignableFrom<FunctionExpressionSyntax>(
            SyntaxFactory.ParseExpression("(value: int) => value"));

        var declarations = semanticModel.GetVisibleValueDeclarationsForTesting(fragment);

        var declaration = Assert.Single(declarations);
        Assert.Equal("value", declaration.Name);
        Assert.Null(declaration.DeclarationNode.SyntaxTree);
    }

    private sealed class FragmentHoverMacro : IMacroDefinition, IMacroFragmentProvider
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

    private sealed class FragmentLocalHoverMacro : IMacroDefinition, IMacroFragmentProvider
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

    private sealed class TargetTypedFragmentMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Name => "targetTypedFragment";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
        {
            var actionDefinition = context.Compilation.GetTypeByMetadataName("System.Action`1")!;
            var intType = context.Compilation.GetSpecialType(SpecialType.System_Int32);
            var actionType = actionDefinition.Construct(intType);
            return
            [
                context.CreateExpressionFragmentRegion(
                    new TextSpan(0, context.BodySpan.Length),
                    actionType)
            ];
        }
    }
}
