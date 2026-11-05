using System.Reflection;
using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public sealed class PortedReadonlyRecordMetadataTests
{
    [Theory]
    [InlineData("Raven.CodeAnalysis.TypedConstant")]
    [InlineData("Raven.CodeAnalysis.Text.LinePositionSpan")]
    [InlineData("Raven.CodeAnalysis.Text.FileLinePositionSpan")]
    [InlineData("Raven.CodeAnalysis.Text.TextChange")]
    [InlineData("Raven.CodeAnalysis.Text.TextChangeRange")]
    [InlineData("Raven.CodeAnalysis.Syntax.SyntaxTree+IncrementalParseResult")]
    [InlineData("Raven.CodeAnalysis.Syntax.InternalSyntax.Parser.ParseResult")]
    [InlineData("Raven.CodeAnalysis.Syntax.InternalSyntax.Parser.Lexer+ConditionalFrame")]
    [InlineData("Raven.CodeAnalysis.Syntax.InternalSyntax.Parser.MacroInvocationSyntaxParser+InvocationParts")]
    [InlineData("Raven.CodeAnalysis.Symbols.PETypeIdentity")]
    [InlineData("Raven.CodeAnalysis.Symbols.PETypeParameterIdentity")]
    public void ValueRecordsPreserveReadonlyTypeAndStorageMetadata(string metadataName)
    {
        var type = typeof(TypedConstant).Assembly.GetType(metadataName, throwOnError: true)!;
        Assert.True(type.IsValueType);
        Assert.True(type.IsDefined(typeof(IsReadOnlyAttribute), inherit: false));
        foreach (var field in type.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
            Assert.True(field.IsInitOnly, $"{metadataName}.{field.Name} must be readonly");
    }
}
