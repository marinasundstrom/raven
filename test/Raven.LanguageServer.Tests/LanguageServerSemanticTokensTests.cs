using System.Collections.Immutable;
using System.IO;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public sealed class LanguageServerSemanticTokensTests
{
    [Fact]
    public async Task SemanticTokens_ClassifyCommonRavenSymbolsAsync()
    {
        const string code = """
import System.Console.*

class Customer(name: string, age: int? = null) {
    val Name: string => name
    val Age: int? => age

    static func Print(customer: Customer) {
        if customer is Customer {
            val same = customer as Customer
        }
        WriteLine($"{customer.Name} age: ${customer.Age ?? "n/a"}")
    }
}
""";

        var tempRoot = Path.Combine(Path.GetTempPath(), $"raven-semantic-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempRoot);

        try
        {
            var projectPath = Path.Combine(tempRoot, "App.rvnproj");
            File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

            var documentPath = Path.Combine(tempRoot, "src", "main.rvn");
            Directory.CreateDirectory(Path.GetDirectoryName(documentPath)!);
            File.WriteAllText(documentPath, code);

            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "temp",
                    Uri = DocumentUri.FromFileSystemPath(tempRoot)
                })
            });

            var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
            var uri = DocumentUri.FromFileSystemPath(documentPath);
            store.UpsertDocument(uri, code);

            var handler = new SemanticTokensHandler(store, NullLogger<SemanticTokensHandler>.Instance);
            var result = await handler.Handle(new SemanticTokensParams
            {
                TextDocument = new TextDocumentIdentifier(uri)
            }, CancellationToken.None);

            result.ShouldNotBeNull();

            var decoded = Decode(code, result.Data, SemanticTokensHandler.Legend);

            Find(decoded, 0, "import").Type.ShouldBe(SemanticTokenType.Keyword);

            var customerDeclaration = Find(decoded, 2, "Customer");
            customerDeclaration.Type.ShouldBe(SemanticTokenType.Class);
            customerDeclaration.Modifiers.ShouldContain(SemanticTokenModifier.Declaration);

            var printDeclaration = Find(decoded, 6, "Print");
            printDeclaration.Type.ShouldBe(SemanticTokenType.Method);
            printDeclaration.Modifiers.ShouldContain(SemanticTokenModifier.Declaration);
            printDeclaration.Modifiers.ShouldContain(SemanticTokenModifier.Static);

            var parameter = Find(decoded, 6, "customer");
            parameter.Type.ShouldBe(SemanticTokenType.Parameter);
            parameter.Modifiers.ShouldContain(SemanticTokenModifier.Declaration);

            var nameProperty = Find(decoded, 3, "Name");
            nameProperty.Type.ShouldBe(SemanticTokenType.Property);
            nameProperty.Modifiers.ShouldContain(SemanticTokenModifier.Declaration);

            Find(decoded, 7, "is").Type.ShouldBe(SemanticTokenType.Operator);
            Find(decoded, 8, "as").Type.ShouldBe(SemanticTokenType.Operator);

            Find(decoded, 10, "\"n/a\"").Type.ShouldBe(SemanticTokenType.String);

            decoded.Any(token => token.Type == SemanticTokenType.Operator && token.Line == 10).ShouldBeTrue();
        }
        finally
        {
            if (Directory.Exists(tempRoot))
                Directory.Delete(tempRoot, recursive: true);
        }
    }

    [Fact]
    public async Task SemanticTokens_MissingParameterTypeAnnotation_DoesNotThrowAsync()
    {
        const string code = """
func Main(value) -> unit {
    value
}
""";

        var tempRoot = Path.Combine(Path.GetTempPath(), $"raven-semantic-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempRoot);

        try
        {
            var projectPath = Path.Combine(tempRoot, "App.rvnproj");
            File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

            var documentPath = Path.Combine(tempRoot, "src", "main.rvn");
            Directory.CreateDirectory(Path.GetDirectoryName(documentPath)!);
            File.WriteAllText(documentPath, code);

            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "temp",
                    Uri = DocumentUri.FromFileSystemPath(tempRoot)
                })
            });

            var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
            var uri = DocumentUri.FromFileSystemPath(documentPath);
            store.UpsertDocument(uri, code);

            var handler = new SemanticTokensHandler(store, NullLogger<SemanticTokensHandler>.Instance);
            var result = await handler.Handle(new SemanticTokensParams
            {
                TextDocument = new TextDocumentIdentifier(uri)
            }, CancellationToken.None);

            result.ShouldNotBeNull();
        }
        finally
        {
            if (Directory.Exists(tempRoot))
                Directory.Delete(tempRoot, recursive: true);
        }
    }

    [Fact]
    public async Task SemanticTokens_RangeRequest_AfterDocumentUpdate_DoesNotReturnShiftedTokensAsync()
    {
        const string initialCode = """
func LoadInboundBatch() -> Result<InboundBatch, FulfillmentError> {
    val dtoResult = try JsonSerializer.Deserialize<InboundBatchDto>(rawJson)

    if dtoResult is Ok(val value) {
        if value is not null {
            return ValidateBatch(value)
        }
    }

    if dtoResult is Error(Exception ex) {
        return InvalidFeedResult("Feed JSON could not be parsed: ${ex.Message}")
    }

    return InvalidFeedResult("Feed JSON resolved to null")
}
""";

        const string updatedCode = """
func LoadInboundBatch() -> Result<InboundBatch, FulfillmentError> {
    val dtoResult = try JsonSerializer.Deserialize<InboundBatchDto>(rawJson)

    // inserted comment to shift later token positions
    if dtoResult is Ok(val value) {
        if value is not null {
            return ValidateBatch(value)
        }
    }

    if dtoResult is Error(Exception ex) {
        return InvalidFeedResult("Feed JSON could not be parsed: ${ex.Message}")
    }

    return InvalidFeedResult("Feed JSON resolved to null")
}
""";

        var tempRoot = Path.Combine(Path.GetTempPath(), $"raven-semantic-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempRoot);

        try
        {
            var projectPath = Path.Combine(tempRoot, "App.rvnproj");
            File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

            var documentPath = Path.Combine(tempRoot, "src", "main.rvn");
            Directory.CreateDirectory(Path.GetDirectoryName(documentPath)!);
            File.WriteAllText(documentPath, initialCode);

            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "temp",
                    Uri = DocumentUri.FromFileSystemPath(tempRoot)
                })
            });

            var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
            var uri = DocumentUri.FromFileSystemPath(documentPath);
            store.UpsertDocument(uri, initialCode);

            var handler = new SemanticTokensHandler(store, NullLogger<SemanticTokensHandler>.Instance);
            var initialResult = await handler.Handle(new SemanticTokensRangeParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(new Position(0, 0), new Position(13, 0))
            }, CancellationToken.None);

            initialResult.ShouldNotBeNull();

            store.UpsertDocument(uri, updatedCode);

            var updatedResult = await handler.Handle(new SemanticTokensRangeParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(new Position(0, 0), new Position(14, 0))
            }, CancellationToken.None);

            updatedResult.ShouldNotBeNull();

            var decoded = Decode(updatedCode, updatedResult.Data, SemanticTokensHandler.Legend);

            Find(decoded, 4, "if").Type.ShouldBe(SemanticTokenType.Keyword);
            Find(decoded, 4, "is").Type.ShouldBe(SemanticTokenType.Operator);
            Find(decoded, 5, "if").Type.ShouldBe(SemanticTokenType.Keyword);
            Find(decoded, 5, "is").Type.ShouldBe(SemanticTokenType.Operator);
            Find(decoded, 10, "if").Type.ShouldBe(SemanticTokenType.Keyword);
            decoded.Any(token => token.Text == "}" && token.Line is 7 or 8 or 13).ShouldBeFalse();
        }
        finally
        {
            if (Directory.Exists(tempRoot))
                Directory.Delete(tempRoot, recursive: true);
        }
    }

    private static DecodedToken Find(ImmutableArray<DecodedToken> tokens, int line, string text)
    {
        var token = tokens.FirstOrDefault(token => token.Line == line && token.Text == text);
        token.ShouldNotBeNull();
        return token!;
    }

    private static ImmutableArray<DecodedToken> Decode(
        string code,
        IEnumerable<int> data,
        SemanticTokensLegend legend)
    {
        var text = SourceText.From(code);
        var lines = code.Replace("\r\n", "\n").Split('\n');
        var tokenTypes = legend.TokenTypes.ToArray();
        var tokenModifiers = legend.TokenModifiers.ToArray();
        var values = data.ToArray();
        var result = ImmutableArray.CreateBuilder<DecodedToken>();

        var line = 0;
        var character = 0;
        for (var i = 0; i < values.Length; i += 5)
        {
            line += values[i];
            character = values[i] == 0 ? character + values[i + 1] : values[i + 1];

            var length = values[i + 2];
            var tokenType = tokenTypes[values[i + 3]];
            var modifierBits = values[i + 4];
            var modifiers = ImmutableArray.CreateBuilder<SemanticTokenModifier>();

            for (var bit = 0; bit < tokenModifiers.Length; bit++)
            {
                if ((modifierBits & (1 << bit)) != 0)
                    modifiers.Add(tokenModifiers[bit]);
            }

            var lineText = line < lines.Length ? lines[line] : string.Empty;
            string tokenText;
            if (character >= 0 && character + length <= lineText.Length)
            {
                tokenText = lineText.Substring(character, length);
            }
            else
            {
                var offset = GetOffset(text, line, character);
                if (offset < 0 || offset + length > text.Length)
                {
                    throw new InvalidOperationException(
                        $"Invalid semantic token position at entry {i / 5}: line={line}, character={character}, length={length}, lineLength={lineText.Length}, offset={offset}, textLength={text.Length}, raw=[{values[i]}, {values[i + 1]}, {values[i + 2]}, {values[i + 3]}, {values[i + 4]}].");
                }

                tokenText = text.ToString(new TextSpan(offset, length));
            }

            result.Add(new DecodedToken(line, character, length, tokenType, modifiers.ToImmutable(), tokenText));
        }

        return result.ToImmutable();
    }

    private static int GetOffset(SourceText sourceText, int line, int character)
    {
        var current = 0;
        for (var index = 0; index < line; index++)
            current += sourceText.GetLineLength(index) + 1;

        return current + character;
    }

    private sealed record DecodedToken(
        int Line,
        int Character,
        int Length,
        SemanticTokenType Type,
        ImmutableArray<SemanticTokenModifier> Modifiers,
        string Text);
}
