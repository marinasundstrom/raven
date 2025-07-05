using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;

using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

namespace Raven.Generators;

[Generator]
public class GreenNodeSourceGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var yamlFiles = context.AdditionalTextsProvider
            .Where(file => Path.GetFileName(file.Path) == "model.yaml");

        var yamlTexts = yamlFiles
            .Select((file, ct) => file.GetText(ct)?.ToString())
            .Where(text => !string.IsNullOrWhiteSpace(text));

        var parsed = yamlTexts.Select((yaml, _) =>
        {
            var deserializer = new DeserializerBuilder()
                .WithNamingConvention(CamelCaseNamingConvention.Instance)
                .IgnoreUnmatchedProperties()
                .Build();

            return deserializer.Deserialize<List<SyntaxNodeModel>>(yaml!);
        });

        context.RegisterSourceOutput(parsed, (ctx, nodes) =>
        {
            var nodesByName = nodes.ToDictionary(n => n.Name);
            foreach (var node in nodes)
            {
                var source = GreenNodeGenerator.GenerateGreenNode(node, nodesByName);
                ctx.AddSource($"{node.Name}Syntax.g.cs", SourceText.From(source, Encoding.UTF8));
            }
        });
    }
}
