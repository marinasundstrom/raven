using System.Security.Cryptography;
using System.Text;
using System.Xml;
using System.Xml.Linq;

using NodesShared;

using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

using var streamReader = new StreamReader("../../Raven.CodeAnalysis/Syntax/Model.yaml");

var deserializer = new DeserializerBuilder()
    .WithNamingConvention(CamelCaseNamingConvention.Instance)
    .IgnoreUnmatchedProperties()
    .Build();

var model = deserializer.Deserialize<List<SyntaxNodeModel>>(streamReader);

using var xmlWriter = XmlWriter.Create("Model.xml", new XmlWriterSettings { Async = true, Indent = true });

await Serialization.SerializeAsXml(model, xmlWriter);