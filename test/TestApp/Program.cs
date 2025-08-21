using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;

class Program
{
    static void Main()
    {
        var compilation = Compilation.Create("test", options: new CompilationOptions(OutputKind.ConsoleApplication));
        var version = TargetFrameworkResolver.GetLatestVersion();
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

        // force setup
        compilation.GetDiagnostics();

        var consoleType = compilation.GetTypeByMetadataName("System.Console");
        Console.WriteLine($"Console type found: {consoleType != null}");
        if (consoleType != null)
        {
            var writeLineMembers = consoleType.GetMembers("WriteLine");
            Console.WriteLine($"WriteLine members: {writeLineMembers.Length}");
            foreach (var m in writeLineMembers) Console.WriteLine(m.ToDisplayString());
        }

        var resolver = new System.Reflection.PathAssemblyResolver(references.Select(r => r.FilePath));
        using var mlc = new System.Reflection.MetadataLoadContext(resolver);
        var asm = mlc.LoadFromAssemblyPath(Path.Combine(refDir!, "System.Console.dll"));
        var consoleType2 = asm.GetType("System.Console", true);
        Console.WriteLine($"Reflection Console type null? {consoleType2 == null}");
        Console.WriteLine($"Declared methods via reflection: {consoleType2?.GetMethods().Length}");

        var stringType = compilation.GetTypeByMetadataName("System.String");
        Console.WriteLine($"String type found: {stringType != null}");
        if (stringType != null)
        {
            var lengthMembers = stringType.GetMembers("Length");
            Console.WriteLine($"String Length members: {lengthMembers.Length}");
        }
    }
}
