namespace Raven.CodeAnalysis.Syntax;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;

/// <summary>
/// RoslynQuoter-like quoter for Raven syntax trees.
/// </summary>
public static class RavenQuoter
{
    private static readonly Type SyntaxFactoryType = typeof(SyntaxFactory);

    private sealed class FactoryInfo
    {
        public FactoryInfo(MethodInfo method)
        {
            Method = method;
            Parameters = method.GetParameters();
        }

        public MethodInfo Method { get; }
        public IReadOnlyList<ParameterInfo> Parameters { get; }
    }

    private static readonly Dictionary<Type, FactoryInfo?> s_factoryCache = new();

    public static string QuoteText(string ravenSource, RavenQuoterOptions? options = null)
    {
        options ??= new RavenQuoterOptions();
        var tree = SyntaxTree.ParseText(ravenSource);
        return Quote(tree.GetRoot(), options);
    }

    public static string Quote(SyntaxNode root, RavenQuoterOptions? options = null)
    {
        options ??= new RavenQuoterOptions();
        var w = new CodeWriter();
        var writer = new QuoterWriter(w, options);

        if (options.GenerateUsingDirectives)
        {
            w.WriteLine("using Raven.CodeAnalysis.Syntax;");
            if (options.UseStaticSyntaxFactoryImport)
                w.WriteLine("using static Raven.CodeAnalysis.Syntax.SyntaxFactory;");
            w.WriteLine();
        }

        if (options.WrapInClass)
        {
            w.WriteLine($"public static class {options.GeneratedClassName}");
            w.WriteLine("{");
            w.Indent();
            w.WriteLine($"public static CompilationUnitSyntax {options.GeneratedMethodName}()");
            w.WriteLine("{");
            w.Indent();

            w.Write("return ");
            writer.WriteNode(root);
            w.WriteLine(".NormalizeWhitespace();");

            w.Unindent();
            w.WriteLine("}");
            w.Unindent();
            w.WriteLine("}");
        }
        else
        {
            writer.WriteNode(root);
            w.WriteLine(".NormalizeWhitespace();");
        }

        return w.ToString();
    }

    private static FactoryInfo? GetFactoryInfo(Type nodeType)
    {
        lock (s_factoryCache)
        {
            if (s_factoryCache.TryGetValue(nodeType, out var cached))
                return cached;
        }

        var simpleName = nodeType.Name.EndsWith("Syntax", StringComparison.Ordinal)
            ? nodeType.Name[..^"Syntax".Length]
            : nodeType.Name;

        var methods = SyntaxFactoryType
            .GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Where(m => m.Name == simpleName && nodeType.IsAssignableFrom(m.ReturnType))
            .ToArray();

        MethodInfo? selected = null;

        if (methods.Length == 1)
        {
            selected = methods[0];
        }
        else if (methods.Length > 1)
        {
            selected = methods.OrderByDescending(m => m.GetParameters().Length).FirstOrDefault();
        }

        var info = selected is null ? null : new FactoryInfo(selected);

        lock (s_factoryCache)
        {
            s_factoryCache[nodeType] = info;
        }

        return info;
    }

    private static PropertyInfo? FindPropertyForParameter(Type nodeType, ParameterInfo parameter)
    {
        var name = parameter.Name;
        if (string.IsNullOrEmpty(name))
            return null;

        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase;

        var prop = nodeType.GetProperty(name, flags);
        if (prop is not null)
            return prop;

        var pascalName = char.ToUpperInvariant(name[0]) + name[1..];
        prop = nodeType.GetProperty(pascalName, flags);

        return prop;
    }

    // -------------------------------------------------------
    // Writer + reflection-based node printer
    // -------------------------------------------------------

    private sealed class QuoterWriter : SyntaxVisitor
    {
        private readonly CodeWriter _w;
        private readonly RavenQuoterOptions _options;

        public QuoterWriter(CodeWriter writer, RavenQuoterOptions options)
        {
            _w = writer;
            _options = options;
        }

        public void WriteNode(SyntaxNode node)
        {
            WriteNodeInternal(node);
        }

        public override void DefaultVisit(SyntaxNode node)
        {
            WriteNodeInternal(node);
        }

        private void WriteFactoryMethodName(string methodName)
        {
            if (_options.UseStaticSyntaxFactoryImport)
                _w.Write(methodName);
            else
            {
                _w.Write("SyntaxFactory.");
                _w.Write(methodName);
            }
        }

        private void WriteNodeInternal(SyntaxNode node)
        {
            var type = node.GetType();
            var typeName = type.Name;
            if (typeName.EndsWith("Syntax", StringComparison.Ordinal))
                typeName = typeName.Substring(0, typeName.Length - "Syntax".Length);

            var usedProperties = new HashSet<PropertyInfo>();

            var factory = GetFactoryInfo(type);
            List<(ParameterInfo Parameter, object? Value)>? paramValues = null;

            if (factory is not null)
            {
                paramValues = new List<(ParameterInfo, object?)>();
                bool ok = true;

                foreach (var param in factory.Parameters)
                {
                    var prop = FindPropertyForParameter(type, param);
                    if (prop is null)
                    {
                        if (param.HasDefaultValue)
                            break;

                        ok = false;
                        break;
                    }

                    var value = prop.GetValue(node);
                    paramValues.Add((param, value));
                    usedProperties.Add(prop);
                }

                if (!ok)
                {
                    factory = null;
                    paramValues = null;
                    usedProperties.Clear();
                }
            }

            if (factory is not null && paramValues is not null && paramValues.Count > 0)
            {
                // Pretty-print:
                //
                // Foo(
                //     arg1,
                //     arg2,
                //     ...)
                WriteFactoryMethodName(factory.Method.Name);
                _w.Write("(");
                _w.WriteLine();
                _w.Indent();

                for (int i = 0; i < paramValues.Count; i++)
                {
                    var (p, value) = paramValues[i];
                    WriteValue(p.ParameterType, value);

                    if (i < paramValues.Count - 1)
                        _w.WriteLine(",");
                    else
                        _w.WriteLine();
                }

                _w.Unindent();
                _w.Write(")");
            }
            else
            {
                // Fallback: Foo()
                WriteFactoryMethodName(typeName);
                _w.Write("()");
            }

            // Apply remaining properties via With<Property>()
            var props = type
                .GetProperties(BindingFlags.Instance | BindingFlags.Public)
                .Where(p => p.CanRead)
                .Where(p => !usedProperties.Contains(p))
                .Select(p => new
                {
                    Property = p,
                    With = type.GetMethod("With" + p.Name, new[] { p.PropertyType })
                })
                .Where(x => x.With is not null)
                .ToList();

            if (props.Count == 0)
                return;

            foreach (var item in props)
            {
                var p = item.Property;
                var value = p.GetValue(node);

                if (IsDefaultValue(p.PropertyType, value))
                    continue;

                _w.WriteLine();
                _w.Indent();
                _w.Write(".With");
                _w.Write(p.Name);
                _w.Write("(");
                WriteValue(p.PropertyType, value);
                _w.Write(")");
                _w.Unindent();
            }
        }

        private void WriteValue(Type type, object? value)
        {
            if (value is null)
            {
                _w.Write("null");
                return;
            }

            if (typeof(SyntaxNode).IsAssignableFrom(type))
            {
                WriteNodeInternal((SyntaxNode)value);
                return;
            }

            if (type == typeof(SyntaxToken))
            {
                WriteToken((SyntaxToken)value);
                return;
            }

            if (type == typeof(SyntaxTokenList))
            {
                WriteTokenList((SyntaxTokenList)value);
                return;
            }

            if (type.IsGenericType &&
                type.GetGenericTypeDefinition() == typeof(SyntaxList<>))
            {
                var elemType = type.GetGenericArguments()[0];
                WriteSyntaxList(elemType, (IEnumerable)value);
                return;
            }

            if (type.IsGenericType &&
                type.Name.StartsWith("SeparatedSyntaxList", StringComparison.Ordinal))
            {
                var elemType = type.GetGenericArguments()[0];
                WriteSeparatedSyntaxList(elemType, (IEnumerable)value);
                return;
            }

            if (type.IsEnum)
            {
                _w.Write(type.Name);
                _w.Write(".");
                _w.Write(Enum.GetName(type, value) ?? value.ToString());
                return;
            }

            if (type == typeof(string))
            {
                _w.Write(Literal((string)value));
                return;
            }

            if (type.IsValueType)
            {
                if (type == typeof(bool))
                    _w.Write(((bool)value) ? "true" : "false");
                else
                    _w.Write(Convert.ToString(value, System.Globalization.CultureInfo.InvariantCulture));
                return;
            }

            _w.Write(value.ToString() ?? "null");
        }

        private void WriteToken(SyntaxToken token)
        {
            if (!_options.IncludeTrivia || (!token.HasLeadingTrivia && !token.HasTrailingTrivia))
            {
                if (token.Kind == SyntaxKind.IdentifierToken)
                {
                    WriteFactoryMethodName("Identifier");
                    _w.Write("(");
                    _w.Write(Literal(token.Text));
                    _w.Write(")");
                }
                else if (token.Kind == SyntaxKind.NumericLiteralToken
                || token.Kind == SyntaxKind.StringLiteralToken)
                {
                    WriteFactoryMethodName("Literal");
                    _w.Write("(");
                    _w.Write(Literal(token.Text));
                    _w.Write(")");
                }
                else
                {
                    WriteFactoryMethodName("Token");
                    _w.Write("(SyntaxKind.");
                    _w.Write(token.Kind.ToString());
                    _w.Write(")");
                }

                return;
            }

            if (token.Kind == SyntaxKind.IdentifierToken)
            {
                WriteFactoryMethodName("Identifier");
                _w.Write("(");
                _w.Write(Literal(token.Text));
                _w.Write(")");
            }
            else
            {
                WriteFactoryMethodName("Token");
                _w.Write("(SyntaxKind.");
                _w.Write(token.Kind.ToString());
                _w.Write(")");
            }

            if (token.HasLeadingTrivia)
            {
                _w.Write(".WithLeadingTrivia(");
                WriteTriviaList(token.LeadingTrivia);
                _w.Write(")");
            }

            if (token.HasTrailingTrivia)
            {
                _w.Write(".WithTrailingTrivia(");
                WriteTriviaList(token.TrailingTrivia);
                _w.Write(")");
            }
        }

        private void WriteTokenList(SyntaxTokenList list)
        {
            if (list.Count == 0)
            {
                //_w.Write("SyntaxTokenList.Empty");

                WriteFactoryMethodName("TokenList()");
                return;
            }

            WriteFactoryMethodName("TokenList");
            _w.Write("(");
            if (list.Count == 1)
            {
                WriteToken(list[0]);
            }
            else
            {
                _w.WriteLine();
                _w.Indent();
                _w.Write("new[] {");
                _w.WriteLine();
                _w.Indent();
                for (int i = 0; i < list.Count; i++)
                {
                    WriteToken(list[i]);
                    if (i < list.Count - 1)
                        _w.WriteLine(",");
                    else
                        _w.WriteLine();
                }
                _w.Unindent();
                _w.Write("}");
                _w.WriteLine();
                _w.Unindent();
            }
            _w.Write(")");
        }

        private void WriteTriviaList(IEnumerable<SyntaxTrivia> trivia)
        {
            var list = trivia.ToList();
            if (list.Count == 0)
            {
                WriteFactoryMethodName("TriviaList");
                _w.Write("()");
                return;
            }

            if (list.Count == 1)
            {
                WriteFactoryMethodName("TriviaList");
                _w.Write("(");
                WriteSingleTrivia(list[0]);
                _w.Write(")");
                return;
            }

            WriteFactoryMethodName("TriviaList");
            _w.Write("(");
            _w.WriteLine();
            _w.Indent();
            _w.Write("new[] {");
            _w.WriteLine();
            _w.Indent();
            for (int i = 0; i < list.Count; i++)
            {
                WriteSingleTrivia(list[i]);
                if (i < list.Count - 1)
                    _w.WriteLine(",");
                else
                    _w.WriteLine();
            }
            _w.Unindent();
            _w.Write("}");
            _w.WriteLine();
            _w.Unindent();
            _w.Write(")");
        }

        private void WriteSingleTrivia(SyntaxTrivia trivia)
        {
            WriteFactoryMethodName("Trivia");
            _w.Write("(SyntaxKind.");
            _w.Write(trivia.Kind.ToString());
            _w.Write(", ");
            _w.Write(Literal(trivia.ToString()));
            _w.Write(")");
        }

        private void WriteSyntaxList(Type elemType, IEnumerable listEnumerable)
        {
            var items = listEnumerable.Cast<object?>().ToList();
            var typeName = elemType.Name;

            if (items.Count == 0)
            {
                //_w.Write($"SyntaxList<{typeName}>.Empty");

                WriteFactoryMethodName("List");
                _w.Write($"<{typeName}>()");

                return;
            }

            if (items.Count == 1)
            {
                WriteFactoryMethodName("SingletonList");
                _w.Write($"<{typeName}>(");
                WriteValue(elemType, items[0]!);
                _w.Write(")");
                return;
            }

            WriteFactoryMethodName("List");
            _w.Write($"<{typeName}>(new {typeName}[]");
            _w.WriteLine();
            _w.WriteLine("{");
            _w.Indent();

            for (int i = 0; i < items.Count; i++)
            {
                WriteValue(elemType, items[i]!);
                if (i < items.Count - 1)
                    _w.WriteLine(",");
                else
                    _w.WriteLine();
            }

            _w.Unindent();
            _w.Write("})");
        }

        private void WriteSeparatedSyntaxList(Type elemType, IEnumerable listEnumerable)
        {
            var items = listEnumerable.Cast<object?>().ToList();
            var typeName = elemType.Name;

            if (items.Count == 0)
            {
                //_w.Write($"SeparateSyntaxSyntax<{typeName}>.Empty");

                WriteFactoryMethodName("SeparatedList");
                _w.Write($"<{typeName}>()");

                return;
            }

            if (items.Count == 1)
            {
                WriteFactoryMethodName("SingletonSeparatedList");
                _w.Write($"<{typeName}>(");
                WriteValue(elemType, items[0]!);
                _w.Write(")");
                return;
            }

            WriteFactoryMethodName("SeparatedList");
            _w.Write($"<{typeName}>(new {typeName}[]");
            _w.WriteLine();
            _w.WriteLine("{");
            _w.Indent();

            for (int i = 0; i < items.Count; i++)
            {
                WriteValue(elemType, items[i]!);
                if (i < items.Count - 1)
                    _w.WriteLine(",");
                else
                    _w.WriteLine();
            }

            _w.Unindent();
            _w.Write("})");
        }

        private static bool IsDefaultValue(Type type, object? value)
        {
            if (value is null)
                return true;

            if (!type.IsValueType)
                return false;

            var defaultVal = Activator.CreateInstance(type);
            return Equals(defaultVal, value);
        }

        private static string Literal(string text)
        {
            // Escape backslashes, quotes, and control characters so the
            // generated C# is always valid.
            var escaped = text
                .Replace("\\", "\\\\")
                .Replace("\"", "\\\"")
                .Replace("\r", "\\r")
                .Replace("\n", "\\n")
                .Replace("\t", "\\t");

            return $"\"{escaped}\"";
        }
    }

    // -------------------------------------------------------
    // Simple indentation-aware writer
    // -------------------------------------------------------
    private sealed class CodeWriter
    {
        private readonly StringBuilder _sb = new();
        private int _indent;
        private bool _atStartOfLine = true;

        public void Indent() => _indent++;
        public void Unindent()
        {
            if (_indent > 0) _indent--;
        }

        public void Write(string text)
        {
            if (_atStartOfLine)
            {
                _sb.Append(new string(' ', _indent * 4));
                _atStartOfLine = false;
            }

            _sb.Append(text);
        }

        public void WriteLine(string text = "")
        {
            if (_atStartOfLine && text.Length > 0)
            {
                _sb.Append(new string(' ', _indent * 4));
            }

            _sb.AppendLine(text);
            _atStartOfLine = true;
        }

        public override string ToString() => _sb.ToString();
    }
}
