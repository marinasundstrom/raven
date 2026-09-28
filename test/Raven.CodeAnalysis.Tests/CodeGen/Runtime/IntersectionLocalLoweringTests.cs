using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests.Utilities;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class IntersectionLocalLoweringTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void LoweredLocal_ExecutesBothMemberViewsAndReassignment(bool reverse, bool reassign)
    {
        var body = """
            var stored: object = Cell()
            ((IWrite)stored).Write(42)
            """;
        if (reassign)
            body += "\nstored = Cell()\n((IWrite)stored).Write(7)";
        body += "\nreturn ((IRead)stored).Value";
        var actual = EmitAndRun(body, "int", reverse, removeReceiverCasts: true);
        Assert.Equal(reassign ? 7 : 42, actual);
    }

    [Fact]
    public void LoweredProjections_PreserveIdentityAndEvaluateInitializerOnce()
    {
        Assert.Equal(true, EmitAndRun("""
            let stored: object = Cell()
            let read: IRead = (IRead)stored
            let write: IWrite = (IWrite)stored
            write.Write(42)
            return System.Object.ReferenceEquals(read, write) && read.Value == 42 && Cell.Created == 1
            """, "bool", reverse: false, removeReceiverCasts: false));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void LoweredClassView_PreservesVirtualDispatch(bool reverse)
    {
        Assert.Equal(40, EmitAndRun("""
            let stored: object = Cell()
            ((IWrite)stored).Write(2)
            return ((Base)stored).ReadBase()
            """, "int", reverse, removeReceiverCasts: true, classView: true));
    }

    private static object? EmitAndRun(string body, string returnType, bool reverse, bool removeReceiverCasts, bool classView = false)
    {
        var tree = SyntaxTree.ParseText($$"""
            interface IRead { val Value: int }
            interface IWrite { func Write(value: int) -> unit }
            open class Base { public virtual func ReadBase() -> int => 1 }
            class Cell: Base, IRead, IWrite {
                public static field Created: int = 0
                private field value: int = 0
                init() { Created = Created + 1 }
                public val Value: int => value
                public override func ReadBase() -> int => 40
                public func Write(next: int) -> unit { value = next }
            }
            public class Runner {
                public static func Run() -> {{returnType}} {
                    {{body}}
                }
            }
            """);
        var compilation = Compilation.Create("intersection-local-lowering", [tree],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "Run");
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        var original = (BoundBlockStatement)model.GetBoundNode(method.Body!)!;
        var read = compilation.GetTypeByMetadataName(classView ? "Base" : "IRead")!;
        var write = compilation.GetTypeByMetadataName("IWrite")!;
        var intersection = reverse ? compilation.CreateIntersectionTypeSymbol(write, read) : compilation.CreateIntersectionTypeSymbol(read, write);

        // Supply the bound input the future source binder will produce, then execute
        // the real lowerer and emitter without weakening the source annotation gate.
        var rewriter = new IntersectionInputRewriter(compilation, intersection, removeReceiverCasts);
        var input = (BoundBlockStatement)rewriter.Visit(original)!;
        Assert.NotNull(rewriter.Local);
        var lowered = Lowerer.LowerBlock(methodSymbol, input);
        Assert.Same(intersection, rewriter.Local!.Type);
        model.CacheLoweredBoundNode(method.Body!, lowered);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        return loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null);
    }

    private sealed class IntersectionInputRewriter(Compilation compilation, ITypeSymbol intersection, bool removeReceiverCasts) : BoundTreeRewriter
    {
        private ILocalSymbol? _original;
        public ILocalSymbol? Local { get; private set; }

        public override BoundNode? VisitVariableDeclarator(BoundVariableDeclarator node)
        {
            if (node.Local.Name != "stored")
                return base.VisitVariableDeclarator(node);
            _original = node.Local;
            Local = new SourceLocalSymbol(node.Local.Name, intersection, node.Local.IsMutable,
                node.Local.ContainingSymbol, node.Local.ContainingType, node.Local.ContainingNamespace, [], []);
            return new BoundVariableDeclarator(Local, Membership(node.Initializer!));
        }

        private BoundExpression Membership(BoundExpression value)
        {
            var initializer = value is BoundConversionExpression conversion ? conversion.Expression : value;
            var membership = compilation.ClassifyConversion(initializer.Type!, intersection);
            Assert.True(membership.IsImplicit && membership.IsReference);
            return new BoundConversionExpression(initializer, intersection, membership);
        }

        public override BoundNode? VisitLocalAccess(BoundLocalAccess node)
            => ReferenceEquals(node.Local, _original) ? new BoundLocalAccess(Local!) : base.VisitLocalAccess(node);

        public override BoundNode? VisitLocalAssignmentExpression(BoundLocalAssignmentExpression node)
            => ReferenceEquals(node.Local, _original)
                ? new BoundLocalAssignmentExpression(Local!, new BoundLocalAccess(Local!), Membership(node.Right), node.UnitType)
                : base.VisitLocalAssignmentExpression(node);

        public override BoundNode? VisitConversionExpression(BoundConversionExpression node)
        {
            if (node.Expression is BoundLocalAccess local && ReferenceEquals(local.Local, _original) &&
                node.Type is INamedTypeSymbol { SpecialType: not SpecialType.System_Object })
            {
                var receiver = new BoundLocalAccess(Local!);
                var projection = compilation.ClassifyConversion(intersection, node.Type);
                Assert.True(projection.IsImplicit && projection.IsReference);
                return removeReceiverCasts ? receiver : new BoundConversionExpression(receiver, node.Type,
                    projection);
            }
            return base.VisitConversionExpression(node);
        }
    }
}
