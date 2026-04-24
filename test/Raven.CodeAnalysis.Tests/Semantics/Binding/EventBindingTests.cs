using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class EventBindingTests : DiagnosticTestBase
{
    [Fact]
    public void InvokingEventOutsideContainingType_ReportsDiagnostic()
    {
        const string code = """
class Button {
    event Clicked: System.Action;
}

func Use(button: Button) -> unit {
    button.Clicked();
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV0201").WithAnySpan().WithArguments("Clicked")]);

        verifier.Verify();
    }

    [Fact]
    public void AssigningEventWithEquals_ReportsDiagnostic()
    {
        const string code = """
class Button {
    event Clicked: System.Action;

    func Assign(handler: System.Action) -> unit {
        Clicked = handler;
    }
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV0201").WithAnySpan().WithArguments("Clicked")]);

        verifier.Verify();
    }

    [Fact]
    public void InvokingCustomEvent_ReportsDiagnostic()
    {
        const string code = """
class Button {
    event Clicked: System.Action {
        add { }
        remove { }
    }

    func Fire() -> unit {
        Clicked();
    }
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV0202").WithAnySpan().WithArguments("Clicked")]);

        verifier.Verify();
    }

    [Fact]
    public void EventSubscription_MemberAccessSymbolInfo_ResolvesToEvent()
    {
        const string code = """
class Button {
    event Clicked: System.Action;

    func Subscribe(handler: System.Action) -> unit {
        self.Clicked += handler;
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var eventDeclaration = root.DescendantNodes().OfType<EventDeclarationSyntax>().Single();
        var declaredEvent = Assert.IsAssignableFrom<IEventSymbol>(model.GetDeclaredSymbol(eventDeclaration));

        var memberAccess = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(m => m.Name.Identifier.ValueText == "Clicked");

        var symbol = model.GetSymbolInfo(memberAccess).Symbol;
        var referencedEvent = Assert.IsAssignableFrom<IEventSymbol>(symbol);
        Assert.True(SymbolEqualityComparer.Default.Equals(declaredEvent, referencedEvent));
    }

    [Fact]
    public void EventSubscription_IdentifierSymbolInfo_ResolvesToEvent()
    {
        const string code = """
class Button {
    event Clicked: System.Action;

    func Subscribe(handler: System.Action) -> unit {
        Clicked += handler;
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var eventDeclaration = root.DescendantNodes().OfType<EventDeclarationSyntax>().Single();
        var declaredEvent = Assert.IsAssignableFrom<IEventSymbol>(model.GetDeclaredSymbol(eventDeclaration));

        var identifier = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "Clicked");

        var symbol = model.GetSymbolInfo(identifier).Symbol;
        var referencedEvent = Assert.IsAssignableFrom<IEventSymbol>(symbol);
        Assert.True(SymbolEqualityComparer.Default.Equals(declaredEvent, referencedEvent));
    }

    [Fact]
    public void WithInitializer_EventSubscription_WithPlusEquals_BindsWithoutDiagnostics()
    {
        const string code = """
class Button {
    event Clicked: System.Action;
}

func Build(handler: System.Action) -> Button {
    return Button with {
        Clicked += handler
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void WithInitializer_EventAssignment_WithEquals_ReportsDiagnostic()
    {
        const string code = """
class Button {
    event Clicked: System.Action;
}

func Build(handler: System.Action) -> Button {
    return Button with {
        Clicked = handler
    }
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV0201").WithAnySpan().WithArguments("Clicked")]);

        verifier.Verify();
    }
}
