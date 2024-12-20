using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Runtime.InteropServices.Marshalling;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class CodeGenerator
{
    private PersistedAssemblyBuilder assemblyBuilder;
    private ModuleBuilder moduleBuilder;
    private MethodBuilder entryPoint;

    private IDictionary<ISymbol, TypeBuilder> _typeBuilders = new Dictionary<ISymbol, TypeBuilder>();
    private IDictionary<ISymbol, MethodBuilder> _methodBuilders = new Dictionary<ISymbol, MethodBuilder>();
    
    IEnumerable<string> versions = [
        ".NETStandard,Version=v2.0",
        ".NETStandard,Version=v2.1",
        ".NETFramework,Version=v7.8",
        ".NETCoreApp,Version=v6.0",
        ".NETCoreApp,Version=v7.0",
        ".NETCoreApp,Version=v8.0",
        ".NETCoreApp,Version=v9.0"
    ];
    
    private Label end;

    public void Generate(Compilation compilation, Stream peStream, Stream? pdbStream)
    {
        var assemblyName = new AssemblyName(compilation.Name);
        assemblyName.Version = new Version(1, 0, 0, 0);

        var targetFrameworkAttribute = new CustomAttributeBuilder(
            typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetConstructor([typeof(string)]),
            [".NETCoreApp,Version=v9.0"] // Replace with your version
        );

        assemblyBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, [targetFrameworkAttribute]);

        moduleBuilder = assemblyBuilder.DefineDynamicModule("MyModule");

        var globalNamespace = compilation.GlobalNamespace;

        GenerateNamespace(globalNamespace);

        foreach (var (k, t) in _typeBuilders)
        {
            t.CreateType();
        }

        MetadataBuilder metadataBuilder = assemblyBuilder.GenerateMetadata(out BlobBuilder ilStream, out _, out MetadataBuilder pdbBuilder);
        MethodDefinitionHandle entryPointHandle = MetadataTokens.MethodDefinitionHandle(entryPoint.MetadataToken);
        DebugDirectoryBuilder debugDirectoryBuilder = GeneratePdb(pdbBuilder, metadataBuilder.GetRowCounts(), entryPointHandle);

        ManagedPEBuilder peBuilder = new ManagedPEBuilder(
                        header: new PEHeaderBuilder(imageCharacteristics: Characteristics.ExecutableImage, subsystem: Subsystem.WindowsCui),
                        metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                        ilStream: ilStream,
                        debugDirectoryBuilder: debugDirectoryBuilder,
                        entryPoint: entryPointHandle);

        BlobBuilder peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);

        peBlob.WriteContentTo(peStream);
    }

    private void GenerateNamespace(INamespaceSymbol @namespace)
    {
        foreach (var member in @namespace.GetMembers())
        {
            if (member is ITypeSymbol type)
            {
                GenerateType(type);
            }
            else  if (member is INamespaceSymbol ns)
            {
                GenerateNamespace(ns);
            }
        }
    }

    private void GenerateType(ITypeSymbol type)
    {
        var syntaxReference = type.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is not null)
        {
            TypeBuilder typeBuilder = moduleBuilder.DefineType("Program", TypeAttributes.Public | TypeAttributes.Class);

            _typeBuilders[type] = typeBuilder;
            
            foreach (var member in type.GetMembers())
            {
                if (member is IMethodSymbol method)
                {
                    MethodBuilder methodBuilder = typeBuilder.DefineMethod("Main", MethodAttributes.HideBySig | MethodAttributes.Public | MethodAttributes.Static, CallingConventions.Standard, typeof(int), []);

                    _methodBuilders[type] = methodBuilder;
                    
                    ILGenerator il = methodBuilder.GetILGenerator();

                    var syntax = syntaxReference.GetSyntax();

                    if (syntax is CompilationUnitSyntax compilationUnit)
                    {
                        var statements = compilationUnit.Members.OfType<GlobalStatementSyntax>()
                            .Select(x => x.Statement);
                        
                        GenerateIL(method, typeBuilder, methodBuilder, statements, il);
                    }
                    else  if (syntax is MethodDeclarationSyntax methodDeclaration)
                    {
                        GenerateIL(method, typeBuilder, methodBuilder, methodDeclaration.Body.Statements.ToList(), il);
                    }
                    
                    if (method.Name == "Main")
                    {
                        entryPoint = methodBuilder;
                    }
                }
            }
        }
    }

    private void GenerateIL(IMethodSymbol symbol, TypeBuilder typeBuilder, MethodBuilder methodBuilder, IEnumerable<StatementSyntax> statements, ILGenerator ilGenerator)
    {
        foreach (var statement in statements)
        {
            GenerateStatement(typeBuilder, methodBuilder, ilGenerator, statement);
        }
    }

    private void GenerateStatement(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement)
    {
        if (statement is ReturnStatementSyntax returnStatement)
        {
            if (returnStatement.Expression is ExpressionSyntax expression)
            {
                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, expression);
            }

            iLGenerator.Emit(OpCodes.Ret);
        }
        else if (statement is BlockSyntax block)
        {
            foreach (var s in block.Statements)
            {
                GenerateStatement(typeBuilder, methodBuilder, iLGenerator, s);
            }
        }
        else if (statement is IfStatementSyntax ifStatementSyntax)
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, ifStatementSyntax.Condition);

            var elseLabel = iLGenerator.DefineLabel();

            GenerateBranchOpForCondition(ifStatementSyntax.Condition, iLGenerator, elseLabel);

            GenerateStatement(typeBuilder, methodBuilder, iLGenerator, ifStatementSyntax.Statement);

            if (ifStatementSyntax.ElseClause is ElseClauseSyntax elseClause)
            {
                iLGenerator.MarkLabel(elseLabel);

                GenerateStatement(typeBuilder, methodBuilder, iLGenerator, elseClause.Statement);
            }
        }
    }

    private static void GenerateBranchOpForCondition(ExpressionSyntax expression, ILGenerator iLGenerator, Label end)
    {
        if (expression is BinaryExpressionSyntax binaryExpression)
        {
            switch (binaryExpression.Kind)
            {
                case SyntaxKind.EqualsExpression:
                    iLGenerator.Emit(OpCodes.Ceq);
                    iLGenerator.Emit(OpCodes.Neg);
                    iLGenerator.Emit(OpCodes.Beq_S, end);
                    break;

                case SyntaxKind.NotEqualsExpression:
                    iLGenerator.Emit(OpCodes.Beq_S, end);
                    break;

                case SyntaxKind.GreaterThanExpression:
                    iLGenerator.Emit(OpCodes.Ble_S, end);
                    break;

                case SyntaxKind.LessThanExpression:
                    iLGenerator.Emit(OpCodes.Bge_S, end);
                    break;
            }
        }
    }

    private void GenerateExpression(TypeBuilder typeBuilder, MethodBuilder entryPoint, ILGenerator iLGenerator, StatementSyntax statement, ExpressionSyntax expression)
    {
        if (expression is BinaryExpressionSyntax binaryExpression)
        {
            GenerateExpression(typeBuilder, entryPoint, iLGenerator, statement, binaryExpression.LeftHandSide);
            GenerateExpression(typeBuilder, entryPoint, iLGenerator, statement, binaryExpression.RightHandSide);

            switch (binaryExpression.Kind)
            {
                case SyntaxKind.AddExpression:
                    iLGenerator.Emit(OpCodes.Add);
                    break;

                case SyntaxKind.SubtractExpression:
                    iLGenerator.Emit(OpCodes.Sub);
                    break;

                case SyntaxKind.MultiplyExpression:
                    iLGenerator.Emit(OpCodes.Mul);
                    break;

                case SyntaxKind.DivideExpression:
                    iLGenerator.Emit(OpCodes.Div);
                    break;

                case SyntaxKind.ModuloExpression:
                    iLGenerator.Emit(OpCodes.Rem);
                    break;
            }
        }
        else if (expression is InvocationExpressionSyntax invocationExpression)
        {
            iLGenerator.Emit(OpCodes.Ldc_I4, 42);
        }
        else if (expression is IdentifierNameSyntax identifierName)
        {
            iLGenerator.Emit(OpCodes.Ldc_I4, 100);
        }
        else if (expression is LiteralExpressionSyntax literalExpression)
        {
            iLGenerator.Emit(OpCodes.Ldc_I4, int.Parse(literalExpression.Token.ValueText));
        }
        else if (expression is ParenthesizedExpressionSyntax parenthesized)
        {
            GenerateExpression(typeBuilder, entryPoint, iLGenerator, statement, parenthesized.Expression);
        }
    }

    static DebugDirectoryBuilder GeneratePdb(MetadataBuilder pdbBuilder, ImmutableArray<int> rowCounts, MethodDefinitionHandle entryPointHandle)
    {
        BlobBuilder portablePdbBlob = new BlobBuilder();
        PortablePdbBuilder portablePdbBuilder = new PortablePdbBuilder(pdbBuilder, rowCounts, entryPointHandle);
        BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);
        // In case saving PDB to a file
        using FileStream fileStream = new FileStream("MyAssemblyEmbeddedSource.pdb", FileMode.Create, FileAccess.Write);
        portablePdbBlob.WriteContentTo(fileStream);

        DebugDirectoryBuilder debugDirectoryBuilder = new DebugDirectoryBuilder();
        debugDirectoryBuilder.AddCodeViewEntry("MyAssemblyEmbeddedSource.pdb", pdbContentId, portablePdbBuilder.FormatVersion);
        // In case embedded in PE:
        // debugDirectoryBuilder.AddEmbeddedPortablePdbEntry(portablePdbBlob, portablePdbBuilder.FormatVersion);
        return debugDirectoryBuilder;
    }
}