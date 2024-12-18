using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Runtime.InteropServices.Marshalling;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

public class CodeGenerator
{
    PersistedAssemblyBuilder assemblyBuilder;
    ModuleBuilder moduleBuilder;

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

    public void Generate(Compilation compilation, string assemblyPath)
    {
        string assemblyNameStr = Path.GetFileNameWithoutExtension(assemblyPath);

        var assemblyName = new AssemblyName(assemblyNameStr);
        assemblyName.Version = new Version(1, 0, 0, 0);

        var targetFrameworkAttribute = new CustomAttributeBuilder(
            typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetConstructor([typeof(string)]),
            [".NETCoreApp,Version=v9.0"] // Replace with your version
        );

        assemblyBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, [targetFrameworkAttribute]);

        moduleBuilder = assemblyBuilder.DefineDynamicModule("MyModule");

        TypeBuilder tb = GenerateType();

        tb.CreateType();

        //assemblyBuilder.Save(assemblyPath); // or could save to a Stream

        var compilationUnit = compilation.SyntaxTrees.First().GetRoot();

        var (tb2, entryPoint) = GenerateEntryPointType(compilationUnit);

        tb2.CreateType();

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

        using var fileStream = new FileStream(assemblyPath, FileMode.Create, FileAccess.Write);
        peBlob.WriteContentTo(fileStream);
    }

    private TypeBuilder GenerateType()
    {
        TypeBuilder tb = moduleBuilder.DefineType("MyType", TypeAttributes.Public | TypeAttributes.Class);

        GenerateMethod(tb);

        return tb;
    }

    private (TypeBuilder, MethodInfo) GenerateEntryPointType(CompilationUnitSyntax compilationUnit)
    {
        TypeBuilder tb = moduleBuilder.DefineType("Program", TypeAttributes.Public | TypeAttributes.Class);

        var entryPointMethod = GenerateEntryPoint(tb, compilationUnit);

        return (tb, entryPointMethod);
    }

    private static MethodBuilder GenerateMethod(TypeBuilder tb)
    {
        var mb = tb.DefineMethod("SumMethod", MethodAttributes.Public | MethodAttributes.Static,
            typeof(int), [typeof(int), typeof(int)]);

        ILGenerator il = mb.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ret);

        return mb;
    }

    private MethodBuilder GenerateEntryPoint(TypeBuilder typeBuilder, CompilationUnitSyntax compilationUnit)
    {
        MethodBuilder entryPoint = typeBuilder.DefineMethod("Main", MethodAttributes.HideBySig | MethodAttributes.Public | MethodAttributes.Static, CallingConventions.Standard, typeof(int), []);
        ILGenerator iLGenerator = entryPoint.GetILGenerator();

        var members = compilationUnit.Members;

        end = iLGenerator.DefineLabel();

        GenerateNamespace(typeBuilder, entryPoint, iLGenerator, members);

        iLGenerator.MarkLabel(end);

        //iLGenerator.Emit(OpCodes.Ret);

        return entryPoint;
    }

    private void GenerateNamespace(TypeBuilder typeBuilder, MethodBuilder entryPoint, ILGenerator iLGenerator, SyntaxList<MemberDeclarationSyntax> members)
    {
        foreach (var member in members)
        {
            if (member is GlobalStatementSyntax globalStatement)
            {
                GenerateStatement(typeBuilder, entryPoint, iLGenerator, globalStatement.Statement);
            }
            else if (member is BaseNamespaceDeclarationSyntax baseNamespaceDeclaration)
            {
                GenerateNamespace(typeBuilder, entryPoint, iLGenerator, baseNamespaceDeclaration.Members);
            }
        }
    }

    private void GenerateStatement(TypeBuilder typeBuilder, MethodBuilder entryPoint, ILGenerator iLGenerator, StatementSyntax statement)
    {
        if (statement is ReturnStatementSyntax returnStatement)
        {
            if (returnStatement.Expression is ExpressionSyntax expression)
            {
                GenerateExpression(typeBuilder, entryPoint, iLGenerator, statement, expression);
            }

            iLGenerator.Emit(OpCodes.Ret);
        }
        else if (statement is BlockSyntax block)
        {
            foreach (var s in block.Statements)
            {
                GenerateStatement(typeBuilder, entryPoint, iLGenerator, s);
            }
        }
        else if (statement is IfStatementSyntax ifStatementSyntax)
        {
            GenerateExpression(typeBuilder, entryPoint, iLGenerator, statement, ifStatementSyntax.Condition);

            var elseLabel = iLGenerator.DefineLabel();

            GenerateBranchOpForCondition(ifStatementSyntax.Condition, iLGenerator, elseLabel);

            GenerateStatement(typeBuilder, entryPoint, iLGenerator, ifStatementSyntax.Statement);

            if (ifStatementSyntax.ElseClause is ElseClauseSyntax elseClause)
            {
                iLGenerator.MarkLabel(elseLabel);

                GenerateStatement(typeBuilder, entryPoint, iLGenerator, elseClause.Statement);
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