using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Runtime.InteropServices.Marshalling;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class CodeGenerator
{
    private PersistedAssemblyBuilder assemblyBuilder;
    private ModuleBuilder moduleBuilder;
    private MethodBuilder entryPoint;

    private IDictionary<ISymbol, TypeBuilder> _typeBuilders = new Dictionary<ISymbol, TypeBuilder>();
    private IDictionary<ISymbol, MethodBuilder> _methodBuilders = new Dictionary<ISymbol, MethodBuilder>();
    private IDictionary<ISymbol, LocalBuilder> _localBuilders = new Dictionary<ISymbol, LocalBuilder>();

    private Label end;
    private readonly Compilation _compilation;

    public CodeGenerator(Compilation compilation)
    {
        _compilation = compilation;
    }

    public void Generate(Stream peStream, Stream? pdbStream)
    {
        var assemblyName = new AssemblyName(_compilation.AssemblyName);
        assemblyName.Version = new Version(1, 0, 0, 0);

        var targetFrameworkAttribute = new CustomAttributeBuilder(
            // TODO: This should not be set here
            typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetConstructor([typeof(string)]),
            [".NETCoreApp,Version=v9.0"]  // Replace with your version
        );

        assemblyBuilder = new PersistedAssemblyBuilder(assemblyName, _compilation.CoreAssembly, [targetFrameworkAttribute]);
        moduleBuilder = assemblyBuilder.DefineDynamicModule(_compilation.AssemblyName);

        var globalNamespace = _compilation.GlobalNamespace;

        GenerateNamespace(globalNamespace);

        CreateTypes();

        MetadataBuilder metadataBuilder = assemblyBuilder.GenerateMetadata(out BlobBuilder ilStream, out _, out MetadataBuilder pdbBuilder);
        MethodDefinitionHandle entryPointHandle = MetadataTokens.MethodDefinitionHandle(entryPoint.MetadataToken);
        DebugDirectoryBuilder debugDirectoryBuilder = GeneratePdb(pdbBuilder, metadataBuilder.GetRowCounts(), entryPointHandle);

        Characteristics imageCharacteristics = _compilation.Options.OutputKind switch
        {
            OutputKind.ConsoleApplication => Characteristics.ExecutableImage,
            OutputKind.DynamicallyLinkedLibrary => Characteristics.Dll,
            _ => Characteristics.Dll,
        };

        ManagedPEBuilder peBuilder = new ManagedPEBuilder(
                        header: new PEHeaderBuilder(imageCharacteristics: imageCharacteristics, subsystem: Subsystem.WindowsCui),
                        metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                        ilStream: ilStream,
                        debugDirectoryBuilder: debugDirectoryBuilder,
                        entryPoint: entryPointHandle);

        BlobBuilder peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);

        peBlob.WriteContentTo(peStream);
    }

    private void CreateTypes()
    {
        foreach (var (k, t) in _typeBuilders)
        {
            t.CreateType();
        }
    }

    private void GenerateNamespace(INamespaceSymbol @namespace)
    {
        foreach (var member in @namespace.GetMembers())
        {
            if (member is ITypeSymbol type)
            {
                GenerateType(type);
            }
            else if (member is INamespaceSymbol ns)
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
            TypeBuilder typeBuilder = moduleBuilder.DefineType(type.Name, TypeAttributes.Public | TypeAttributes.Class);

            _typeBuilders[type] = typeBuilder;

            foreach (var member in type.GetMembers())
            {
                if (member is IMethodSymbol method)
                {
                    GenerateMethod(type, typeBuilder, syntaxReference, method);
                }
            }
        }
    }

    private void GenerateMethod(ITypeSymbol type, TypeBuilder typeBuilder, SyntaxReference syntaxReference,
        IMethodSymbol method)
    {
        MethodBuilder methodBuilder = typeBuilder.DefineMethod(method.Name, MethodAttributes.HideBySig | MethodAttributes.Public | MethodAttributes.Static, CallingConventions.Standard, _compilation.CoreAssembly.GetType(typeof(void).FullName), []);

        _methodBuilders[type] = methodBuilder;

        var syntax = syntaxReference.GetSyntax();

        ILGenerator il = methodBuilder.GetILGenerator();

        var semanticModel = this._compilation.GetSemanticModel(syntax.SyntaxTree);

        foreach (var localDeclStmt in syntax.DescendantNodes().OfType<LocalDeclarationStatementSyntax>())
        {
            foreach (var localDeclarator in localDeclStmt.Declaration.Declarators)
            {
                var symbol = semanticModel.GetDeclaredSymbol(localDeclarator) as ILocalSymbol;
                var clrType = symbol.Type.GetClrType(_compilation);
                var builder = il.DeclareLocal(clrType);
                builder.SetLocalSymInfo(symbol.Name);
                _localBuilders[symbol] = builder;
            }
        }

        if (syntax is CompilationUnitSyntax compilationUnit)
        {
            var statements = compilationUnit.Members.OfType<GlobalStatementSyntax>()
                .Select(x => x.Statement);

            GenerateIL(method, typeBuilder, methodBuilder, statements, il);
        }
        else if (syntax is MethodDeclarationSyntax methodDeclaration)
        {
            GenerateIL(method, typeBuilder, methodBuilder, methodDeclaration.Body.Statements.ToList(), il);
        }

        if (method.Name == "Main")
        {
            entryPoint = methodBuilder;
        }
    }

    private void GenerateIL(IMethodSymbol symbol, TypeBuilder typeBuilder, MethodBuilder methodBuilder, IEnumerable<StatementSyntax> statements, ILGenerator ilGenerator)
    {
        foreach (var statement in statements)
        {
            GenerateStatement(typeBuilder, methodBuilder, ilGenerator, statement);
        }

        ilGenerator.Emit(OpCodes.Ret);
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
                // Define a label for the end of the 'if' statement
                var endIfLabel = iLGenerator.DefineLabel();

                // Branch to end of 'if' after the 'if' block
                iLGenerator.Emit(OpCodes.Br_S, endIfLabel);

                // Mark the 'else' label
                iLGenerator.MarkLabel(elseLabel);

                // Generate the 'else' block
                GenerateStatement(typeBuilder, methodBuilder, iLGenerator, elseClause.Statement);

                // Mark the end of the 'if' statement
                iLGenerator.MarkLabel(endIfLabel);
            }
            else
            {
                // If no 'else' block, mark the 'else' label
                iLGenerator.MarkLabel(elseLabel);
            }
        }
        else if (statement is ExpressionStatementSyntax expressionStatement)
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, expressionStatement.Expression);
        }
        else if (statement is LocalDeclarationStatementSyntax localDeclarationStatement)
        {
            foreach (var declarator in localDeclarationStatement.Declaration.Declarators)
            {
                if (declarator.Initializer is not null)
                {
                    var localSymbol = _compilation
                        .GetSemanticModel(localDeclarationStatement.SyntaxTree)
                        .GetSymbolInfo(declarator).Symbol as ILocalSymbol;

                    GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, declarator.Initializer.Value);

                    var localBuilder = _localBuilders[localSymbol];

                    iLGenerator.Emit(OpCodes.Stloc, localBuilder);
                }
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
                    iLGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case SyntaxKind.NotEqualsExpression:
                    iLGenerator.Emit(OpCodes.Neg);
                    iLGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case SyntaxKind.GreaterThanExpression:
                    iLGenerator.Emit(OpCodes.Ble_S, end);
                    break;

                case SyntaxKind.LessThanExpression:
                    iLGenerator.Emit(OpCodes.Bge_S, end);
                    break;
            }
        }
        else if (expression is LiteralExpressionSyntax literalExpression)
        {
            if (literalExpression.Kind == SyntaxKind.TrueLiteralExpression)
            {
                iLGenerator.Emit(OpCodes.Brfalse_S, end);
            }
            else if (literalExpression.Kind == SyntaxKind.FalseLiteralExpression)
            {
                iLGenerator.Emit(OpCodes.Neg);
                iLGenerator.Emit(OpCodes.Brfalse_S, end);
            }
        }
        else if (expression is IdentifierNameSyntax identifierName)
        {
            iLGenerator.Emit(OpCodes.Brfalse_S, end);
        }
    }

    private void GenerateExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, ExpressionSyntax expression)
    {
        if (expression is BinaryExpressionSyntax binaryExpression)
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, binaryExpression.LeftHandSide);
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, binaryExpression.RightHandSide);

            var semanticModel = _compilation.GetSemanticModel(expression.SyntaxTree!);

            var methodSymbol = semanticModel.GetSymbolInfo(binaryExpression).Symbol as IMethodSymbol;

            if (methodSymbol is not null)
            {
                var concatMethod = methodSymbol as MetadataMethodSymbol;

                iLGenerator.Emit(OpCodes.Call, concatMethod.GetMethodInfo());
                return;
            }

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
        else if (expression is MemberAccessExpressionSyntax memberAccessExpression)
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, memberAccessExpression.Expression);
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, memberAccessExpression.Name);
        }
        else if (expression is InvocationExpressionSyntax invocationExpression)
        {
            // Resolve target identifier or access
            // If method or delegate, then invoke

            foreach (var argument in invocationExpression.ArgumentList.Arguments)
            {
                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, argument.Expression);
            }

            var target = _compilation
                .GetSemanticModel(invocationExpression.SyntaxTree)
                .GetSymbolInfo(invocationExpression).Symbol as MetadataMethodSymbol;

            if (!target.GetMethodInfo().IsStatic /* target.IsStatic */)
            {
                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, invocationExpression.Expression);
            }

            iLGenerator.Emit(OpCodes.Call, target.GetMethodInfo());
        }
        else if (expression is IdentifierNameSyntax identifierName)
        {
            // Resolve target identifier or access
            // If local, property, or field, then load

            var localSymbol = _compilation
                .GetSemanticModel(expression.SyntaxTree)
                .GetSymbolInfo(expression).Symbol as ILocalSymbol;

            var localBuilder = _localBuilders[localSymbol];

            iLGenerator.Emit(OpCodes.Ldloc, localBuilder);
        }
        else if (expression is LiteralExpressionSyntax literalExpression)
        {
            if (literalExpression.Kind == SyntaxKind.StringLiteralExpression)
            {
                var v = literalExpression.Token.ValueText;
                iLGenerator.Emit(OpCodes.Ldstr, v.Substring(1, v.Length - 2));
            }
            else if (literalExpression.Kind == SyntaxKind.TrueLiteralExpression)
            {
                var v = literalExpression.Token.ValueText;
                iLGenerator.Emit(OpCodes.Ldc_I4_1);
            }
            else if (literalExpression.Kind == SyntaxKind.FalseLiteralExpression)
            {
                var v = literalExpression.Token.ValueText;
                iLGenerator.Emit(OpCodes.Ldc_I4_0);
            }
            else
            {
                iLGenerator.Emit(OpCodes.Ldc_I4, int.Parse(literalExpression.Token.ValueText));
            }
        }
        else if (expression is ParenthesizedExpressionSyntax parenthesized)
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, parenthesized.Expression);
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