using System.Collections.Immutable;
using System.Linq.Expressions;
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

    private readonly IDictionary<ISymbol, TypeBuilder> _typeBuilders = new Dictionary<ISymbol, TypeBuilder>();
    private readonly IDictionary<ISymbol, MethodBuilder> _methodBuilders = new Dictionary<ISymbol, MethodBuilder>();
    private readonly IDictionary<ISymbol, LocalBuilder> _localBuilders = new Dictionary<ISymbol, LocalBuilder>();

    private readonly Label end;
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

        ilGenerator.Emit(OpCodes.Nop);
        ilGenerator.Emit(OpCodes.Ret);
    }

    private void GenerateStatement(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement)
    {
        switch (statement)
        {
            case ReturnStatementSyntax returnStatement:
                GenerateReturnStatement(typeBuilder, methodBuilder, iLGenerator, statement, returnStatement);
                break;

            case ExpressionStatementSyntax expressionStatement:
                GenerateExpressionStatement(typeBuilder, methodBuilder, iLGenerator, statement, expressionStatement);
                break;

            case LocalDeclarationStatementSyntax localDeclarationStatement:
                GenerateDeclarationStatement(typeBuilder, methodBuilder, iLGenerator, statement, localDeclarationStatement);
                break;
        }
    }

    private void GenerateReturnStatement(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, ReturnStatementSyntax returnStatement)
    {
        if (returnStatement.Expression is ExpressionSyntax expression)
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, expression);
        }

        iLGenerator.Emit(OpCodes.Ret);
    }

    private void GenerateBlock(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, BlockSyntax block)
    {
        foreach (var s in block.Statements)
        {
            GenerateStatement(typeBuilder, methodBuilder, iLGenerator, s);
        }
    }

    private void GenerateIfStatement(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, IfExpressionSyntax ifStatementSyntax)
    {
        GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, ifStatementSyntax.Condition);

        var elseLabel = iLGenerator.DefineLabel();

        GenerateBranchOpForCondition(ifStatementSyntax.Condition, iLGenerator, elseLabel);

        GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, ifStatementSyntax.Expression);

        if (ifStatementSyntax.ElseClause is ElseClauseSyntax elseClause)
        {
            // Define a label for the end of the 'if' statement
            var endIfLabel = iLGenerator.DefineLabel();

            // Branch to end of 'if' after the 'if' block
            iLGenerator.Emit(OpCodes.Br_S, endIfLabel);

            // Mark the 'else' label
            iLGenerator.MarkLabel(elseLabel);

            // Generate the 'else' block
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, elseClause.Expression);

            // Mark the end of the 'if' statement
            iLGenerator.MarkLabel(endIfLabel);
        }
        else
        {
            // If no 'else' block, mark the 'else' label
            iLGenerator.MarkLabel(elseLabel);
        }
    }

    private static void GenerateBranchOpForCondition(ExpressionSyntax expression, ILGenerator iLGenerator, Label end)
    {
        if (expression is ParenthesizedExpressionSyntax parenthesizedExpression)
        {
            GenerateBranchOpForCondition(parenthesizedExpression.Expression, iLGenerator, end);
            return;
        }

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


    private void GenerateWhileStatement(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, WhileExpressionSyntax whileStatementSyntax)
    {
        var beginLabel = iLGenerator.DefineLabel();
        var endLabel = iLGenerator.DefineLabel();

        iLGenerator.MarkLabel(beginLabel);

        GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, whileStatementSyntax.Condition);

        GenerateBranchOpForCondition(whileStatementSyntax.Condition, iLGenerator, endLabel);

        GenerateStatement(typeBuilder, methodBuilder, iLGenerator, whileStatementSyntax.Statement);

        iLGenerator.Emit(OpCodes.Br_S, beginLabel);

        //End
        iLGenerator.MarkLabel(endLabel);
    }

    private void GenerateExpressionStatement(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, ExpressionStatementSyntax expressionStatement)
    {
        GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, expressionStatement.Expression);

        var symbol = GetSymbolInfo(expressionStatement.Expression).Symbol;

        if (expressionStatement.Expression is InvocationExpressionSyntax invocationExpression)
        {
            symbol = ((IMethodSymbol)symbol).ReturnType;
        }

        // TODO: Handle the case that Pop is required. If not Void, and not assigned anywhere.

        if (symbol is not null && symbol?.UnwrapType()?.SpecialType != SpecialType.System_Void)
        {
            // The value is not used, pop it from the stack.

            iLGenerator.Emit(OpCodes.Pop);
        }
    }

    private void GenerateDeclarationStatement(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, LocalDeclarationStatementSyntax localDeclarationStatement)
    {
        foreach (var declarator in localDeclarationStatement.Declaration.Declarators)
        {
            GenerateDeclarator(typeBuilder, methodBuilder, iLGenerator, statement, localDeclarationStatement, declarator);
        }
    }

    private void GenerateDeclarator(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, LocalDeclarationStatementSyntax localDeclarationStatement, VariableDeclaratorSyntax declarator)
    {
        if (declarator.Initializer is not null)
        {
            var localSymbol = GetSymbolInfo(declarator).Symbol as ILocalSymbol;

            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, declarator.Initializer.Value);

            var localBuilder = _localBuilders[localSymbol];

            iLGenerator.Emit(OpCodes.Stloc, localBuilder);
        }
    }

    private void GenerateExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, ExpressionSyntax expression)
    {
        switch (expression)
        {
            case BinaryExpressionSyntax binaryExpression:
                GenerateBinaryExpression(typeBuilder, methodBuilder, iLGenerator, statement, binaryExpression);
                break;

            case MemberAccessExpressionSyntax memberAccessExpression:
                GenerateMemberAccessExpression(typeBuilder, methodBuilder, iLGenerator, statement, memberAccessExpression);
                break;

            case InvocationExpressionSyntax invocationExpression:
                GenerateInvocationExpression(typeBuilder, methodBuilder, iLGenerator, statement, invocationExpression);
                break;

            case IdentifierNameSyntax identifierName:
                GenerateNameExpression(iLGenerator, identifierName);
                break;

            case LiteralExpressionSyntax literalExpression:
                GenerateLiteralExpression(iLGenerator, literalExpression);
                break;

            case ParenthesizedExpressionSyntax parenthesized:
                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, parenthesized.Expression);
                break;

            case IfExpressionSyntax ifStatementSyntax:
                GenerateIfStatement(typeBuilder, methodBuilder, iLGenerator, statement, ifStatementSyntax);
                break;

            case WhileExpressionSyntax whileStatement:
                GenerateWhileStatement(typeBuilder, methodBuilder, iLGenerator, statement, whileStatement);
                break;

            case BlockSyntax block:
                GenerateBlock(typeBuilder, methodBuilder, iLGenerator, block);
                break;

            case AssignmentExpressionSyntax assignmentExpression:
                GenerateAssignmentExpression(typeBuilder, methodBuilder, iLGenerator, statement, assignmentExpression);
                break;

            case ObjectCreationExpressionSyntax objectCreationExpression:
                GenerateObjectCreationExpression(typeBuilder, methodBuilder, iLGenerator, statement, objectCreationExpression);
                break;

            case CollectionExpressionSyntax collectionExpression:
                GenerateCollectionExpression(typeBuilder, methodBuilder, iLGenerator, statement, collectionExpression);
                break;

            case ElementAccessExpressionSyntax elementAccessExpression:
                GenerateElementAccessExpression(typeBuilder, methodBuilder, iLGenerator, statement, elementAccessExpression);
                break;
        }
    }

    private void GenerateCollectionExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, CollectionExpressionSyntax collectionExpression)
    {
        var target = GetSymbolInfo(collectionExpression).Symbol;

        if (target is IArrayTypeSymbol arrayTypeSymbol)
        {
            iLGenerator.Emit(OpCodes.Ldc_I4, collectionExpression.Elements.Count);
            iLGenerator.Emit(OpCodes.Newarr, arrayTypeSymbol.ElementType.GetClrType(_compilation));

            int index = 0;
            foreach (var element in collectionExpression.Elements)
            {
                iLGenerator.Emit(OpCodes.Dup);
                iLGenerator.Emit(OpCodes.Ldc_I4, index);
                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, element.Expression);
                iLGenerator.Emit(OpCodes.Stelem_I4);
                index++;
            }
        }
    }

    private void GenerateElementAccessExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, ElementAccessExpressionSyntax elementAccessExpression)
    {
        var target = GetSymbolInfo(elementAccessExpression.Expression).Symbol;

        if (target is ILocalSymbol localSymbol
            && localSymbol.Type is IArrayTypeSymbol arrayTypeSymbol)
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, elementAccessExpression.Expression);

            foreach (var argument in elementAccessExpression.ArgumentList.Arguments.Reverse())
            {
                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, argument.Expression);
            }

            iLGenerator.Emit(OpCodes.Ldelem_I4);
        }
    }

    private void GenerateObjectCreationExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, ObjectCreationExpressionSyntax objectCreationExpression)
    {
        foreach (var argument in objectCreationExpression.ArgumentList.Arguments.Reverse())
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, argument.Expression);
        }

        var target = GetSymbolInfo(objectCreationExpression.Type).Symbol as MetadataMethodSymbol;

        iLGenerator.Emit(OpCodes.Newobj, target.GetConstructorInfo());
    }

    private void GenerateAssignmentExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, AssignmentExpressionSyntax assignmentExpression)
    {
        var symbol = GetSymbolInfo(assignmentExpression.LeftHandSide).Symbol;

        if (assignmentExpression.LeftHandSide is ElementAccessExpressionSyntax elementAccessExpression)
        {
            var localSymbol = GetSymbolInfo(elementAccessExpression.Expression).Symbol as ILocalSymbol;

            var localBuilder = _localBuilders[localSymbol];
            iLGenerator.Emit(OpCodes.Ldloc, localBuilder);

            foreach (var argument in elementAccessExpression.ArgumentList.Arguments)
            {
                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, argument.Expression);
            }

            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, assignmentExpression.RightHandSide);

            iLGenerator.Emit(OpCodes.Stelem_I4);
        }
        else
        {
            if (symbol is ILocalSymbol localSymbol)
            {
                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, assignmentExpression.RightHandSide);

                var localBuilder = _localBuilders[localSymbol];

                iLGenerator.Emit(OpCodes.Stloc, localBuilder);
            }
        }
    }

    private SymbolInfo GetSymbolInfo(SyntaxNode syntaxNode)
    {
        return _compilation
                        .GetSemanticModel(syntaxNode.SyntaxTree)
                        .GetSymbolInfo(syntaxNode);
    }

    private void GenerateBinaryExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, BinaryExpressionSyntax binaryExpression)
    {
        GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, binaryExpression.LeftHandSide);
        GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, binaryExpression.RightHandSide);

        var semanticModel = _compilation.GetSemanticModel(binaryExpression.SyntaxTree!);

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

    private void GenerateMemberAccessExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, MemberAccessExpressionSyntax memberAccessExpression)
    {
        GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, memberAccessExpression.Expression);
        GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, memberAccessExpression.Name);
    }

    private void GenerateInvocationExpression(TypeBuilder typeBuilder, MethodBuilder methodBuilder, ILGenerator iLGenerator, StatementSyntax statement, InvocationExpressionSyntax invocationExpression)
    {
        // Resolve target identifier or access
        // If method or delegate, then invoke

        var target = GetSymbolInfo(invocationExpression).Symbol as MetadataMethodSymbol;

        if (!target?.IsStatic ?? false)
        {
            // Instance member invocation

            var expr = invocationExpression.Expression;
            if (invocationExpression.Expression is MemberAccessExpressionSyntax e)
            {
                // Get the target Expression. Ignores the Name, which is the method.
                expr = e.Expression;
            }

            var localSymbol = GetSymbolInfo(expr).Symbol as ILocalSymbol;

            if (localSymbol is not null)
            {
                // A local

                var localBuilder = _localBuilders[localSymbol];

                if (localSymbol.Type.IsValueType)
                {
                    // Loading the address of the value to the instance.

                    iLGenerator.Emit(OpCodes.Ldloca, localBuilder);
                }
                else
                {
                    // Since it's a reference type, the address is stored in the local.

                    iLGenerator.Emit(OpCodes.Ldloc, localBuilder);
                }
            }
            else
            {
                // It's an expression.

                GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, expr);

                if (target.ContainingType.IsValueType)
                {
                    var clrType = target.ContainingType.GetClrType(_compilation);
                    var builder = iLGenerator.DeclareLocal(clrType);
                    //_localBuilders[target] = builder;

                    iLGenerator.Emit(OpCodes.Stloc, builder);
                    iLGenerator.Emit(OpCodes.Ldloca, builder);
                }
            }
        }

        foreach (var argument in invocationExpression.ArgumentList.Arguments.Reverse())
        {
            GenerateExpression(typeBuilder, methodBuilder, iLGenerator, statement, argument.Expression);
        }

        if (target?.IsStatic ?? false)
        {
            iLGenerator.Emit(OpCodes.Call, target.GetMethodInfo());
        }
        else
        {
            if (target.ContainingType.IsValueType)
            {
                iLGenerator.Emit(OpCodes.Call, target.GetMethodInfo());
            }
            else
            {
                iLGenerator.Emit(OpCodes.Callvirt, target.GetMethodInfo());
            }
        }
    }

    private void GenerateNameExpression(ILGenerator iLGenerator, IdentifierNameSyntax identifierName)
    {
        // Resolve target identifier or access
        // If local, property, or field, then load

        var symbol = GetSymbolInfo(identifierName).Symbol;

        if (symbol is ILocalSymbol localSymbol)
        {
            var localBuilder = _localBuilders[localSymbol];

            iLGenerator.Emit(OpCodes.Ldloc, localBuilder);
        }
        else if (symbol is IFieldSymbol fieldSymbol)
        {
            var metadataFieldSymbol = fieldSymbol as MetadataFieldSymbol;

            if (fieldSymbol.IsLiteral)
            {
                if (fieldSymbol.Type.SpecialType == SpecialType.System_Int32)
                {
                    iLGenerator.Emit(OpCodes.Ldc_I4, (int)metadataFieldSymbol.GetConstantValue()!);
                }
                else
                {
                    throw new Exception("Unsupported constant type");
                }
            }
            else
            {
                if (metadataFieldSymbol.IsStatic)
                {
                    iLGenerator.Emit(OpCodes.Ldsfld, metadataFieldSymbol.GetFieldInfo());
                }
                else
                {
                    iLGenerator.Emit(OpCodes.Ldfld, metadataFieldSymbol.GetFieldInfo());
                }
            }
        }
        else if (symbol is IPropertySymbol propertySymbol)
        {
            if (propertySymbol.ContainingType!.Name == "Array") //.SpecialType == SpecialType.System_Array)
            {
                if (propertySymbol.Name == "Length")
                {
                    iLGenerator.Emit(OpCodes.Ldlen);
                }
            }
            else
            {
                var metadataPropertySymbol = propertySymbol as MetadataPropertySymbol;
                var getMethod = metadataPropertySymbol.GetMethod as MetadataMethodSymbol;

                if (!propertySymbol.IsStatic
                    && propertySymbol.ContainingType.IsValueType)
                {
                    var clrType = propertySymbol.ContainingType.GetClrType(_compilation);
                    var builder = iLGenerator.DeclareLocal(clrType);
                    //_localBuilders[symbol] = builder;

                    iLGenerator.Emit(OpCodes.Stloc, builder);
                    iLGenerator.Emit(OpCodes.Ldloca, builder);
                }

                iLGenerator.Emit(OpCodes.Call, getMethod.GetMethodInfo());
            }
        }
    }

    private static void GenerateLiteralExpression(ILGenerator iLGenerator, LiteralExpressionSyntax literalExpression)
    {
        switch (literalExpression.Kind)
        {
            case SyntaxKind.NumericLiteralExpression:
                {
                    var v = literalExpression.Token.ValueText;
                    iLGenerator.Emit(OpCodes.Ldc_I4, int.Parse(v));
                    break;
                }

            case SyntaxKind.StringLiteralExpression:
                {
                    var v = literalExpression.Token.ValueText;
                    iLGenerator.Emit(OpCodes.Ldstr, v.Substring(1, v.Length - 2));
                    break;
                }

            case SyntaxKind.TrueLiteralExpression:
                {
                    var v = literalExpression.Token.ValueText;
                    iLGenerator.Emit(OpCodes.Ldc_I4_1);
                    break;
                }

            case SyntaxKind.FalseLiteralExpression:
                {
                    var v = literalExpression.Token.ValueText;
                    iLGenerator.Emit(OpCodes.Ldc_I4_0);
                    break;
                }

            default:
                iLGenerator.Emit(OpCodes.Ldc_I4, int.Parse(literalExpression.Token.ValueText));
                break;
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