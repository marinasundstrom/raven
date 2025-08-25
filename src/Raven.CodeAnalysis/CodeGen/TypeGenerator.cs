using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class TypeGenerator
{
    readonly Dictionary<IMethodSymbol, MethodGenerator> _methodGenerators = new Dictionary<IMethodSymbol, MethodGenerator>(SymbolEqualityComparer.Default);
    readonly Dictionary<IFieldSymbol, FieldBuilder> _fieldBuilders = new Dictionary<IFieldSymbol, FieldBuilder>(SymbolEqualityComparer.Default);

    private Compilation _compilation;

    public CodeGenerator CodeGen { get; }
    public Compilation Compilation => _compilation ??= CodeGen.Compilation;
    public ITypeSymbol TypeSymbol { get; }
    public TypeBuilder? TypeBuilder { get; private set; }

    public IEnumerable<MethodGenerator> MethodGenerators => _methodGenerators.Values;

    public Type? Type { get; private set; }

    public TypeGenerator(CodeGenerator codeGen, ITypeSymbol typeSymbol)
    {
        CodeGen = codeGen;
        TypeSymbol = typeSymbol;
    }

    public void DefineTypeBuilder()
    {
        TypeAttributes typeAttributes = TypeAttributes.Public;

        if (TypeSymbol.BaseType.Name == "Enum")
        {
            TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                TypeSymbol.MetadataName,
                TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.Serializable,
                ResolveClrType(TypeSymbol.BaseType) // bör vara System.Enum
            );

            // Lägg till value__ direkt här
            TypeBuilder.DefineField(
                "value__",
                Compilation.GetTypeByMetadataName("System.Int32").GetClrType(Compilation),
                FieldAttributes.Public | FieldAttributes.SpecialName | FieldAttributes.RTSpecialName
            );

            return;
        }

        var syntaxReference = TypeSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is not null)
        {
            TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                TypeSymbol.MetadataName,
                typeAttributes,
                ResolveClrType(TypeSymbol.BaseType));
        }
    }

    public void DefineMemberBuilders()
    {
        if (TypeSymbol.BaseType.ContainingNamespace.Name == "System"
            && TypeSymbol.BaseType.Name == "Enum")
        {
            foreach (var fieldSymbol in TypeSymbol.GetMembers().OfType<IFieldSymbol>())
            {
                var fieldBuilder = TypeBuilder.DefineField(
                    fieldSymbol.Name,
                    TypeBuilder,
                    FieldAttributes.Public | FieldAttributes.Static | FieldAttributes.Literal
                );

                fieldBuilder.SetConstant(fieldSymbol.GetConstantValue());

                CodeGen.AddMemberBuilder((SourceSymbol)fieldSymbol, fieldBuilder);
            }

            return;
        }

        foreach (var memberSymbol in TypeSymbol.GetMembers())
        {
            switch (memberSymbol)
            {
                case IMethodSymbol methodSymbol:
                    {
                        var methodGenerator = new MethodGenerator(this, methodSymbol);
                        _methodGenerators[methodSymbol] = methodGenerator;
                        methodGenerator.DefineMethodBuilder();

                        CodeGen.AddMemberBuilder((SourceSymbol)methodSymbol, methodGenerator.MethodBase);
                        break;
                    }
                case IFieldSymbol fieldSymbol:
                    {
                        var type = fieldSymbol.Type.Equals(TypeSymbol, SymbolEqualityComparer.Default) ? TypeBuilder : ResolveClrType(fieldSymbol.Type);

                        FieldAttributes attr = FieldAttributes.Public;

                        if (fieldSymbol.IsLiteral)
                        {
                            attr |= FieldAttributes.Literal;
                        }

                        if (fieldSymbol.IsStatic)
                        {
                            attr |= FieldAttributes.Static;
                        }

                        var fieldBuilder = TypeBuilder.DefineField(fieldSymbol.Name, type, attr);
                        if (fieldSymbol.IsLiteral)
                            fieldBuilder.SetConstant(fieldSymbol.GetConstantValue());
                        _fieldBuilders[fieldSymbol] = fieldBuilder;

                        CodeGen.AddMemberBuilder((SourceSymbol)fieldSymbol, fieldBuilder);
                        break;
                    }
                case IPropertySymbol propertySymbol:
                    {
                        MethodGenerator? getGen = null;
                        MethodGenerator? setGen = null;

                        if (propertySymbol.GetMethod is IMethodSymbol getMethod)
                        {
                            getGen = new MethodGenerator(this, getMethod);
                            _methodGenerators[getMethod] = getGen;
                            getGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)getMethod, getGen.MethodBase);
                        }

                        if (propertySymbol.SetMethod is IMethodSymbol setMethod)
                        {
                            setGen = new MethodGenerator(this, setMethod);
                            _methodGenerators[setMethod] = setGen;
                            setGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)setMethod, setGen.MethodBase);
                        }

                        var propertyType = ResolveClrType(propertySymbol.Type);
                        var paramTypes = propertySymbol.GetMethod?.Parameters.Select(p => ResolveClrType(p.Type)).ToArray() ?? Type.EmptyTypes;
                        var propBuilder = TypeBuilder.DefineProperty(propertySymbol.Name, PropertyAttributes.None, propertyType, paramTypes);

                        if (getGen != null)
                            propBuilder.SetGetMethod((MethodBuilder)getGen.MethodBase);
                        if (setGen != null)
                            propBuilder.SetSetMethod((MethodBuilder)setGen.MethodBase);

                        CodeGen.AddMemberBuilder((SourceSymbol)propertySymbol, propBuilder);
                        break;
                    }
            }
        }
    }

    public void EmitMemberILBodies()
    {
        foreach (var methodGenerators in _methodGenerators.Values.ToList())
        {
            methodGenerators.EmitBody();
        }
    }

    public Type CreateType() => TypeBuilder!.CreateType();

    public bool HasMethodGenerator(IMethodSymbol methodSymbol)
    {
        return _methodGenerators.ContainsKey(methodSymbol);
    }

    public void Add(IMethodSymbol methodSymbol, MethodGenerator methodGenerator)
    {
        _methodGenerators[methodSymbol] = methodGenerator;
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(CodeGen);
    }
}