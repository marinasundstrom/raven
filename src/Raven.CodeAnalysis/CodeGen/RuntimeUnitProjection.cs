using Mono.Cecil;
using Mono.Cecil.Cil;

namespace Raven.CodeAnalysis.CodeGen;

// Reflection.Emit uses a private intermediate Unit representation. The selected
// target contract owns the emitted value type; CLI no-result returns remain void.
internal static class RuntimeUnitProjection
{
    internal static void Apply(ModuleDefinition module, RuntimeUnitContract? contract, AssemblyNameReference core)
    {
        if (contract is null)
            return;
        var scope = module.AssemblyReferences.FirstOrDefault(reference => reference.FullName == core.FullName);
        if (scope is null)
        {
            scope = core;
            module.AssemblyReferences.Add(scope);
        }
        var separator = contract.TypeName.LastIndexOf('.');
        var target = new TypeReference(separator < 0 ? "" : contract.TypeName[..separator],
            contract.TypeName[(separator + 1)..], module, scope, true);
        var unit = module.GetType("System.Unit");
        if (unit is not null && (!unit.IsValueType || unit.Fields.Any(field => !field.IsStatic)
            || unit.Methods.Any(method => method.IsConstructor && method.IsStatic)))
            throw new InvalidOperationException("The compiler Unit representation must be an empty value.");

        bool IsLocalScope(TypeReference type) => ReferenceEquals(type.Scope, module)
            || type.Scope is AssemblyNameReference scope && scope.FullName == module.Assembly.Name.FullName;
        bool IsUnit(TypeReference type) => type.FullName == "System.Unit"
            && (ReferenceEquals(type, unit) || IsLocalScope(type));
        TypeReference Map(TypeReference type, bool storage = true)
        {
            // Signature proxies may carry the semantic target name with the source
            // unit symbol's provisional scope. It must resolve to the selected core.
            if (IsUnit(type) || storage && type.MetadataType == MetadataType.Void
                || type.FullName == contract.TypeName && IsLocalScope(type) && module.GetType(contract.TypeName) is null)
                return target;
            if (type is GenericInstanceType generic)
            {
                var mapped = new GenericInstanceType(Map(generic.ElementType, false));
                foreach (var argument in generic.GenericArguments)
                    mapped.GenericArguments.Add(Map(argument));
                return mapped;
            }
            if (type is ArrayType array)
            {
                if (array.IsVector)
                    return new ArrayType(Map(array.ElementType));
                var mapped = new ArrayType(Map(array.ElementType), array.Rank);
                for (var i = 0; i < array.Dimensions.Count; i++)
                    mapped.Dimensions[i] = array.Dimensions[i];
                return mapped;
            }
            if (type is RequiredModifierType required)
                return new RequiredModifierType(required.ModifierType, Map(required.ElementType, storage));
            if (type is OptionalModifierType optional)
                return new OptionalModifierType(optional.ModifierType, Map(optional.ElementType, storage));
            if (type is PinnedType pinned)
                return new PinnedType(Map(pinned.ElementType, storage));
            if (type is ByReferenceType byref)
                return new ByReferenceType(Map(byref.ElementType));
            if (type is PointerType pointer)
                return new PointerType(Map(pointer.ElementType, false));
            return type;
        }
        void MapMethod(MethodReference method)
        {
            if (IsUnit(method.DeclaringType))
                throw new InvalidOperationException("Unit implementation members are not target API members.");
            method.DeclaringType = Map(method.DeclaringType, false);
            method.ReturnType = Map(method.ReturnType, false);
            foreach (var parameter in method.Parameters)
                parameter.ParameterType = Map(parameter.ParameterType);
            if (method is GenericInstanceMethod generic)
                for (var i = 0; i < generic.GenericArguments.Count; i++)
                    generic.GenericArguments[i] = Map(generic.GenericArguments[i]);
        }
        foreach (var type in module.GetTypes().Where(type => type != unit).ToArray())
        {
            foreach (var field in type.Fields)
                field.FieldType = Map(field.FieldType);
            foreach (var property in type.Properties)
                property.PropertyType = Map(property.PropertyType);
            foreach (var method in type.Methods)
            {
                MapMethod(method);
                if (!method.HasBody)
                    continue;
                var body = method.Body;
                foreach (var local in body.Variables)
                    local.VariableType = Map(local.VariableType);
                foreach (var instruction in body.Instructions.ToArray())
                {
                    // Materializing a value can grow a branch beyond its short range.
                    instruction.OpCode = instruction.OpCode.Code switch
                    {
                        Code.Br_S => OpCodes.Br,
                        Code.Brtrue_S => OpCodes.Brtrue,
                        Code.Brfalse_S => OpCodes.Brfalse,
                        Code.Beq_S => OpCodes.Beq,
                        Code.Bne_Un_S => OpCodes.Bne_Un,
                        Code.Bge_S => OpCodes.Bge,
                        Code.Bge_Un_S => OpCodes.Bge_Un,
                        Code.Bgt_S => OpCodes.Bgt,
                        Code.Bgt_Un_S => OpCodes.Bgt_Un,
                        Code.Ble_S => OpCodes.Ble,
                        Code.Ble_Un_S => OpCodes.Ble_Un,
                        Code.Blt_S => OpCodes.Blt,
                        Code.Blt_Un_S => OpCodes.Blt_Un,
                        Code.Leave_S => OpCodes.Leave,
                        _ => instruction.OpCode
                    };
                    if (instruction.Operand is FieldReference literal && IsUnit(literal.DeclaringType))
                    {
                        if (instruction.OpCode != OpCodes.Ldsfld || literal.Name != "Value"
                            || unit is null || !unit.Fields.Any(field => field.Name == "Value" && field.IsStatic && field.IsInitOnly))
                            throw new InvalidOperationException("Unsupported use of the compiler Unit representation.");
                        var local = new VariableDefinition(target);
                        body.Variables.Add(local);
                        // Preserve incoming branches and sequence points on the first instruction.
                        instruction.OpCode = OpCodes.Ldloca;
                        instruction.Operand = local;
                        var initialize = Instruction.Create(OpCodes.Initobj, target);
                        var load = Instruction.Create(OpCodes.Ldloc, local);
                        var il = body.GetILProcessor();
                        il.InsertAfter(instruction, initialize);
                        il.InsertAfter(initialize, load);
                    }
                    else if (instruction.Operand is MethodReference called)
                        MapMethod(called);
                    else if (instruction.Operand is FieldReference field)
                    {
                        field.DeclaringType = Map(field.DeclaringType, false);
                        field.FieldType = Map(field.FieldType);
                    }
                    else if (instruction.Operand is TypeReference operand)
                        instruction.Operand = Map(operand);
                }
            }
        }
        if (unit is not null)
            module.Types.Remove(unit);
    }
}
