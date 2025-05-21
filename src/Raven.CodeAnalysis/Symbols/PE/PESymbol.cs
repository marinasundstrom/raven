using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class PESymbol : Symbol
{
    protected PESymbol(ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations) : base(containingSymbol, containingType, containingNamespace, locations, [])
    {

    }

    public override IAssemblySymbol ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override IModuleSymbol ContainingModule => ContainingNamespace?.ContainingModule;

    protected PEAssemblySymbol PEContainingAssembly => (PEAssemblySymbol)ContainingAssembly;

    protected PEModuleSymbol PEContainingModule => (PEModuleSymbol)ContainingModule;

    protected Accessibility MapAccessibility(MemberInfo memberInfo)
    {
        if (memberInfo is MethodBase method)
        {
            if (method.IsPublic)
                return Accessibility.Public;
            //if (method.IsFamily)
            //    return Accessibility.Protected;
            if (method.IsAssembly)
                return Accessibility.Internal;
            if (method.IsFamilyOrAssembly)
                return Accessibility.ProtectedOrInternal;
            if (method.IsPrivate)
                return Accessibility.Private;

            return Accessibility.NotApplicable;
        }

        if (memberInfo is FieldInfo field)
        {
            if (field.IsPublic)
                return Accessibility.Public;
            //if (field.IsFamily)
            //    return Accessibility.Protected;
            if (field.IsAssembly)
                return Accessibility.Internal;
            if (field.IsFamilyOrAssembly)
                return Accessibility.ProtectedOrInternal;
            if (field.IsPrivate)
                return Accessibility.Private;

            return Accessibility.NotApplicable;
        }

        if (memberInfo is PropertyInfo prop)
        {
            var accessor = prop.GetMethod ?? prop.SetMethod;
            if (accessor != null)
                return MapAccessibility(accessor);
            return Accessibility.NotApplicable;
        }

        if (memberInfo is EventInfo evt)
        {
            var accessor = evt.AddMethod ?? evt.RemoveMethod;
            if (accessor != null)
                return MapAccessibility(accessor);
            return Accessibility.NotApplicable;
        }

        if (memberInfo is Type type)
        {
            if (type.IsNested)
            {
                if (type.IsNestedPublic)
                    return Accessibility.Public;
                //if (type.IsNestedFamily)
                //    return Accessibility.Protected;
                if (type.IsNestedAssembly)
                    return Accessibility.Internal;
                if (type.IsNestedFamORAssem)
                    return Accessibility.ProtectedOrInternal;
                if (type.IsNestedPrivate)
                    return Accessibility.Private;
            }
            else
            {
                return type.IsPublic ? Accessibility.Public : Accessibility.Internal;
            }
        }

        return Accessibility.NotApplicable;
    }
}