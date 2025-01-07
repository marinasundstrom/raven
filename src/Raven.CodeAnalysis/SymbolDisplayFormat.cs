namespace Raven.CodeAnalysis;

public class SymbolDisplayFormat
{
    public static SymbolDisplayFormat CSharpErrorMessageFormat { get; } = new SymbolDisplayFormat
    {
        TypeQualificationStyle = SymbolDisplayTypeQualificationStyle.NameAndContainingTypes,
        GenericsOptions = SymbolDisplayGenericsOptions.IncludeTypeParameters,
        MemberOptions = SymbolDisplayMemberOptions.IncludeContainingType,
        MiscellaneousOptions = SymbolDisplayMiscellaneousOptions.EscapeIdentifiers
    };

    public static SymbolDisplayFormat CSharpShortErrorMessageFormat { get; } = new SymbolDisplayFormat
    {
        TypeQualificationStyle = SymbolDisplayTypeQualificationStyle.NameOnly,
        GenericsOptions = SymbolDisplayGenericsOptions.None,
        MemberOptions = SymbolDisplayMemberOptions.None
    };

    public static SymbolDisplayFormat FullyQualifiedFormat { get; } = new SymbolDisplayFormat
    {
        DelegateStyle = SymbolDisplayDelegateStyle.NameAndSignature,
        ExtensionMethodStyle = SymbolDisplayExtensionMethodStyle.InstanceMethod,
        GenericsOptions = SymbolDisplayGenericsOptions.IncludeTypeParameters | SymbolDisplayGenericsOptions.IncludeTypeConstraints,
        GlobalNamespaceStyle = SymbolDisplayGlobalNamespaceStyle.Included,
        KindOptions = SymbolDisplayKindOptions.IncludeNamespaceKeyword | SymbolDisplayKindOptions.IncludeTypeKeyword,
        LocalOptions = SymbolDisplayLocalOptions.IncludeType,
        MemberOptions = SymbolDisplayMemberOptions.IncludeAccessibility | SymbolDisplayMemberOptions.IncludeParameters,
        MiscellaneousOptions = SymbolDisplayMiscellaneousOptions.UseSpecialTypes,
        PropertyStyle = SymbolDisplayPropertyStyle.ShowReadWriteDescriptor,
        TypeQualificationStyle = SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces
    };

    /*
     * new SymbolDisplayFormat
       {
           TypeQualificationStyle = SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
           GenericsOptions = SymbolDisplayGenericsOptions.IncludeTypeParameters,
           GlobalNamespaceStyle = SymbolDisplayGlobalNamespaceStyle.Included
       };
     */

    public static SymbolDisplayFormat MinimallyQualifiedFormat { get; } = new SymbolDisplayFormat
    {
        TypeQualificationStyle = SymbolDisplayTypeQualificationStyle.NameOnly,
        GenericsOptions = SymbolDisplayGenericsOptions.None,
        MiscellaneousOptions = SymbolDisplayMiscellaneousOptions.UseSpecialTypes
    };

    public SymbolDisplayDelegateStyle DelegateStyle { get; private set; }

    public SymbolDisplayExtensionMethodStyle ExtensionMethodStyle { get; private set; }

    public SymbolDisplayGenericsOptions GenericsOptions { get; private set; }

    public SymbolDisplayGlobalNamespaceStyle GlobalNamespaceStyle { get; private set; }

    public SymbolDisplayKindOptions KindOptions { get; private set; }

    public SymbolDisplayLocalOptions LocalOptions { get; private set; }

    public SymbolDisplayMemberOptions MemberOptions { get; private set; }

    public SymbolDisplayMiscellaneousOptions MiscellaneousOptions { get; private set; }

    public SymbolDisplayParameterOptions ParameterOptions { get; private set; }

    public SymbolDisplayPropertyStyle PropertyStyle { get; private set; }

    public SymbolDisplayTypeQualificationStyle TypeQualificationStyle { get; private set; }
}

public enum SymbolDisplayPropertyStyle
{
    NameOnly = 0,
    ShowReadWriteDescriptor = 1,
}

public enum SymbolDisplayTypeQualificationStyle
{
    NameOnly = 0,
    NameAndContainingTypes = 1,
    NameAndContainingTypesAndNamespaces = 2,
}

public enum SymbolDisplayDelegateStyle
{
    NameOnly = 0,
    NameAndParameters = 1,
    NameAndSignature = 2
}

public enum SymbolDisplayExtensionMethodStyle
{
    Default = 0,
    InstanceMethod = 1,
    StaticMethod = 2
}

[Flags]
public enum SymbolDisplayGenericsOptions
{
    None = 0,
    IncludeTypeParameters = 1,
    IncludeTypeConstraints = 2,
    IncludeVariance = 4
}

public enum SymbolDisplayGlobalNamespaceStyle
{
    Omitted = 0,
    OmittedAsContaining = 1,
    Included = 2
}

[Flags]
public enum SymbolDisplayKindOptions
{
    None = 0,
    IncludeNamespaceKeyword = 1,
    IncludeTypeKeyword = 2,
    IncludeMemberKeyword = 4
}

[Flags]
public enum SymbolDisplayLocalOptions
{
    None = 0,
    IncludeType = 1,
    IncludeConstantValue = 2,
    IncludeModifiers = 4,
    IncludeRef = 4
}

[Flags]
public enum SymbolDisplayMemberOptions
{
    None = 0,
    IncludeType = 1,
    IncludeModifiers = 2,
    IncludeAccessibility = 4,
    IncludeExplicitInterface = 8,
    IncludeParameters = 16,
    IncludeContainingType = 32,
    IncludeConstantValue = 64,
    IncludeRef = 128
}

[Flags]
public enum SymbolDisplayMiscellaneousOptions
{
    None = 0,
    UseSpecialTypes = 1,
    EscapeIdentifiers = 2,
    UseAsterisksInMultidimensionalArrays = 4,
    UseErrorTypeSymbolName = 8,
    RemoveAttributeSuffix = 16,
    ExpandNullable = 32,
    IncludeNullableContextAttribute = 64,
    AllowDefaultLiteral = 128,
    IncludeNotNullableReferenceTypeModifier = 256,
    CollapseTupleTypes = 512,
    ExpandedValueTuple = 1024
}

[Flags]
public enum SymbolDisplayParameterOptions
{
    None = 0,
    IncludeExtensionThis = 1,
    IncludeModifiers = 2,
    IncludeParamsRefOut = 2,
    IncludeType = 4,
    IncludeName = 8,
    IncludeDefaultValue = 16,
    IncludeOptionalBrackets = 32
}