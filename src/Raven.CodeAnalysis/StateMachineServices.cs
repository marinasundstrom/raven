namespace Raven.CodeAnalysis;

// Canonical type construction and configured async policy remain compilation-owned.
internal interface StateMachineServices : AttributeValidationServices
{
    bool UseHeapAsyncStateMachines { get; }
    bool CaptureAsyncExceptions { get; }
    TypeSymbol ErrorTypeSymbol { get; }
    TypeSymbol CreateArrayTypeSymbol(TypeSymbol elementType, int rank = 1, int? fixedLength = null);
    TypeSymbol CreatePointerTypeSymbol(TypeSymbol pointedAtType);
}
