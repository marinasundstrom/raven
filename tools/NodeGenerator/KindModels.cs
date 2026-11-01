internal record TokenKindModel(
    string Name,
    string? Text = null,
    string? Value = null,
    bool IsReservedWord = false,
    bool IsTrivia = false,
    bool IsUnaryOperator = false,
    bool IsBinaryOperator = false,
    int? Precedence = null);

internal record NodeKindModel(string Name, string Type);
