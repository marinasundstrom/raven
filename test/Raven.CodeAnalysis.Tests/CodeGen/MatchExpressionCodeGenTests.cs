using System;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class MatchExpressionCodeGenTests
{
    [Fact]
    public void MatchExpression_WithValueTypeArm_EmitsAndRuns()
    {
        const string code = """
let value = 42
let result = value match {
    int i => i.ToString()
    _ => "None"
}

System.Console.WriteLine(result)
""";

        var output = CodeGen.CodeGenTestUtilities.EmitAndRun(code, "match_value_type");
        if (output is null)
            return;
        Assert.Equal("42", output);
    }

    [Fact]
    public void MatchExpression_AsReturnValue_EmitsAndRuns()
    {
        const string code = """
let describer = Describer()
let zero = describer.Describe(0)
let two = describer.Describe(2)

System.Console.WriteLine(zero + "," + two)

class Describer {
    Describe(value: int) -> string {
        return value match {
            0 => "zero"
            _ => value.ToString()
        }
    }
}
""";

        var output = CodeGen.CodeGenTestUtilities.EmitAndRun(code, "match_return_value");
        if (output is null)
            return;
        Assert.Equal("zero,2", output);
    }

    [Fact]
    public void MatchExpression_WithStringLiteralPattern_MatchesExactValue()
    {
        const string code = """
let foo = "foo" match {
    "foo" => "str"
    _ => "None"
}

let empty = "" match {
    "foo" => "str"
    _ => "None"
}

System.Console.WriteLine(foo + "," + empty)
""";

        var output = CodeGen.CodeGenTestUtilities.EmitAndRun(code, "match_string_literal");
        if (output is null)
            return;
        Assert.Equal("str,None", output);
    }

    [Fact]
    public void MatchExpression_WithUnionTupleArm_EmitsAndRuns()
    {
        const string code = """
let describer = Describer()
let tuple: bool | (flag: bool, text: string) = (false, "tuple")
let boolResult = describer.Describe(false)
let tupleResult = describer.Describe(tuple)

System.Console.WriteLine(boolResult + "," + tupleResult)

class Describer {
    Describe(value: bool | (flag: bool, text: string)) -> string {
        return value match {
            true => "true"
            false => "false"
            (flag: bool, text: string) => text
        }
    }
}
""";

        var output = CodeGen.CodeGenTestUtilities.EmitAndRun(code, "match_union_tuple");
        if (output is null)
            return;
        Assert.Equal("false,tuple", output);
    }

    [Fact]
    public void MatchExpression_WithUnionTupleFallback_EmitsAndRuns()
    {
        const string code = """
let tuple = (42, 2)
let foo = tuple.Item1
let tuple2 = (42, "Bar")
let name = tuple2.Item2
let x: bool | (flag: bool, text: string) = false

let r = x match {
    (flag: bool, text: string) => "tuple"
    _ => "none"
}

System.Console.WriteLine(r)
""";

        var output = CodeGen.CodeGenTestUtilities.EmitAndRun(code, "match_union_tuple_fallback");
        if (output is null)
            return;

        Assert.Equal("none", output);
    }

    [Fact]
    public void MatchExpression_WithMixedPrimitiveAndTupleArms_EmitsAndRuns()
    {
        const string code = """
let describer = Describer()
let boolResult = describer.Describe(false)
let tupleValue: bool | (flag: bool, text: string) = (false, "tuple")
let tupleResult = describer.Describe(tupleValue)

System.Console.WriteLine(boolResult + "," + tupleResult)

class Describer {
    Describe(value: bool | (flag: bool, text: string)) -> string {
        return value match {
            false => "false"
            true => "true"
            (flag: bool, text: string) => text
            _ => "none"
        }
    }
}
""";

        var output = CodeGen.CodeGenTestUtilities.EmitAndRun(code, "match_union_mixed_tuple");
        if (output is null)
            return;

        Assert.Equal("false,tuple", output);
    }

}
