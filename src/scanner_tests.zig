const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;

// tests
fn compareOutputWithTokensOutput(expected_output: []const u8, file_contents: []const u8) !void {
    var runtime_start: usize = 0;
    _ = &runtime_start;

    var scanner = try Scanner.new(file_contents[runtime_start..], std.testing.allocator);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    const errors = try scanner.getErrors();
    for (errors) |error_type| {
        try buffer.writer().print("{s}", .{error_type});
    }
    for (tokens) |token| {
        try buffer.writer().print("{s}", .{token});
    }

    std.debug.print("START TEST:\n", .{});
    std.debug.print("Expected: \n{s}", .{expected_output});
    std.debug.print("Actual: \n{s}", .{buffer.items});
    std.debug.print("END TEST\n\n", .{});
    try std.testing.expect(std.mem.eql(u8, expected_output, buffer.items));
}

test "empty file print EOF null" {
    const file_contents = "";
    const expected_output = "EOF  null\n";

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can parse braces, parens, plus, minus, star, comma, semicolon, and dot" {
    const file_contents =
        \\()
        \\{}
        \\+-*
        \\,.,
        \\;;
    ;
    const expected_output =
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\PLUS + null
        \\MINUS - null
        \\STAR * null
        \\COMMA , null
        \\DOT . null
        \\COMMA , null
        \\SEMICOLON ; null
        \\SEMICOLON ; null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "prints unexpected character error message for unknowns with line number" {
    const file_contents =
        \\():
        \\#
        \\&
        \\$
        \\@
        \\{}
        \\
    ;
    const expected_output =
        \\[line 1] Error: Unexpected character: :
        \\[line 2] Error: Unexpected character: #
        \\[line 3] Error: Unexpected character: &
        \\[line 4] Error: Unexpected character: $
        \\[line 5] Error: Unexpected character: @
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "ignores tabs and spaces" {
    const file_contents = "\t  \t {}";
    const expected_output =
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can match = ==" {
    const file_contents = "\t= == ===";
    const expected_output =
        \\EQUAL = null
        \\EQUAL_EQUAL == null
        \\EQUAL_EQUAL == null
        \\EQUAL = null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can match ! !=" {
    const file_contents = "\t! != !!==";
    const expected_output =
        \\BANG ! null
        \\BANG_EQUAL != null
        \\BANG ! null
        \\BANG_EQUAL != null
        \\EQUAL = null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can match < <=" {
    const file_contents = "\t<  <=  < = <= ";
    const expected_output =
        \\LESS < null
        \\LESS_EQUAL <= null
        \\LESS < null
        \\EQUAL = null
        \\LESS_EQUAL <= null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can match > >=" {
    const file_contents = "\t>  >=  > = >= ";
    const expected_output =
        \\GREATER > null
        \\GREATER_EQUAL >= null
        \\GREATER > null
        \\EQUAL = null
        \\GREATER_EQUAL >= null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can ignore comments with // and parse single /" {
    const file_contents =
        \\@ // {} this parens should not be parsed
        \\{} / ()
    ;
    const expected_output =
        \\[line 1] Error: Unexpected character: @
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\SLASH / null
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can scan for strings" {
    const file_contents =
        \\"abc" ""
        \\"123abc"
        \\"
    ;
    const expected_output =
        \\[line 3] Error: Unterminated string.
        \\STRING "abc" abc
        \\STRING "" 
        \\STRING "123abc" 123abc
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can scan for numbers" {
    const file_contents =
        \\123 0 1
        \\123.4
        \\1234.1234
        \\1.
        \\
    ;

    const expected_output =
        \\NUMBER 123 123.0
        \\NUMBER 0 0.0
        \\NUMBER 1 1.0
        \\NUMBER 123.4 123.4
        \\NUMBER 1234.1234 1234.1234
        \\NUMBER 1 1.0
        \\DOT . null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(expected_output, file_contents);
}

test "can scan for identifier" {
    const file_contents =
        \\foo bar _hello
        \\var foo = 10;
    ;
    const expected_output =
        \\IDENTIFIER foo null
        \\IDENTIFIER bar null
        \\IDENTIFIER _hello null
        \\VAR var null
        \\IDENTIFIER foo null
        \\EQUAL = null
        \\NUMBER 10 10.0
        \\SEMICOLON ; null
        \\EOF  null
        \\
    ;

    try compareOutputWithTokensOutput(
        expected_output,
        file_contents,
    );
}
