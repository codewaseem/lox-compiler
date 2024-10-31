const std = @import("std");

const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,

    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
};

const Literal = union(enum) {
    str: []const u8,
    num: f64,
    pub fn format(value: Literal, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (value) {
            .str => |str| try std.fmt.format(writer, "{s}", .{str}),
            .num => |num| {
                var buf: [256]u8 = undefined;
                const str = try std.fmt.bufPrint(&buf, "{d}", .{num});
                try writer.writeAll(str);
                if (std.mem.indexOfScalar(u8, str, '.') == null) {
                    try writer.writeAll(".0");
                }
            },
        }
    }
};

pub const ScanError = error{ UnexpectedCharacter, UnterminatedString };

pub const Token = struct {
    type: TokenType = undefined,
    lexeme: []const u8 = undefined,
    line: usize,
    literal: ?Literal,

    const self = @This();

    pub fn new(line: usize, token_type: TokenType, lexeme: []const u8, literal: ?Literal) Token {
        return .{
            .type = token_type,
            .lexeme = lexeme,
            .line = line,
            .literal = literal,
        };
    }

    pub fn format(value: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(
            writer,
            "{s} {s} {?}\n",
            .{ @tagName(value.type), value.lexeme, value.literal },
        );
    }
};

pub const Error = struct {
    type: ScanError,
    line: usize,
    source: []const u8,

    pub fn new(error_type: ScanError, line: usize, source: []const u8) Error {
        return .{
            .type = error_type,
            .line = line,
            .source = source,
        };
    }

    pub fn format(value: Error, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (value.type) {
            ScanError.UnexpectedCharacter => {
                try std.fmt.format(
                    writer,
                    "[line {d}] Error: Unexpected character: {s}\n",
                    .{ value.line, value.source },
                );
            },

            ScanError.UnterminatedString => {
                try std.fmt.format(
                    writer,
                    "[line {d}] Error: Unterminated string.\n",
                    .{value.line},
                );
            },
        }
    }
};

pub const Scanner = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),
    errors: std.ArrayList(Error),
    start: usize = 0,
    currentLine: usize = 1,
    currentPos: usize = 0,

    pub fn new(source: []const u8, allocator: std.mem.Allocator) Scanner {
        return .{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
            .errors = std.ArrayList(Error).init(allocator),
        };
    }

    pub fn deinit(self: *Scanner) void {
        self.tokens.deinit();
        self.errors.deinit();
    }

    fn readChar(self: *Scanner) u8 {
        return self.source[self.currentPos];
    }

    fn endOfFile(self: *Scanner) bool {
        return self.currentPos >= self.source.len;
    }

    fn jumpToNextChar(self: *Scanner) void {
        self.currentPos += 1;
    }

    fn addNullLiteralToken(
        self: *Scanner,
        token_type: TokenType,
    ) !void {
        try self.addTokenValue(token_type, null);
    }

    fn addTokenValue(self: *Scanner, token_type: TokenType, literal: ?Literal) !void {
        try self.tokens.append(Token.new(
            self.currentLine,
            token_type,
            self.source[self.start .. self.currentPos + 1],
            literal,
        ));
    }

    fn addUnexpectedCharacterError(self: *Scanner) !void {
        try self.errors.append(Error.new(
            ScanError.UnexpectedCharacter,
            self.currentLine,
            self.source[self.start .. self.start + 1],
        ));
    }

    fn getNextChar(self: *Scanner) u8 {
        return if (self.canJumpToNextChar()) self.source[self.currentPos + 1] else 0;
    }

    fn matchNextAndJump(self: *Scanner, char: u8) bool {
        if (self.canJumpToNextChar() and self.source[self.currentPos + 1] == char) {
            self.currentPos += 1;
            return true;
        } else {
            return false;
        }
    }

    fn canJumpToNextChar(self: *Scanner) bool {
        return self.currentPos + 1 < self.source.len;
    }

    fn jumpedToEndOfQoutes(self: *Scanner) bool {
        return while (self.canJumpToNextChar()) {
            self.jumpToNextChar();
            if (self.readChar() == '"') break true;
        } else false;
    }

    fn jumpToEndOfLine(self: *Scanner) void {
        while (self.canJumpToNextChar() and self.source[self.currentPos + 1] != '\n') {
            self.currentPos += 1;
        }
    }

    fn scanNumber(self: *Scanner) !void {
        while (self.canJumpToNextChar() and std.ascii.isDigit(self.getNextChar())) {
            self.jumpToNextChar();
        }

        // possible floating number?
        if (self.getNextChar() == '.') {
            self.jumpToNextChar();
            if (std.ascii.isDigit(self.getNextChar())) {
                self.jumpToNextChar();
                while (self.canJumpToNextChar() and std.ascii.isDigit(self.getNextChar())) {
                    self.jumpToNextChar();
                }
            } else {
                // take a step back to ignore . char read
                // as the current number is not a floating number
                self.currentPos -= 1;
            }
        }

        // at this point we have moved over one char from number
        // so take a step back so that addTokenValue slices text correctly
        try self.addTokenValue(.NUMBER, .{ .num = try std.fmt.parseFloat(f64, self.source[self.start .. self.currentPos + 1]) });
    }

    fn scanString(self: *Scanner) !void {
        if (self.jumpedToEndOfQoutes() == false) {
            try self.errors.append(Error.new(
                ScanError.UnterminatedString,
                self.currentLine,
                self.source[self.start .. self.start + 1],
            ));
        } else {
            const str = self.source[self.start + 1 .. self.currentPos];
            try self.addTokenValue(.STRING, .{ .str = str });
        }
    }

    pub fn scanTokens(self: *Scanner) ![]const Token {
        while (!self.endOfFile()) : (self.jumpToNextChar()) {
            self.start = self.currentPos;
            const c = self.readChar();
            switch (c) {
                '\t', ' ' => continue,
                '\n' => {
                    self.currentLine += 1;
                },
                '0'...'9' => try self.scanNumber(),
                '"' => try self.scanString(),
                '=' => try if (self.matchNextAndJump('=')) self.addNullLiteralToken(.EQUAL_EQUAL) else self.addNullLiteralToken(.EQUAL),
                '!' => try if (self.matchNextAndJump('=')) self.addNullLiteralToken(.BANG_EQUAL) else self.addNullLiteralToken(.BANG),
                '<' => try if (self.matchNextAndJump('=')) self.addNullLiteralToken(.LESS_EQUAL) else self.addNullLiteralToken(.LESS),
                '>' => try if (self.matchNextAndJump('=')) self.addNullLiteralToken(.GREATER_EQUAL) else self.addNullLiteralToken(.GREATER),
                '/' => try if (self.matchNextAndJump('/')) {
                    self.jumpToEndOfLine();
                } else self.addNullLiteralToken(.SLASH),
                '(' => try self.addNullLiteralToken(.LEFT_PAREN),
                ')' => try self.addNullLiteralToken(.RIGHT_PAREN),
                '{' => try self.addNullLiteralToken(.LEFT_BRACE),
                '}' => try self.addNullLiteralToken(.RIGHT_BRACE),
                ',' => try self.addNullLiteralToken(.COMMA),
                '.' => try self.addNullLiteralToken(.DOT),
                '-' => try self.addNullLiteralToken(.MINUS),
                '+' => try self.addNullLiteralToken(.PLUS),
                ';' => try self.addNullLiteralToken(.SEMICOLON),
                '*' => try self.addNullLiteralToken(.STAR),
                else => try self.addUnexpectedCharacterError(),
            }
        }

        try self.tokens.append(Token.new(self.currentLine, .EOF, "", null));
        return self.tokens.items;
    }

    pub fn getErrors(self: *Scanner) ![]const Error {
        return self.errors.items;
    }
};

fn compareOutputWithTokensOutput(expected_output: []const u8, file_contents: []const u8) !void {
    var runtime_start: usize = 0;
    _ = &runtime_start;

    var scanner = Scanner.new(file_contents[runtime_start..], std.testing.allocator);
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
