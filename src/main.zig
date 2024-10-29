const std = @import("std");

const MyErrors = error{TokenNotFound};

const TokenType = enum {
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

const stdOutWriter = std.io.getStdOut().writer();

const Token = struct {
    type: TokenType = undefined,
    lexeme: []const u8,
    literal: ?[]u8,

    fn print(self: *Token) !void {
        try stdOutWriter.print("{s} {s} {?s}\n", .{ @tagName(self.type), self.lexeme, self.literal });
    }
};

const EOFToken = Token{ .lexeme = "", .type = .EOF, .literal = null };

const LPARENToken = Token{ .lexeme = "(", .literal = null, .type = .LEFT_PAREN };

const RPARENToken = Token{ .lexeme = ")", .literal = null, .type = .RIGHT_PAREN };

const LBRACEToken = Token{ .lexeme = "{", .literal = null, .type = .LEFT_BRACE };

const RBRACEToken = Token{ .lexeme = "}", .literal = null, .type = .RIGHT_BRACE };

pub fn main() !void {
    // You can use print statements as follows for debugging, they'll be visible when running tests.

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (!std.mem.eql(u8, command, "tokenize")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    var tokens = std.ArrayList(Token).init(std.heap.page_allocator);
    defer tokens.deinit();

    // Uncomment this block to pass the first stage
    for (file_contents) |c| {
        switch (c) {
            '(' => {
                try tokens.append(LPARENToken);
            },
            ')' => {
                try tokens.append(RPARENToken);
            },
            '{' => {
                try tokens.append(LBRACEToken);
            },
            '}' => {
                try tokens.append(RBRACEToken);
            },
            else => continue,
        }
    }

    try tokens.append(EOFToken);

    for (tokens.items) |*token| {
        try token.print();
    }
}
