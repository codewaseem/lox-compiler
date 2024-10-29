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

    fn init(tokenType: TokenType, lexeme: []const u8, literal: ?[]u8) Token {
        return Token{ .type = tokenType, .lexeme = lexeme, .literal = literal };
    }

    fn print(self: *Token) !void {
        try stdOutWriter.print("{s} {s} {?s}\n", .{ @tagName(self.type), self.lexeme, self.literal });
    }
};

const EOFToken = Token.init(.EOF, "", null);
const LPARENToken = Token.init(.LEFT_PAREN, "(", null);
const RPARENToken = Token.init(.RIGHT_PAREN, ")", null);
const LBRACEToken = Token.init(.LEFT_BRACE, "{", null);
const RBRACEToken = Token.init(.RIGHT_BRACE, "}", null);
const CommaToken = Token.init(.COMMA, ",", null);
const DotToken = Token.init(.DOT, ".", null);
const MinusToken = Token.init(.MINUS, "-", null);
const PlusToken = Token.init(.PLUS, "+", null);
const SemicolonToken = Token.init(.SEMICOLON, ";", null);
const StarToken = Token.init(.STAR, "*", null);
const AssignmentToken = Token.init(.EQUAL, "=", null);
const EqualsToken = Token.init(.EQUAL_EQUAL, "==", null);
const NegationToken = Token.init(.BANG, "!", null);
const InEqualityToken = Token.init(.BANG_EQUAL, "!=", null);
const LessThanToken = Token.init(.LESS, "<", null);
const GreaterThanToken = Token.init(.GREATER, ">", null);
const LessThanOrEqualToken = Token.init(.LESS_EQUAL, "<=", null);
const GreaterThanOrEqualToken = Token.init(.GREATER_EQUAL, ">=", null);
const SlashToken = Token.init(.SLASH, "/", null);

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

    var currentLine: usize = 1;
    var errorCount: usize = 0;
    // Uncomment this block to pass the first stage

    var cursorPos: usize = 0;

    while (cursorPos < file_contents.len) : (cursorPos += 1) {
        const c = file_contents[cursorPos];

        switch (c) {
            '\t', ' ' => continue,
            '\n' => {
                currentLine += 1;
            },
            '=' => {
                if (cursorPos + 1 < file_contents.len and file_contents[cursorPos + 1] == '=') {
                    cursorPos += 1;
                    try tokens.append(EqualsToken);
                } else {
                    try tokens.append(AssignmentToken);
                }
            },
            '!' => {
                if (cursorPos + 1 < file_contents.len and file_contents[cursorPos + 1] == '=') {
                    cursorPos += 1;
                    try tokens.append(InEqualityToken);
                } else {
                    try tokens.append(NegationToken);
                }
            },
            '<' => {
                if (cursorPos + 1 < file_contents.len and file_contents[cursorPos + 1] == '=') {
                    cursorPos += 1;
                    try tokens.append(LessThanOrEqualToken);
                } else {
                    try tokens.append(LessThanToken);
                }
            },
            '>' => {
                if (cursorPos + 1 < file_contents.len and file_contents[cursorPos + 1] == '=') {
                    cursorPos += 1;
                    try tokens.append(GreaterThanOrEqualToken);
                } else {
                    try tokens.append(GreaterThanToken);
                }
            },
            '/' => {
                if (cursorPos + 1 < file_contents.len and file_contents[cursorPos + 1] == '/') {
                    cursorPos += 1;
                    while (cursorPos + 1 < file_contents.len and file_contents[cursorPos + 1] != '\n') {
                        cursorPos += 1;
                    }
                } else {
                    try tokens.append(SlashToken);
                }
            },
            '(' => try tokens.append(LPARENToken),
            ')' => try tokens.append(RPARENToken),
            '{' => try tokens.append(LBRACEToken),
            '}' => try tokens.append(RBRACEToken),
            ',' => try tokens.append(CommaToken),
            '.' => try tokens.append(DotToken),
            '-' => try tokens.append(MinusToken),
            '+' => try tokens.append(PlusToken),
            ';' => try tokens.append(SemicolonToken),
            '*' => try tokens.append(StarToken),
            else => {
                errorCount += 1;
                std.debug.print("[line {d}] Error: Unexpected character: {c}\n", .{ currentLine, c });
            },
        }
    }

    try tokens.append(EOFToken);

    for (tokens.items) |*token| {
        try token.print();
    }

    if (errorCount > 0) {
        std.process.exit(65);
    }
}
