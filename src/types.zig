const std = @import("std");

pub const TokenType = enum {
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

pub const Literal = union(enum) {
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

pub const BinaryExpression = struct {
    left: *Expr,
    right: *Expr,
    operator: Token,
};

pub const UnaryExpression = struct {
    right: *Expr,
    operator: Token,
};

pub const GroupingExpression = struct {
    expression: *Expr,
};

pub const LiteralExpression = union(enum) {
    bool: bool,
    nil: void,
    literal: Literal,

    pub fn format(this: LiteralExpression, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .bool => |value| try std.fmt.format(writer, "{}", .{value}),
            .nil => try std.fmt.format(writer, "{s}", .{"nil"}),
            .literal => |value| try std.fmt.format(writer, "{}", .{value}),
        }
    }
};

pub const Expr = union(enum) {
    Binary: BinaryExpression,
    Unary: UnaryExpression,
    Group: GroupingExpression,
    Literal: LiteralExpression,

    pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .Literal => |value| {
                try std.fmt.format(writer, "{}", .{value});
            },
            .Binary => |binary| {
                try std.fmt.format(writer, "({s} {} {})", .{
                    binary.operator.lexeme,
                    binary.left,
                    binary.right,
                });
            },
            .Unary => |unary| {
                try std.fmt.format(writer, "({s} {})", .{
                    unary.operator.lexeme,
                    unary.right,
                });
            },
            .Group => |group| {
                try std.fmt.format(writer, "(group {})", .{group.expression});
            },
        }
    }
};

pub const Stmt = union(enum) {
    Expression: *Expr,
    Print: *Expr,

    pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .Expression => |value| {
                try std.fmt.format(writer, "{}", .{value});
            },
            .Print => |value| {
                try std.fmt.format(writer, "{}", .{value});
            },
        }
    }
};
