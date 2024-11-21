const std = @import("std");
const Types = @import("types.zig");

const Token = Types.Token;
const Literal = Types.Literal;
const TokenType = Types.TokenType;

const Expr = union(enum) {
    pub const PrimaryExpr = union(enum) {
        bool: bool,
        nil: void,
        literal: Literal,
        group: *Expr,

        pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (this) {
                .bool => {
                    try std.fmt.format(writer, "{}", .{this.bool});
                },
                .nil => {
                    try std.fmt.format(writer, "{s}", .{"nil"});
                },
                .literal => {
                    try std.fmt.format(writer, "{}", .{this.literal});
                },
                .group => {
                    try std.fmt.format(writer, "(group {})", .{this.group.*});
                },
            }
        }
    };

    Primary: PrimaryExpr,

    pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .Primary => {
                try std.fmt.format(writer, "{}", .{this.Primary});
            },
        }
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const Token,
    current: usize = 0,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
        };
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == .EOF;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }

    fn newExpr(self: *Parser, value: anytype) !*Expr {
        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = value;
        return expr;
    }

    fn match(self: *Parser, token_types: anytype) bool {
        return inline for (std.meta.fields(@TypeOf(token_types))) |field| {
            const value = @field(token_types, field.name);
            if (self.check(value)) {
                _ = self.advance();
                return true;
            }
        } else false;
    }

    fn consume(self: *Parser, token_type: TokenType) Token {
        if (self.check(token_type)) return self.advance();

        @panic("handle error");
    }

    fn primary(self: *Parser) !*Expr {
        if (self.match(.{.FALSE})) return self.newExpr(.{ .Primary = .{ .bool = false } });
        if (self.match(.{.TRUE})) return self.newExpr(.{ .Primary = .{ .bool = true } });
        if (self.match(.{.NIL})) return self.newExpr(.{ .Primary = .{ .nil = {} } });

        if (self.match(.{ .NUMBER, .STRING })) return self.newExpr(
            .{
                .Primary = .{ .literal = self.previous().literal.? },
            },
        );

        if (self.match(.{.LEFT_PAREN})) {
            const expr = try self.primary();
            _ = self.consume(.RIGHT_PAREN);
            return try self.newExpr(.{ .Primary = .{ .group = expr } });
        }

        unreachable;
    }

    pub fn parse(self: *Self) !*Expr {
        return self.primary();
    }

    // pub fn parse(self: *Self) void {
    //     for (self.tokens) |token| {
    //         switch (token.type) {
    //             TokenType.FALSE => {},
    //             TokenType.NIL =>
    //         }
    //     }
    // }
};

test "does nothing" {
    const tokens = &.{
        Token.new(1, .FALSE, "false", null),
        Token.new(2, .NUMBER, "42.0", .{ .num = 42.0 }),
        Token.new(3, .STRING, "hello", .{ .str = "hello" }),
        Token.new(3, .LEFT_PAREN, "(", null),
        Token.new(3, .STRING, "hello", .{ .str = "hello" }),
        Token.new(3, .RIGHT_PAREN, ")", null),
        Token.new(3, .LEFT_PAREN, "(", null),
        Token.new(3, .NUMBER, "42.0", .{ .num = 42.0 }),
        Token.new(3, .RIGHT_PAREN, ")", null),
        Token.new(3, .LEFT_PAREN, "(", null),
        Token.new(3, .NIL, "nil", null),
        Token.new(3, .RIGHT_PAREN, ")", null),
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var parser = Parser.init(arena.allocator(), tokens);
    var expr = try parser.parse();
    std.debug.print("{}\n", .{expr});
    expr = try parser.parse();
    std.debug.print("{}\n", .{expr});
    expr = try parser.parse();
    std.debug.print("{}\n", .{expr});

    expr = try parser.parse();
    std.debug.print("{}\n", .{expr});

    expr = try parser.parse();
    std.debug.print("{}\n", .{expr});

    expr = try parser.parse();
    std.debug.print("{}\n", .{expr});
}
