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
            }
        }
    };

    const BinaryExpr = struct {
        left: *Expr,
        right: *Expr,
        operator: Token,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try std.fmt.format(writer, "({s} {} {})", .{
                value.operator.lexeme,
                value.left,
                value.right,
            });
        }
    };

    const UnaryExpr = struct {
        right: *Expr,
        operator: Token,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try std.fmt.format(writer, "({s} {})", .{
                value.operator.lexeme,
                value.right,
            });
        }
    };

    const GroupingExpr = struct {
        expression: *Expr,

        pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try std.fmt.format(writer, "(group {})", .{this.expression});
        }
    };

    Primary: PrimaryExpr,
    Binary: BinaryExpr,
    Unary: UnaryExpr,
    Group: GroupingExpr,

    pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .Primary => {
                try std.fmt.format(writer, "{}", .{this.Primary});
            },
            .Binary => {
                try std.fmt.format(writer, "{}", .{this.Binary});
            },
            .Unary => {
                try std.fmt.format(writer, "{}", .{this.Unary});
            },
            .Group => {
                try std.fmt.format(writer, "{}", .{this.Group});
            },
        }
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const Token,
    current: usize = 0,

    const ParserError = error{OutOfMemory};

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
        };
    }

    fn peek(self: *Self) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens[self.current - 1];
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().type == .EOF;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn check(self: *Self, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }

    fn newExpr(self: *Self, value: anytype) ParserError!*Expr {
        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = value;
        return expr;
    }

    fn match(self: *Self, token_types: anytype) bool {
        return inline for (std.meta.fields(@TypeOf(token_types))) |field| {
            const value = @field(token_types, field.name);
            if (self.check(value)) {
                _ = self.advance();
                return true;
            }
        } else false;
    }

    fn consume(self: *Self, token_type: TokenType) Token {
        if (self.check(token_type)) return self.advance();

        @panic("handle error");
    }

    fn expression(self: *Self) ParserError!*Expr {
        return self.equality();
    }

    fn equality(self: *Self) ParserError!*Expr {
        var expr = try self.comparision();

        while (self.match(.{
            .BANG_EQUAL,
            .EQUAL_EQUAL,
        })) {
            const op = self.previous();
            const right = try self.comparision();
            expr = try self.newExpr(
                .{ .Binary = .{
                    .left = expr,
                    .operator = op,
                    .right = right,
                } },
            );
        }

        return expr;
    }

    fn comparision(self: *Self) ParserError!*Expr {
        var expr = try self.term();

        while (self.match(.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const op = self.previous();
            const right = try self.term();
            expr = try self.newExpr(.{ .Binary = .{
                .left = expr,
                .operator = op,
                .right = right,
            } });
        }

        return expr;
    }

    fn term(self: *Self) ParserError!*Expr {
        var expr = try self.factor();

        while (self.match(.{
            .MINUS,
            .PLUS,
        })) {
            const op = self.previous();
            const right = try self.factor();
            expr = try self.newExpr(.{ .Binary = .{
                .left = expr,
                .operator = op,
                .right = right,
            } });
        }

        return expr;
    }

    fn factor(self: *Self) ParserError!*Expr {
        // std.debug.print("factor {}\n", .{self.peek()});
        var expr = try self.unary();

        while (self.match(.{
            .SLASH,
            .STAR,
        })) {
            const op = self.previous();
            const right = try self.unary();
            expr = try self.newExpr(.{ .Binary = .{
                .left = expr,
                .operator = op,
                .right = right,
            } });
        }

        return expr;
    }

    fn unary(self: *Self) ParserError!*Expr {
        // std.debug.print("unary {}\n", .{self.peek()});

        if (self.match(.{ .BANG, .MINUS })) {
            const op = self.previous();
            const expr = try self.unary();
            return self.newExpr(.{ .Unary = .{
                .operator = op,
                .right = expr,
            } });
        }

        return try self.primary();
    }

    fn primary(self: *Self) ParserError!*Expr {
        // std.debug.print("primary {}\n", .{self.peek()});
        if (self.match(.{.FALSE})) return self.newExpr(.{ .Primary = .{ .bool = false } });
        if (self.match(.{.TRUE})) return self.newExpr(.{ .Primary = .{ .bool = true } });
        if (self.match(.{.NIL})) return self.newExpr(.{ .Primary = .{ .nil = {} } });

        if (self.match(.{ .NUMBER, .STRING })) return self.newExpr(
            .{
                .Primary = .{ .literal = self.previous().literal.? },
            },
        );

        if (self.match(.{.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = self.consume(.RIGHT_PAREN);
            return self.newExpr(.{ .Group = .{ .expression = expr } });
        }

        unreachable;
    }

    pub fn parse(self: *Self) !*Expr {
        return self.expression();
    }
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
}
