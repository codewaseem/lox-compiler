const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const types = @import("types.zig");

const TokenType = types.TokenType;
const Token = types.Token;
const LiteralToken = types.Literal;

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
    literal: LiteralToken,
};

pub const Expr = union(enum) {
    Binary: BinaryExpression,
    Unary: UnaryExpression,
    Group: GroupingExpression,
    Literal: LiteralExpression,

    pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .Literal => |value| {
                switch (value) {
                    .bool => {
                        try std.fmt.format(writer, "{}", .{value.bool});
                    },
                    .nil => {
                        try std.fmt.format(writer, "{s}", .{"nil"});
                    },
                    .literal => {
                        try std.fmt.format(writer, "{}", .{value.literal});
                    },
                }
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

const Error = error{
    OutOfMemory,
    ExpectedExpression,
    ExpectedRightParen,
};

const ParserError = struct {
    token: Token,
    error_type: Error,

    pub fn new(
        error_type: Error,
        token: Token,
    ) ParserError {
        return .{
            .error_type = error_type,
            .token = token,
        };
    }

    pub fn format(value: ParserError, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (value.error_type) {
            Error.ExpectedRightParen => {
                try std.fmt.format(
                    writer,
                    "[line {d}] Error: at '{s}': Expected ')' after expression.\n",
                    .{ value.token.line, value.token.lexeme },
                );
            },
            Error.ExpectedExpression => {
                try std.fmt.format(
                    writer,
                    "[line {d}] Error: at '{s}': Expected expression.\n",
                    .{
                        value.token.line,
                        value.token.lexeme,
                    },
                );
            },
            Error.OutOfMemory => {
                try std.fmt.format(writer, "Error: OutOfMemory\n", .{});
            },
        }
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const Token,
    current: usize = 0,
    errors: std.ArrayList(ParserError),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Parser {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .errors = std.ArrayList(ParserError).init(allocator),
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

    fn newExpr(self: *Self, value: anytype) Error!*Expr {
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

    fn consume(self: *Self, token_type: TokenType, error_type: Error) Token {
        if (self.check(token_type)) return self.advance();

        const error_token = self.peek();
        switch (error_type) {
            Error.ExpectedRightParen => {
                self.errors.append(ParserError.new(
                    Error.ExpectedRightParen,
                    error_token,
                )) catch |err| {
                    std.debug.print("Unexpected Error: {}\n", .{err});
                };
            },
            else => {
                @panic("Unhandled error case\n");
            },
        }
        return error_token;
    }

    fn expression(self: *Self) Error!*Expr {
        return self.equality();
    }

    fn equality(self: *Self) Error!*Expr {
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

    fn comparision(self: *Self) Error!*Expr {
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

    fn term(self: *Self) Error!*Expr {
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

    fn factor(self: *Self) Error!*Expr {
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

    fn unary(self: *Self) Error!*Expr {
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

    fn primary(self: *Self) Error!*Expr {
        // std.debug.print("primary {}\n", .{self.peek()});
        if (self.match(.{.FALSE})) return self.newExpr(.{ .Literal = .{ .bool = false } });
        if (self.match(.{.TRUE})) return self.newExpr(.{ .Literal = .{ .bool = true } });
        if (self.match(.{.NIL})) return self.newExpr(.{ .Literal = .{ .nil = {} } });

        if (self.match(.{ .NUMBER, .STRING })) {
            return self.newExpr(
                .{
                    .Literal = .{ .literal = self.previous().literal.? },
                },
            );
        }

        if (self.match(.{.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = self.consume(.RIGHT_PAREN, Error.ExpectedRightParen);
            return self.newExpr(.{ .Group = .{ .expression = expr } });
        }

        try self.errors.append(ParserError.new(Error.ExpectedExpression, self.peek()));
        return Error.ExpectedExpression;
    }

    pub fn parse(self: *Self) !?*Expr {
        return self.expression() catch |err| {
            switch (err) {
                Error.ExpectedExpression => return null,
                Error.ExpectedRightParen => return null,
                else => return err,
            }
        };
    }
};
