const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const types = @import("types.zig");

const TokenType = types.TokenType;
const Token = types.Token;
const Expr = types.Expr;
const BinaryExpression = types.BinaryExpression;
const UnaryExpression = types.UnaryExpression;
const GroupingExpression = types.GroupingExpression;
const LiteralExpression = types.LiteralExpression;
const Value = types.Value;
const Stmt = types.Stmt;

const Error = error{
    OutOfMemory,
    ExpectedExpression,
    ExpectedRightParen,
    ExpectedSemicolon,
    ExpectedVariableName,
    InvalidAssignmentTarget,
    ExpectedRightBrace,
    ExpectedLeftParenAfterIf,
    ExpectedRightParenAfterIf,
};

pub const ParserError = struct {
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
            Error.ExpectedSemicolon => {
                try std.fmt.format(
                    writer,
                    "[line {d}] Error: at '{s}': Expected ';' after expression.\n",
                    .{
                        value.token.line,
                        value.token.lexeme,
                    },
                );
            },
            Error.ExpectedVariableName => {
                try std.fmt.format(
                    writer,
                    "[line {d}] Error: Expected variable name.",
                    .{value.token.line},
                );
            },
            Error.InvalidAssignmentTarget => {
                try std.fmt.format(
                    writer,
                    "[line {d}] Error: Invalid assignment target.",
                    .{value.token.line},
                );
            },
            Error.ExpectedRightBrace => {
                try std.fmt.format(
                    writer,
                    "[line {d}] Error at end: Expect '{s}'.\n",
                    .{ value.token.line - 1, "}" },
                );
            },
            Error.ExpectedLeftParenAfterIf => {
                try std.fmt.format(writer, "Expect '(' after 'if'.\n", .{});
            },
            Error.ExpectedRightParenAfterIf => {
                try std.fmt.format(writer, "Expect ')' after if condition.", .{});
            },
        }
    }
};

pub const TokenConsumer = struct {
    tokens: []const Token,
    errors: std.ArrayList(ParserError),
    current: usize = 0,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) TokenConsumer {
        return .{
            .tokens = tokens,
            .errors = std.ArrayList(ParserError).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
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

    fn match(self: *Self, token_types: anytype) bool {
        return inline for (std.meta.fields(@TypeOf(token_types))) |field| {
            const value = @field(token_types, field.name);
            if (self.check(value)) {
                _ = self.advance();
                return true;
            }
        } else false;
    }

    fn consume(self: *Self, token_type: TokenType, error_type: Error) !Token {
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
            Error.ExpectedSemicolon => {
                try self.errors.append(ParserError.new(Error.ExpectedSemicolon, error_token));
            },
            Error.ExpectedRightBrace => {
                try self.errors.append(ParserError.new(Error.ExpectedRightBrace, error_token));
            },
            else => {
                @panic("Unhandled error case\n");
            },
        }
        return error_type;
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    token_consumer: TokenConsumer,
    statements: std.ArrayList(Stmt),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Parser {
        return .{
            .allocator = allocator,
            .token_consumer = TokenConsumer.init(allocator, tokens),
            .statements = std.ArrayList(Stmt).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.token_consumer.deinit();
        self.statements.deinit();
    }

    fn newExpr(self: *Self, value: anytype) Error!*Expr {
        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = value;
        return expr;
    }

    fn expression(self: *Self) Error!*Expr {
        return self.assignment();
    }

    fn orExpr(self: *Self) Error!*Expr {
        var expr = try self.andExpr();

        while (self.token_consumer.match(.{.OR})) {
            const operator = self.token_consumer.previous();
            const right = try self.andExpr();
            expr = try self.newExpr(Expr{ .Logical = .{
                .left = expr,
                .operator = operator,
                .right = right,
            } });
        }

        return expr;
    }

    fn andExpr(self: *Self) Error!*Expr {
        var expr = try self.equality();

        while (self.token_consumer.match(.{.AND})) {
            const operator = self.token_consumer.previous();
            const right = try self.equality();
            expr = try self.newExpr(Expr{ .Logical = .{
                .left = expr,
                .operator = operator,
                .right = right,
            } });
        }

        return expr;
    }

    fn assignment(self: *Self) Error!*Expr {
        const expr = try self.orExpr();

        if (self.token_consumer.match(.{.EQUAL})) {
            const equals = self.token_consumer.previous();
            const value = try self.assignment();

            switch (expr.*) {
                .Var => |v| {
                    return self.newExpr(.{ .Assign = .{
                        .name = v,
                        .value = value,
                    } });
                },
                else => {
                    try self.token_consumer.errors.append(
                        ParserError.new(Error.InvalidAssignmentTarget, equals),
                    );
                    return Error.InvalidAssignmentTarget;
                },
            }
        }

        return expr;
    }

    fn equality(self: *Self) Error!*Expr {
        var expr = try self.comparision();

        while (self.token_consumer.match(.{
            .BANG_EQUAL,
            .EQUAL_EQUAL,
        })) {
            const op = self.token_consumer.previous();
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

        while (self.token_consumer.match(.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const op = self.token_consumer.previous();
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

        while (self.token_consumer.match(.{
            .MINUS,
            .PLUS,
        })) {
            const op = self.token_consumer.previous();
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

        while (self.token_consumer.match(.{
            .SLASH,
            .STAR,
        })) {
            const op = self.token_consumer.previous();
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

        if (self.token_consumer.match(.{ .BANG, .MINUS })) {
            const op = self.token_consumer.previous();
            const expr = try self.unary();
            return self.newExpr(.{ .Unary = .{
                .operator = op,
                .right = expr,
            } });
        }

        return try self.primary();
    }

    fn primary(self: *Self) Error!*Expr {
        if (self.token_consumer.match(.{.FALSE})) return self.newExpr(.{ .Literal = .{ .bool = false } });
        if (self.token_consumer.match(.{.TRUE})) return self.newExpr(.{ .Literal = .{ .bool = true } });
        if (self.token_consumer.match(.{.NIL})) return self.newExpr(.{ .Literal = .{ .nil = {} } });

        if (self.token_consumer.match(.{ .NUMBER, .STRING })) {
            return self.newExpr(
                .{
                    .Literal = .{ .literal = self.token_consumer.previous().literal.? },
                },
            );
        }

        if (self.token_consumer.match(.{.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = try self.token_consumer.consume(.RIGHT_PAREN, Error.ExpectedRightParen);
            return self.newExpr(.{ .Group = .{ .expression = expr } });
        }

        if (self.token_consumer.match(.{.IDENTIFIER})) {
            return self.newExpr(.{ .Var = self.token_consumer.previous() });
        }

        try self.token_consumer.errors.append(
            ParserError.new(Error.ExpectedExpression, self.token_consumer.peek()),
        );

        return Error.ExpectedExpression;
    }

    pub fn parseExpression(self: *Self) !?*Expr {
        return self.assignment() catch |err| {
            switch (err) {
                Error.ExpectedExpression => return null,
                Error.ExpectedRightParen => return null,
                else => return err,
            }
        };
    }

    pub fn statement(self: *Self) Error!Stmt {
        if (self.token_consumer.match(.{.IF})) {
            return self.ifStatement();
        }

        if (self.token_consumer.match(.{.PRINT})) {
            return self.printStatement();
        }

        if (self.token_consumer.match(.{.LEFT_BRACE})) {
            return self.blockStatement();
        }

        return self.expressionStatement();
    }

    pub fn ifStatement(self: *Self) Error!Stmt {
        _ = try self.token_consumer.consume(.LEFT_PAREN, Error.ExpectedLeftParenAfterIf);
        const condition = try self.expression();
        _ = try self.token_consumer.consume(.RIGHT_PAREN, Error.ExpectedLeftParenAfterIf);

        const then_stmt = try self.statement();
        var else_stmt: ?Stmt = null;

        if (self.token_consumer.match(.{.ELSE})) {
            else_stmt = try self.statement();
        }

        return Stmt{
            .If = .{
                .condition = condition,
                .then_branch = try Stmt.new(self.allocator, then_stmt),
                .else_branch = if (else_stmt) |v| try Stmt.new(self.allocator, v) else null,
            },
        };
    }

    pub fn declaration(self: *Self) Error!Stmt {
        if (self.token_consumer.match(.{.VAR})) {
            return self.varDeclaration();
        }
        return self.statement();
    }

    pub fn varDeclaration(self: *Self) Error!Stmt {
        const name_token = try self.token_consumer.consume(.IDENTIFIER, Error.ExpectedVariableName);

        var expr: *Expr = try self.newExpr(.{ .Literal = .{ .nil = {} } });

        if (self.token_consumer.match(.{.EQUAL})) {
            expr = try self.expression();
        }

        _ = try self.token_consumer.consume(.SEMICOLON, Error.ExpectedSemicolon);

        return Stmt{ .Var = .{ .name = name_token, .initializer = expr } };
    }

    pub fn printStatement(self: *Self) Error!Stmt {
        const value = try self.expression();
        _ = try self.token_consumer.consume(.SEMICOLON, Error.ExpectedSemicolon);
        return Stmt{ .Print = value };
    }

    pub fn blockStatement(self: *Self) Error!Stmt {
        var statements = std.ArrayList(Stmt).init(self.allocator);

        while (!self.token_consumer.check(.RIGHT_BRACE) and !self.token_consumer.isAtEnd()) {
            try statements.append(try self.declaration());
        }

        _ = try self.token_consumer.consume(.RIGHT_BRACE, Error.ExpectedRightBrace);
        return Stmt{ .Block = statements.items };
    }

    pub fn expressionStatement(self: *Self) Error!Stmt {
        const expr = try self.expression();
        _ = try self.token_consumer.consume(.SEMICOLON, Error.ExpectedSemicolon);
        return Stmt{ .Expression = expr };
    }

    pub fn parse(self: *Self) ![]Stmt {
        while (!self.token_consumer.isAtEnd()) {
            try self.statements.append(try self.declaration());
        }

        return self.statements.items;
    }
};
