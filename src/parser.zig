const std = @import("std");
const Types = @import("types.zig");

const Token = Types.Token;
const StringNumberLiteral = Types.StringNumberLiteral;
const TokenType = Types.TokenType;

const PrimaryExpr = union(enum) {
    BOOL: bool,
    NIL: void,
    STRING_OR_NUMBER: StringNumberLiteral,

    pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .BOOL => {
                try std.fmt.format(writer, "{}", .{this.BOOL});
            },
            .NIL => {
                try std.fmt.format(writer, "{s}", .{"nil"});
            },
            .STRING_OR_NUMBER => {
                try std.fmt.format(writer, "{}", .{this.STRING_OR_NUMBER});
            },
        }
    }

    // pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    //     switch (value) {
    //         .False => {},
    //     }

    //     try std.fmt.format(writer, "{}", .{
    //         value.value,
    //     });
    // }
};

const Expr = union(enum) {
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
        inline for (std.meta.fields(@TypeOf(token_types))) |field| {
            const value = @field(token_types, field.name);
            if (self.check(value)) {
                _ = self.advance();
                return true;
            }
            return false;
        }
    }

    fn primary(self: *Parser) !*Expr {
        if (self.match(.{.FALSE})) return self.newExpr(.{ .Primary = .{ .BOOL = false } });
        if (self.match(.{.TRUE})) return self.newExpr(.{ .Primary = .{ .BOOL = true } });
        if (self.match(.{.NIL})) return self.newExpr(.{ .Primary = .{ .NIL = {} } });

        if (self.match(.{ .NUMBER, .STRING })) return self.newExpr(
            .{
                .Primary = .{ .STRING_OR_NUMBER = self.previous().literal.? },
            },
        );

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
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var parser = Parser.init(arena.allocator(), tokens);
    const expression = try parser.parse();

    std.debug.print("{}\n", .{expression});
}
