const std = @import("std");
const types = @import("./types.zig");
const Expr = types.Expr;
const LiteralExpression = types.LiteralExpression;
const Literal = types.Literal;
const GroupingExpression = types.GroupingExpression;
const UnaryExpression = types.UnaryExpression;
const BinaryExpression = types.BinaryExpression;

const Value = union(enum) {
    literal: LiteralExpression,

    pub fn format(this: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .literal => |value| {
                switch (value) {
                    .literal => |literal| {
                        switch (literal) {
                            .str => |str| try std.fmt.format(writer, "{s}", .{str}),
                            .num => {
                                var buf: [256]u8 = undefined;
                                const str = try std.fmt.bufPrint(&buf, "{d}", .{literal});
                                // if the number ends with .0, remove the .0
                                if (std.mem.endsWith(u8, str, ".0")) {
                                    try writer.writeAll(str[0 .. str.len - 2]);
                                } else {
                                    try writer.writeAll(str);
                                }
                            },
                        }
                    },
                    else => {
                        try std.fmt.format(writer, "{}", .{value});
                    },
                }
            },
        }
    }
};

pub const Interpreter = struct {
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return .{
            .allocator = allocator,
        };
    }

    pub fn interpret(self: *Self, expression: *Expr) Value {
        return self.evaluate(expression);
    }

    fn evaluateLiteral(_: *Self, expression: *LiteralExpression) Value {
        return .{ .literal = expression.* };
    }

    fn evaluateGrouping(self: *Self, expression: *GroupingExpression) Value {
        return self.evaluate(expression.expression);
    }

    fn evaluateUnaryExpression(self: *Self, expression: *UnaryExpression) Value {
        const right = self.evaluate(expression.right);

        switch (expression.operator.type) {
            .MINUS => {
                return .{ .literal = .{ .literal = .{ .num = -right.literal.literal.num } } };
            },
            .BANG => {
                return .{ .literal = .{ .bool = !self.isTruthy(right) } };
            },
            else => unreachable,
        }
    }

    fn evaluateBinaryExpression(self: *Self, expression: *BinaryExpression) Value {
        const left = self.evaluate(expression.left);
        const right = self.evaluate(expression.right);

        switch (expression.operator.type) {
            .MINUS => {
                return .{ .literal = .{ .literal = .{ .num = left.literal.literal.num - right.literal.literal.num } } };
            },
            .SLASH => {
                return .{ .literal = .{ .literal = .{ .num = left.literal.literal.num / right.literal.literal.num } } };
            },
            .STAR => {
                return .{ .literal = .{ .literal = .{ .num = left.literal.literal.num * right.literal.literal.num } } };
            },
            else => unreachable,
        }
    }

    fn isTruthy(_: *Self, value: Value) bool {
        return switch (value) {
            .literal => |literal| {
                switch (literal) {
                    .bool => |v| return v,
                    .nil => return false,
                    else => return true,
                }
            },
        };
    }

    fn evaluate(self: *Self, expression: *Expr) Value {
        switch (expression.*) {
            .Literal => |*literal| return self.evaluateLiteral(literal),
            .Group => |*grouping| return self.evaluateGrouping(grouping),
            .Unary => |*unary| return self.evaluateUnaryExpression(unary),
            .Binary => |*binary| return self.evaluateBinaryExpression(binary),
        }
    }
};
