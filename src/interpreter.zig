const std = @import("std");
const types = @import("./types.zig");
const Expr = types.Expr;
const LiteralExpression = types.LiteralExpression;
const Literal = types.Literal;
const GroupingExpression = types.GroupingExpression;
const UnaryExpression = types.UnaryExpression;

const Value = union(enum) {
    literal: LiteralExpression,

    pub fn format(this: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .literal => |value| try std.fmt.format(writer, "{}", .{value}),
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

    pub fn interpret(self: *Self, expression: *Expr) !Value {
        return self.evaluate(expression);
    }

    // lets first evaluate true, false, and nil
    fn evaluateLiteral(_: *Self, expression: *LiteralExpression) !Value {
        return .{ .literal = expression.* };
    }

    fn evaluate(self: *Self, expression: *Expr) !Value {
        switch (expression.*) {
            .Literal => |*literal| return try self.evaluateLiteral(literal),
            else => unreachable,
        }
    }
};
