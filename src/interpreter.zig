const std = @import("std");
const types = @import("./types.zig");
const Expr = types.Expr;
const LiteralExpression = types.LiteralExpression;
const Literal = types.Literal;
const GroupingExpression = types.GroupingExpression;
const UnaryExpression = types.UnaryExpression;
const BinaryExpression = types.BinaryExpression;

pub const InterpreterErrorSet = error{
    OutOfMemory,
    NoSpaceLeft,
    OperandMustBeNumber,
};

pub const InterpreterError = struct {
    error_type: InterpreterErrorSet,
    line: usize,

    pub fn new(error_type: InterpreterErrorSet, line: usize) InterpreterError {
        return .{
            .error_type = error_type,
            .line = line,
        };
    }

    pub fn format(self: InterpreterError, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.error_type) {
            InterpreterErrorSet.OperandMustBeNumber => try std.fmt.format(writer, "{s}", .{"Operand must be a number.\n"}),
            else => unreachable,
        }
        try std.fmt.format(writer, "[line {d}]\n", .{self.line});
    }
};

const Value = union(enum) {
    bool: bool,
    nil: void,
    num: f64,
    str: []const u8,

    pub fn format(this: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .bool => |value| try std.fmt.format(writer, "{}", .{value}),
            .nil => try std.fmt.format(writer, "{s}", .{"nil"}),
            .num => |value| {
                var buf: [256]u8 = undefined;
                const str = try std.fmt.bufPrint(&buf, "{d}", .{value});
                // if the number ends with .0, remove the .0
                if (std.mem.endsWith(u8, str, ".0")) {
                    try writer.writeAll(str[0 .. str.len - 2]);
                } else {
                    try writer.writeAll(str);
                }
            },
            .str => |value| try std.fmt.format(writer, "{s}", .{value}),
        }
    }
};

pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    runtime_error: ?InterpreterError = null,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return .{
            .allocator = allocator,
        };
    }

    pub fn interpret(self: *Self, expression: *Expr) InterpreterErrorSet!Value {
        return self.evaluate(expression);
    }

    fn evaluateLiteral(_: *Self, expression: *LiteralExpression) Value {
        return switch (expression.*) {
            .bool => |value| .{ .bool = value },
            .nil => .{ .nil = {} },
            .literal => |value| {
                switch (value) {
                    .num => |num| return .{ .num = num },
                    .str => |str| return .{ .str = str },
                }
            },
        };
    }

    fn evaluateGrouping(self: *Self, expression: *GroupingExpression) InterpreterErrorSet!Value {
        return self.evaluate(expression.expression);
    }

    fn evaluateUnaryExpression(self: *Self, expression: *UnaryExpression) InterpreterErrorSet!Value {
        const right = try self.evaluate(expression.right);

        switch (expression.operator.type) {
            .MINUS => {
                switch (right) {
                    .num => |num| return .{ .num = -num },
                    else => {
                        self.runtime_error = InterpreterError.new(
                            InterpreterErrorSet.OperandMustBeNumber,
                            expression.operator.line,
                        );
                        return InterpreterErrorSet.OperandMustBeNumber;
                    },
                }
            },
            .BANG => {
                return .{ .bool = !self.isTruthy(right) };
            },
            else => unreachable,
        }
    }

    fn evaluateBinaryExpression(self: *Self, expression: *BinaryExpression) InterpreterErrorSet!Value {
        const left = try self.evaluate(expression.left);
        const right = try self.evaluate(expression.right);

        switch (expression.operator.type) {
            .MINUS => {
                switch (left) {
                    .num => |num| {
                        switch (right) {
                            .num => |right_num| return .{ .num = num - right_num },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .SLASH => {
                switch (left) {
                    .num => |num| {
                        switch (right) {
                            .num => |right_num| return .{ .num = num / right_num },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .STAR => {
                switch (left) {
                    .num => |num| {
                        switch (right) {
                            .num => |right_num| return .{ .num = num * right_num },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .PLUS => {
                switch (left) {
                    .str => |left_str| {
                        switch (right) {
                            .str => |right_str| {
                                const len = left_str.len + right_str.len + 1;
                                const buf = try self.allocator.alloc(u8, len);
                                const str = try std.fmt.bufPrint(buf, "{s}{s}", .{ left_str, right_str });
                                return .{ .str = str };
                            },
                            else => unreachable,
                        }
                    },
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .num = left_num + right_num },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .GREATER => {
                switch (left) {
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .bool = left_num > right_num },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .LESS => {
                switch (left) {
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .bool = left_num < right_num },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .GREATER_EQUAL => {
                switch (left) {
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .bool = left_num >= right_num },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .LESS_EQUAL => {
                switch (left) {
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .bool = left_num <= right_num },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .BANG_EQUAL => {
                return .{ .bool = !self.isEqual(left, right) };
            },
            .EQUAL_EQUAL => {
                return .{ .bool = self.isEqual(left, right) };
            },
            else => unreachable,
        }
    }

    fn isEqual(_: *Self, left: Value, right: Value) bool {
        switch (left) {
            .num => |left_num| {
                switch (right) {
                    .num => |right_num| return left_num == right_num,
                    else => return false,
                }
            },
            .bool => |left_bool| {
                switch (right) {
                    .bool => |right_bool| return left_bool == right_bool,
                    else => return false,
                }
            },
            .str => |left_str| {
                switch (right) {
                    .str => |right_str| return std.mem.eql(u8, left_str, right_str),
                    else => return false,
                }
            },
            .nil => {
                switch (right) {
                    .nil => return true,
                    else => return false,
                }
            },
        }
    }

    fn isTruthy(_: *Self, value: Value) bool {
        return switch (value) {
            .bool => |b| b,
            .nil => false,
            else => true,
        };
    }

    fn evaluate(self: *Self, expression: *Expr) InterpreterErrorSet!Value {
        switch (expression.*) {
            .Literal => |*literal| return self.evaluateLiteral(literal),
            .Group => |*grouping| return self.evaluateGrouping(grouping),
            .Unary => |*unary| return self.evaluateUnaryExpression(unary),
            .Binary => |*binary| return self.evaluateBinaryExpression(binary),
        }
    }
};
