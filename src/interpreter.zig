const std = @import("std");
const types = @import("./types.zig");

const Value = types.Value;
const Expr = types.Expr;
const LiteralExpression = types.LiteralExpression;
const Literal = types.Literal;
const GroupingExpression = types.GroupingExpression;
const UnaryExpression = types.UnaryExpression;
const BinaryExpression = types.BinaryExpression;
const AssignmentExpression = types.AssignmentExpression;
const Stmt = types.Stmt;
const Token = types.Token;
const Environment = types.Envirnoment;

pub const InterpreterErrorSet = error{
    OutOfMemory,
    NoSpaceLeft,
    OperandMustBeNumber,
    OperandsMustBeNumbers,
    OperandsMustBeTwoNumbersOrStrings,
    UndefinedVariable,
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
            else => unreachable,
        }
        try std.fmt.format(writer, "[line {d}]\n", .{self.line});
    }
};

pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    runtime_error: bool = false,
    env: Environment,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return .{
            .allocator = allocator,
            .env = Environment.init(allocator, null),
        };
    }

    pub fn interpret(self: *Self, expression: *Expr) !Value {
        if (self.evaluate(expression)) |value| {
            return value;
        } else |err| {
            self.runtime_error = true;
            switch (err) {
                InterpreterErrorSet.OperandMustBeNumber => std.debug.print("{s}", .{
                    "Operand must be a number.\n",
                }),
                InterpreterErrorSet.OperandsMustBeNumbers => std.debug.print("{s}", .{
                    "Operands must be numbers.\n",
                }),
                InterpreterErrorSet.OperandsMustBeTwoNumbersOrStrings => std.debug.print("{s}", .{
                    "Operands must be two numbers or two strings.\n",
                }),
                else => {},
            }
            switch (expression.*) {
                .Binary => |binary| {
                    std.debug.print("[line {d}]\n", .{binary.operator.line});
                },
                .Unary => |unary| {
                    std.debug.print("[line {d}]\n", .{unary.operator.line});
                },
                .Var => |v| {
                    std.debug.print("[line {d}]\n", .{v.line});
                },
                .Assign => |v| {
                    std.debug.print("[line {d}]\n", .{v.name.line});
                },
                else => unreachable,
            }
            return err;
        }
    }

    pub fn run(self: *Self, statements: []const Stmt) !void {
        for (statements) |stm| {
            try self.interpretStatement(stm);
        }
    }

    pub fn interpretStatement(self: *Self, stm: Stmt) InterpreterErrorSet!void {
        switch (stm) {
            .Expression => |expression| {
                _ = try self.interpret(expression);
            },
            .Print => |expression| {
                const value = try self.interpret(expression);
                std.io.getStdOut().writer().print("{s}\n", .{value}) catch unreachable;
            },
            .Var => |v| {
                const val: Value = try self.interpret(v.initializer);
                try self.env.define(v.name.lexeme, val);
            },
            .Block => |block| {
                try self.interpretBlock(block);
            },
        }
    }

    pub fn interpretBlock(self: *Self, statements: []const Stmt) InterpreterErrorSet!void {
        var prev_env = self.env;
        var new_env = Environment.init(self.allocator, &prev_env);

        self.env = new_env;

        for (statements) |stm| {
            try self.interpretStatement(stm);
        }

        new_env.deinit();
        self.env = prev_env;
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
                            else => return InterpreterErrorSet.OperandsMustBeNumbers,
                        }
                    },
                    else => return InterpreterErrorSet.OperandsMustBeNumbers,
                }
            },
            .SLASH => {
                switch (left) {
                    .num => |num| {
                        switch (right) {
                            .num => |right_num| return .{ .num = num / right_num },
                            else => return InterpreterErrorSet.OperandsMustBeNumbers,
                        }
                    },
                    else => return InterpreterErrorSet.OperandsMustBeNumbers,
                }
            },
            .STAR => {
                switch (left) {
                    .num => |num| {
                        switch (right) {
                            .num => |right_num| return .{ .num = num * right_num },
                            else => return InterpreterErrorSet.OperandsMustBeNumbers,
                        }
                    },
                    else => return InterpreterErrorSet.OperandsMustBeNumbers,
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
                            else => return InterpreterErrorSet.OperandsMustBeTwoNumbersOrStrings,
                        }
                    },
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .num = left_num + right_num },
                            else => return InterpreterErrorSet.OperandsMustBeTwoNumbersOrStrings,
                        }
                    },
                    else => return InterpreterErrorSet.OperandsMustBeTwoNumbersOrStrings,
                }
            },
            .GREATER => {
                switch (left) {
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .bool = left_num > right_num },
                            else => return InterpreterErrorSet.OperandsMustBeNumbers,
                        }
                    },
                    else => return InterpreterErrorSet.OperandsMustBeNumbers,
                }
            },
            .LESS => {
                switch (left) {
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .bool = left_num < right_num },
                            else => return InterpreterErrorSet.OperandsMustBeNumbers,
                        }
                    },
                    else => return InterpreterErrorSet.OperandsMustBeNumbers,
                }
            },
            .GREATER_EQUAL => {
                switch (left) {
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .bool = left_num >= right_num },
                            else => return InterpreterErrorSet.OperandsMustBeNumbers,
                        }
                    },
                    else => return InterpreterErrorSet.OperandsMustBeNumbers,
                }
            },
            .LESS_EQUAL => {
                switch (left) {
                    .num => |left_num| {
                        switch (right) {
                            .num => |right_num| return .{ .bool = left_num <= right_num },
                            else => return InterpreterErrorSet.OperandsMustBeNumbers,
                        }
                    },
                    else => return InterpreterErrorSet.OperandsMustBeNumbers,
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

    fn evaluateVarExpression(self: *Self, token: Token) InterpreterErrorSet!Value {
        return self.env.get(token.lexeme) catch |err| {
            std.debug.print("Undefined variable '{s}'.\n", .{token.lexeme});
            return err;
        };
    }

    fn evaluateAssignment(self: *Self, expr: *AssignmentExpression) InterpreterErrorSet!Value {
        const value = try self.evaluate(expr.value);
        self.env.assign(expr.name.lexeme, value) catch |err| {
            std.debug.print("Undefined variable '{s}'.\n", .{expr.name.lexeme});
            return err;
        };
        return value;
    }

    fn evaluate(self: *Self, expression: *Expr) InterpreterErrorSet!Value {
        switch (expression.*) {
            .Literal => |*literal| return self.evaluateLiteral(literal),
            .Group => |*grouping| return self.evaluateGrouping(grouping),
            .Unary => |*unary| return self.evaluateUnaryExpression(unary),
            .Binary => |*binary| return self.evaluateBinaryExpression(binary),
            .Assign => |*assign| return self.evaluateAssignment(assign),
            .Var => |token| return self.evaluateVarExpression(token),
        }
    }
};
