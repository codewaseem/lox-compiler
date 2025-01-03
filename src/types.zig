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

pub const Value = union(enum) {
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

pub const Envirnoment = struct {
    allocator: std.mem.Allocator,
    values: std.StringHashMap(Value),
    enclosing: ?*Envirnoment = null,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Envirnoment) Envirnoment {
        return .{
            .allocator = allocator,
            .values = std.StringHashMap(Value).init(allocator),
            .enclosing = enclosing,
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit();
    }

    pub fn define(self: *Self, name: []const u8, val: Value) !void {
        try self.values.put(name, val);
    }

    pub fn assign(self: *Self, name: []const u8, val: Value) !void {
        if (self.values.contains(name)) {
            try self.values.put(name, val);
            return;
        }

        if (self.enclosing) |enc| {
            try enc.assign(name, val);
            return;
        }

        return error.UndefinedVariable;
    }

    pub fn get(self: *Self, name: []const u8) !Value {
        if (self.values.contains(name)) {
            return self.values.get(name).?;
        }

        if (self.enclosing) |enc| {
            return enc.get(name);
        }

        return error.UndefinedVariable;
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

pub const AssignmentExpression = struct {
    name: Token,
    value: *Expr,
};

pub const LogicalExpression = struct {
    left: *Expr,
    right: *Expr,
    operator: Token,
};

pub const Expr = union(enum) {
    Binary: BinaryExpression,
    Unary: UnaryExpression,
    Group: GroupingExpression,
    Logical: LogicalExpression,
    Literal: LiteralExpression,
    Assign: AssignmentExpression,
    Var: Token,

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
            .Assign => |assign| {
                try std.fmt.format(
                    writer,
                    "(= {s} {})",
                    .{ assign.name.lexeme, assign.value },
                );
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
            .Var => |token| {
                try std.fmt.format(writer, "{s}", .{token.lexeme});
            },
            .Logical => |logical| {
                try std.fmt.format(writer, "{} {s} {}", .{
                    logical.left,
                    logical.operator.lexeme,
                    logical.right,
                });
            },
        }
    }
};

pub const Variable = struct {
    name: Token,
    initializer: *Expr,
};

pub const IfStmt = struct {
    condition: *Expr,
    then_branch: *Stmt,
    else_branch: ?*Stmt = null,
};

pub const WhileStmt = struct {
    condition: *Expr,
    block: *Stmt,
};

pub const Stmt = union(enum) {
    Expression: *Expr,
    Print: *Expr,
    Var: Variable,
    Block: []const Stmt,
    If: IfStmt,
    While: WhileStmt,

    pub fn new(allocator: std.mem.Allocator, value: Stmt) !*Stmt {
        const stmt_ptr = try allocator.create(Stmt);
        errdefer allocator.destroy(stmt_ptr);

        stmt_ptr.* = value;
        return stmt_ptr;
    }

    pub fn format(this: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (this) {
            .Expression => |value| {
                try std.fmt.format(writer, "{}", .{value});
            },
            .Print => |value| {
                try std.fmt.format(writer, "{}", .{value});
            },
            .Var => |variable| {
                try std.fmt.format(writer, "{}", .{variable.initializer});
            },
            .Block => |block| {
                for (block) |stmt| {
                    try std.fmt.format(writer, "{}", .{stmt});
                }
            },
            .If => |if_statement| {
                try std.fmt.format(writer, "if ({?}) \n\t then {?} \n\t else {?}", .{
                    if_statement.condition,
                    if_statement.then_branch,
                    if_statement.else_branch,
                });
            },
            .While => |while_stmt| {
                try std.fmt.format(writer, "while ({?}) \n\t {{ {%s} }}", .{
                    while_stmt.condition,
                    while_stmt.block,
                });
            },
        }
    }
};
