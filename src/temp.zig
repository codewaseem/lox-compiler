// const ExprType = enum { Binary, Unary, Literal, Grouping };

// const Expr = union(ExprType) {
//     Binary: *BinaryExpr,
//     Unary: *UnaryExpr,
//     Literal: *LiteralExpr,
//     Grouping: *GroupingExpr,

//     pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
//         switch (value) {
//             .Binary => {
//                 try std.fmt.format(writer, "{}", .{value.Binary});
//             },
//             .Unary => {
//                 try std.fmt.format(writer, "{}", .{value.Unary});
//             },
//             .Literal => {
//                 try std.fmt.format(writer, "{}", .{value.Literal});
//             },
//             .Grouping => {
//                 try std.fmt.format(writer, "{}", .{value.Grouping});
//             },
//         }
//     }
// };

// const BinaryExpr = struct {
//     left: *Expr,
//     right: *Expr,
//     operator: Token,

//     pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
//         try std.fmt.format(writer, "({s} {} {})", .{
//             value.operator.lexeme,
//             value.left,
//             value.right,
//         });
//     }
// };

// const UnaryExpr = struct {
//     right: *Expr,
//     operator: Token,

//     pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
//         try std.fmt.format(writer, "({s} {})", .{
//             value.operator.lexeme,
//             value.right,
//         });
//     }
// };

// const LiteralExpr = struct {
//     value: Literal,

//     pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
//         try std.fmt.format(writer, "{}", .{
//             value.value,
//         });
//     }
// };

// const GroupingExpr = struct {
//     expression: *Expr,

//     pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
//         try std.fmt.format(writer, "(group {})", .{
//             value.expression,
//         });
//     }
// };
