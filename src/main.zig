const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const Parser = @import("./parser.zig").Parser;
const InterpreterModule = @import("./interpreter.zig");

const Interpreter = InterpreterModule.Interpreter;

pub fn main() !void {
    // You can use print statements as follows for debugging, they'll be visible when running tests.

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh <command> <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (!std.mem.eql(u8, command, "tokenize") and !std.mem.eql(u8, command, "parse") and !std.mem.eql(u8, command, "evaluate") and !std.mem.eql(u8, command, "run")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    const stdout = std.io.getStdOut().writer();

    // Scanning phase
    var scanner = try Scanner.new(file_contents, std.heap.page_allocator);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();
    const scanner_errors = scanner.getErrors();

    // Output tokens if requested
    if (std.mem.eql(u8, command, "tokenize")) {
        for (scanner_errors) |scan_error| {
            std.debug.print("{s}", .{scan_error});
        }

        for (tokens) |token| {
            try stdout.print("{s}", .{token});
        }

        // Exit with error if there were any scanner or parser errors
        if (scanner_errors.len > 0) {
            std.process.exit(65);
        }
    }

    // Parsing phase
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const arena_allocator = arena.allocator();
    defer arena.deinit();

    var parser = Parser.init(arena_allocator, tokens);
    defer parser.deinit();

    var interpreter = Interpreter.init(arena_allocator);

    // Output parsed expression if requested
    if (std.mem.eql(u8, command, "parse")) {
        const expressions = try parser.parseExpression();

        for (parser.token_consumer.errors.items) |err| {
            std.debug.print("{}", .{err});
        }
        if (expressions) |expr| {
            try stdout.print("{}\n", .{expr});
        }

        // Exit with error if there were any parser errors
        if (parser.token_consumer.errors.items.len > 0) {
            std.process.exit(65);
        }
    } else if (std.mem.eql(u8, command, "evaluate")) {
        const expressions = try parser.parseExpression() orelse unreachable;
        try interpreter.interpret(expressions);
        if (interpreter.runtime_error) {
            std.process.exit(70);
        }
    } else if (std.mem.eql(u8, command, "run")) {
        const statements = parser.parse() catch |err| {
            std.debug.print("Error: {}\n", .{err});
            std.process.exit(65);
        };
        try interpreter.run(statements);
        if (interpreter.runtime_error) {
            std.process.exit(70);
        }
    }
}
