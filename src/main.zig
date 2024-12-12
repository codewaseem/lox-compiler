const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const Parser = @import("./parser.zig").Parser;
const Interpreter = @import("./interpreter.zig").Interpreter;

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

    if (!std.mem.eql(u8, command, "tokenize") and !std.mem.eql(u8, command, "parse") and !std.mem.eql(u8, command, "evaluate")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    // Scanning phase
    var scanner = try Scanner.new(file_contents, std.heap.page_allocator);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();
    const scanner_errors = try scanner.getErrors();

    const stdout = std.io.getStdOut().writer();

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
    defer arena.deinit();

    var parser = Parser.init(arena.allocator(), tokens);
    const expressions = try parser.parse();

    // Output parsed expression if requested
    if (std.mem.eql(u8, command, "parse")) {
        for (parser.errors.items) |err| {
            std.debug.print("{}", .{err});
        }
        if (expressions) |expr| {
            try stdout.print("{}\n", .{expr});
        }

        // Exit with error if there were any parser errors
        if (parser.errors.items.len > 0) {
            std.process.exit(65);
        }
    }

    // Evaluation phase
    if (expressions) |expr| {
        // Output evaluated result if requested
        if (std.mem.eql(u8, command, "evaluate")) {
            var interpreter = Interpreter.init(std.heap.page_allocator);
            const result = interpreter.interpret(expr);

            try stdout.print("{}\n", .{result});
        }
    }
}
