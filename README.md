[![progress-banner](https://backend.codecrafters.io/progress/interpreter/6d4a4ae6-88fa-4533-b3fc-6679220f7184)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

This is a starting point for Zig solutions to the
["Build your own Interpreter" Challenge](https://app.codecrafters.io/courses/interpreter/overview).

This challenge follows the book
[Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.

In this challenge you'll build an interpreter for
[Lox](https://craftinginterpreters.com/the-lox-language.html), a simple
scripting language. Along the way, you'll learn about tokenization, ASTs,
tree-walk interpreters and more.

Before starting this challenge, make sure you've read the "Welcome" part of the
book that contains these chapters:

- [Introduction](https://craftinginterpreters.com/introduction.html) (chapter 1)
- [A Map of the Territory](https://craftinginterpreters.com/a-map-of-the-territory.html)
  (chapter 2)
- [The Lox Language](https://craftinginterpreters.com/the-lox-language.html)
  (chapter 3)

These chapters don't involve writing code, so they won't be covered in this
challenge. This challenge will start from chapter 4,
[Scanning](https://craftinginterpreters.com/scanning.html).

**Note**: If you're viewing this repo on GitHub, head over to
[codecrafters.io](https://codecrafters.io) to try the challenge.



# Implementation Details

This is a Turing-complete implementation of the Lox interpreter in Zig, following the classic phases of interpretation:

## 1. Lexical Analysis (Scanner)
- Converts source code into tokens
- Handles identifiers, keywords, literals, and operators
- Provides detailed error reporting for invalid characters

## 2. Syntactic Analysis (Parser)
- Builds Abstract Syntax Tree (AST) from tokens
- Implements recursive descent parsing
- Supports expressions, statements, and control flow

## 3. Execution (Interpreter)
- Tree-walk interpreter that executes the AST
- Manages runtime environment and variable scoping
- Supports arithmetic operations, strings, and control flow
- Handles runtime error reporting

## Project Structure
```
src/
├── main.zig       # Entry point and command handling
├── scanner.zig    # Lexical analysis
├── parser.zig     # Syntactic analysis
├── interpreter.zig # Execution engine
└── types.zig      # Core type definitions
```

## Features
- Variable declarations and assignments
- Arithmetic and logical operations
- Control flow (if, while, for)
- Runtime error handling
