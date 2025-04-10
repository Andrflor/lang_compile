package compiler

/*
 * ======================================================================
 * Language Compiler Implementation
 *
 * This package implements a compiler for a custom language with features
 * like pointings, patterns, and constraints.
 *
 * Organization:
 * 1. Token definitions and lexer
 * 2. AST node definitions
 * 3. Fixed parser implementation with Pratt parsing for expressions
 * 4. Utility functions
 * 5. Error handling and recovery
 * ======================================================================
 */

import "core:fmt"
import "core:os"
import "core:strings"
import vmem "core:mem/virtual"

// ===========================================================================
// SECTION 1: TOKEN DEFINITIONS AND LEXER
// ===========================================================================

/*
 * Token_Kind represents all possible token types in the language
 */
Token_Kind :: enum {
	Invalid,
	EOF,
	Identifier,

	// Numbers
	Integer,
	Float,
	Hexadecimal,
	Binary,

	// String Literal
	String_Literal,

	// Executors
	Execute, // !
	At,

	// Assignators
	PointingPush, // ->
	PointingPull, // <-
	EventPush, // >-
	EventPull, // -<
	ResonancePush, // >>-
	ResonancePull, // -<<

	// Comparisons
	Equal, // =
	LessThan, // <
	GreaterThan, // >
	LessEqual, // <=
	GreaterEqual, // >=

	// Separators
	Colon, // :
	Question, // ?
	Dot, // .
	DoubleDot, // ..
	Ellipsis, // ...
	Newline, // \n
	Range, // 1..2 (full range)
	PrefixRange, // ..1
	PostfixRange, // 1..

	// Grouping
	LeftBrace, // {
	RightBrace, // }
	LeftParen, // (
	RightParen, // )
  LeftBracket, // [
  RightBracket, // ]

	// Math & Logic
	Plus, // +
	Minus, // -
	Asterisk, // *
	Slash, // /
	Percent, // %
	BitAnd, // &
	BitOr, // |
	BitXor, // ^
	BitNot, // ~
}

/*
 * Position represents a location in the source code with line and column information
 */
Position :: struct {
    line:   int, // Line number (1-based)
    column: int, // Column number (1-based)
    offset: int, // Absolute offset in source
}

/*
 * Token represents a lexical token with its kind, text content and position
 */
Token :: struct {
    kind:     Token_Kind, // Type of token
    text:     string,     // Original text of the token
    position: Position,   // Position information (line, column, offset)
}

/*
 * Lexer maintains state during lexical analysis
 */
Lexer :: struct {
    source:      string, // Source code being lexed
    position:    Position, // Current position in source (line, column, offset)
    peek_offset: int, // Lookahead for peeking at characters without advancing
    line_starts: []int, // Precomputed array of line start positions for faster line/column calculation
    source_len:  int, // Cache length to avoid repeated calls
}

/*
 * create_position creates a new Position struct
 */
create_position :: #force_inline proc(line, column, offset: int) -> Position {
    return Position{line = line, column = column, offset = offset}
}

/*
 * init_lexer initializes a lexer with the given source code with optimized line tracking
 */
init_lexer :: proc(l: ^Lexer, source: string) {
    l.source = source
    l.position = create_position(1, 1, 0) // Start at line 1, column 1
    l.peek_offset = 0
    l.source_len = len(source)

    // Pre-compute line starts for faster line/column calculation
    // This is a significant optimization for large files
    line_count := 1 // At least one line
    for i := 0; i < l.source_len; i += 1 {
        if source[i] == '\n' {
            line_count += 1
        }
    }

    // Allocate and initialize line starts array
    l.line_starts = make([]int, line_count, context.allocator)
    l.line_starts[0] = 0 // First line starts at offset 0

    line_idx := 1
    for i := 0; i < l.source_len; i += 1 {
        if source[i] == '\n' && line_idx < line_count {
            l.line_starts[line_idx] = i + 1
            line_idx += 1
        }
    }
}

/*
 * current_char returns the current character without advancing
 */
current_char :: #force_inline proc(l: ^Lexer) -> u8 {
    if l.position.offset >= l.source_len {
        return 0
    }
    return l.source[l.position.offset]
}

/*
 * peek_char looks ahead by n characters without advancing
 */
peek_char :: #force_inline proc(l: ^Lexer, n: int = 1) -> u8 {
    peek_pos := l.position.offset + n
    if peek_pos >= l.source_len {
        return 0
    }
    return l.source[peek_pos]
}

/*
 * advance_position moves the lexer position forward by one character,
 * using precomputed line starts for faster line/column updates
 */
advance_position :: #force_inline proc(l: ^Lexer) {
    if l.position.offset < l.source_len {
        // Check if we're at a newline character
        if l.source[l.position.offset] == '\n' {
            l.position.line += 1
            l.position.column = 1
        } else {
            l.position.column += 1
        }
        l.position.offset += 1
    }
}

/*
 * advance_by advances the lexer position by n characters
 * Optimized to avoid excessive function calls
 */
advance_by :: #force_inline proc(l: ^Lexer, n: int) {
    if n <= 0 do return

    for i := 0; i < n && l.position.offset < l.source_len; i += 1 {
        // Direct inlining of advance_position for speed
        if l.source[l.position.offset] == '\n' {
            l.position.line += 1
            l.position.column = 1
        } else {
            l.position.column += 1
        }
        l.position.offset += 1
    }
}

/*
 * match_char checks if the current character matches expected
 * and advances if it does, returning true on match
 */
match_char :: #force_inline proc(l: ^Lexer, expected: u8) -> bool {
    if l.position.offset >= l.source_len || l.source[l.position.offset] != expected {
        return false
    }
    advance_position(l)
    return true
}

/*
 * match_str checks if the string at current position matches expected
 * and advances by the length of the string if it does
 */
match_str :: proc(l: ^Lexer, expected: string) -> bool {
    if l.position.offset + len(expected) > l.source_len {
        return false
    }

    for i := 0; i < len(expected); i += 1 {
        if l.source[l.position.offset + i] != expected[i] {
            return false
        }
    }

    advance_by(l, len(expected))
    return true
}

/*
 * next_token scans and returns the next token from the input source
 * Optimized for speed by reducing function calls and using direct pattern matching
 */
next_token :: proc(l: ^Lexer) -> Token {
    skip_whitespace(l)

    if l.position.offset >= l.source_len {
        return Token{kind = .EOF, position = l.position}
    }

    start_pos := l.position
    c := l.source[l.position.offset]

    // Use a jump table approach for faster dispatch
    switch c {
    case '\n':
        return scan_newline(l, start_pos)
    case '`', '"', '\'':
        return scan_string(l, start_pos)
    case '@':
        advance_position(l)
        return Token{kind = .At, text = "@", position = start_pos}
    case '{':
        advance_position(l)
        return Token{kind = .LeftBrace, text = "{", position = start_pos}
    case '}':
        advance_position(l)
        return Token{kind = .RightBrace, text = "}", position = start_pos}
    case '[':
        advance_position(l)
        return Token{kind = .LeftBracket, text = "[", position = start_pos}
    case ']':
        advance_position(l)
        return Token{kind = .RightBracket, text = "]", position = start_pos}
    case '(':
        advance_position(l)
        return Token{kind = .LeftParen, text = "(", position = start_pos}
    case ')':
        advance_position(l)
        return Token{kind = .RightParen, text = ")", position = start_pos}
    case '!':
        advance_position(l)
        return Token{kind = .Execute, text = "!", position = start_pos}
    case ':':
        advance_position(l)
        return Token{kind = .Colon, text = ":", position = start_pos}
    case '?':
        advance_position(l)
        return Token{kind = .Question, text = "?", position = start_pos}
    case '.':
        // Optimized to use direct character checks instead of function calls
        if l.position.offset + 1 < l.source_len && l.source[l.position.offset + 1] == '.' {
            advance_by(l, 2) // Skip ".."

            // Check for ellipsis "..."
            if l.position.offset < l.source_len && l.source[l.position.offset] == '.' {
                advance_position(l)
                return Token{kind = .Ellipsis, text = "...", position = start_pos}
            }

            // Check for prefix range "..1"
            if l.position.offset < l.source_len && is_digit(l.source[l.position.offset]) {
                start_num := l.position.offset
                for l.position.offset < l.source_len && is_digit(l.source[l.position.offset]) {
                    advance_position(l)
                }
                return Token{kind = .PrefixRange, text = l.source[start_pos.offset:l.position.offset], position = start_pos}
            }

            return Token{kind = .DoubleDot, text = "..", position = start_pos}
        }

        advance_position(l)
        return Token{kind = .Dot, text = ".", position = start_pos}
    case '=':
        // Optimized equality check
        advance_position(l)
        if l.position.offset < l.source_len && l.source[l.position.offset] == '=' {
            advance_position(l)
            return Token{kind = .Equal, text = "==", position = start_pos}
        }
        return Token{kind = .Equal, text = "=", position = start_pos}
    case '<':
        // Optimized less-than related tokens
        advance_position(l)
        if l.position.offset < l.source_len {
            if l.source[l.position.offset] == '=' {
                advance_position(l)
                return Token{kind = .LessEqual, text = "<=", position = start_pos}
            } else if l.source[l.position.offset] == '-' {
                advance_position(l)
                return Token{kind = .PointingPull, text = "<-", position = start_pos}
            }
        }
        return Token{kind = .LessThan, text = "<", position = start_pos}
    case '>':
        // Optimized greater-than related tokens
        advance_position(l)
        if l.position.offset < l.source_len {
            if l.source[l.position.offset] == '=' {
                advance_position(l)
                return Token{kind = .GreaterEqual, text = ">=", position = start_pos}
            } else if l.source[l.position.offset] == '-' {
                advance_position(l)
                return Token{kind = .EventPush, text = ">-", position = start_pos}
            } else if l.position.offset + 1 < l.source_len &&
                   l.source[l.position.offset] == '>' &&
                   l.source[l.position.offset + 1] == '-' {
                advance_by(l, 2)
                return Token{kind = .ResonancePush, text = ">>-", position = start_pos}
            }
        }
        return Token{kind = .GreaterThan, text = ">", position = start_pos}
    case '-':
        // Optimized minus-related tokens
        advance_position(l)
        if l.position.offset < l.source_len {
            if l.source[l.position.offset] == '>' {
                advance_position(l)
                return Token{kind = .PointingPush, text = "->", position = start_pos}
            } else if l.source[l.position.offset] == '<' {
                advance_position(l)
                if l.position.offset < l.source_len && l.source[l.position.offset] == '<' {
                    advance_position(l)
                    return Token{kind = .ResonancePull, text = "-<<", position = start_pos}
                }
                return Token{kind = .EventPull, text = "-<", position = start_pos}
            }
        }
        return Token{kind = .Minus, text = "-", position = start_pos}
    case '/':
        // Single line comment - optimized with direct string matching
        if l.position.offset + 1 < l.source_len && l.source[l.position.offset + 1] == '/' {
            advance_by(l, 2)

            // Consume until newline
            for l.position.offset < l.source_len && l.source[l.position.offset] != '\n' {
                advance_position(l)
            }

            // Recursive call to get the next non-comment token
            return next_token(l)
        }

        // Multi line comment
        if l.position.offset + 1 < l.source_len && l.source[l.position.offset + 1] == '*' {
            advance_by(l, 2)

            // Scan for */
            loop: for l.position.offset + 1 < l.source_len {
                if l.source[l.position.offset] == '*' && l.source[l.position.offset + 1] == '/' {
                    advance_by(l, 2) // Skip closing */
                    break loop
                }
                advance_position(l)
            }

            // Recursive call to get the next non-comment token
            return next_token(l)
        }

        advance_position(l)
        return Token{kind = .Slash, text = "/", position = start_pos}
    case '0':
        // Special number formats (hex, binary)
        if l.position.offset + 1 < l.source_len {
            next := l.source[l.position.offset + 1]

            if next == 'x' || next == 'X' {
                return scan_hexadecimal(l, start_pos)
            }

            if next == 'b' || next == 'B' {
                return scan_binary(l, start_pos)
            }
        }

        // Fall through to regular number handling
        fallthrough
    case '1', '2', '3', '4', '5', '6', '7', '8', '9':
        return scan_number(l, start_pos)
    case '+':
        advance_position(l)
        return Token{kind = .Plus, text = "+", position = start_pos}
    case '*':
        advance_position(l)
        return Token{kind = .Asterisk, text = "*", position = start_pos}
    case '%':
        advance_position(l)
        return Token{kind = .Percent, text = "%", position = start_pos}
    case '&':
        advance_position(l)
        return Token{kind = .BitAnd, text = "&", position = start_pos}
    case '|':
        advance_position(l)
        return Token{kind = .BitOr, text = "|", position = start_pos}
    case '^':
        advance_position(l)
        return Token{kind = .BitXor, text = "^", position = start_pos}
    case '~':
        advance_position(l)
        return Token{kind = .BitNot, text = "~", position = start_pos}
    case:
        // Identifiers - optimized with direct character range checks
        if is_alpha(c) || c == '_' {
            // Fast path for identifiers
            advance_position(l)

            // Consume all alphanumeric characters
            for l.position.offset < l.source_len && is_alnum(l.source[l.position.offset]) {
                advance_position(l)
            }

            return Token{kind = .Identifier, text = l.source[start_pos.offset:l.position.offset], position = start_pos}
        }

        // Unknown character
        advance_position(l)
        return Token{kind = .Invalid, text = string([]u8{c}), position = start_pos}
    }
}

/*
 * scan_newline processes consecutive newline characters
 * Optimized with a direct loop instead of recursive function calls
 */
scan_newline :: #force_inline proc(l: ^Lexer, start_pos: Position) -> Token {
    // Consume first newline
    advance_position(l)

    // Consume all consecutive newlines
    for l.position.offset < l.source_len && l.source[l.position.offset] == '\n' {
        advance_position(l)
    }

    return Token{kind = .Newline, text = "\\n", position = start_pos}
}

/*
 * scan_string processes a string literal enclosed in provided delimiter
 * Optimized for speed with fewer function calls
 */
scan_string :: proc(l: ^Lexer, start_pos: Position) -> Token {
    delimiter := l.source[l.position.offset]
    // Skip opening delimiter
    advance_position(l)
    str_start := l.position.offset

    // Fast path: scan without escapes
    // This optimization significantly improves performance for strings without escape sequences
    escape_found := false
    for l.position.offset < l.source_len {
        current := l.source[l.position.offset]

        if current == delimiter {
            break
        }

        if current == '\\' {
            escape_found = true
            break
        }

        advance_position(l)
    }

    // If we found an escape sequence, handle the more complex path
    if escape_found {
        for l.position.offset < l.source_len && l.source[l.position.offset] != delimiter {
            // Handle escapes
            if l.source[l.position.offset] == '\\' && l.position.offset + 1 < l.source_len {
                advance_by(l, 2)  // Skip the escape sequence
            } else {
                advance_position(l)
            }
        }
    }

    if l.position.offset < l.source_len {
        text := l.source[str_start:l.position.offset]
        advance_position(l) // Skip closing delimiter
        return Token{kind = .String_Literal, text = text, position = start_pos}
    }

    return Token{kind = .Invalid, text = "Unterminated string", position = start_pos}
}

/*
 * scan_hexadecimal processes hexadecimal number literals
 * Optimized with direct character checks
 */
scan_hexadecimal :: #force_inline proc(l: ^Lexer, start_pos: Position) -> Token {
    // Skip "0x" prefix
    advance_by(l, 2)
    hex_start := l.position.offset

    // Fast path: use a tight loop to consume all hex digits
    for l.position.offset < l.source_len && is_hex_digit(l.source[l.position.offset]) {
        advance_position(l)
    }

    if l.position.offset == hex_start {
        return Token{kind = .Invalid, text = "Invalid hexadecimal number", position = start_pos}
    }

    return Token{kind = .Hexadecimal, text = l.source[start_pos.offset:l.position.offset], position = start_pos}
}

/*
 * scan_binary processes binary number literals
 * Optimized with direct character checks
 */
scan_binary :: #force_inline proc(l: ^Lexer, start_pos: Position) -> Token {
    // Skip "0b" prefix
    advance_by(l, 2)
    bin_start := l.position.offset

    // Fast path: use tight loop for binary digits
    for l.position.offset < l.source_len {
        c := l.source[l.position.offset]
        if c != '0' && c != '1' {
            break
        }
        advance_position(l)
    }

    if l.position.offset == bin_start {
        return Token{kind = .Invalid, text = "Invalid binary number", position = start_pos}
    }

    return Token{kind = .Binary, text = l.source[start_pos.offset:l.position.offset], position = start_pos}
}

/*
 * scan_number processes numeric literals and range notations
 * Optimized to use direct character checks for different paths
 */
scan_number :: proc(l: ^Lexer, start_pos: Position) -> Token {
    // Parse integer part with fast path
    for l.position.offset < l.source_len && is_digit(l.source[l.position.offset]) {
        advance_position(l)
    }

    // Check for range notation (e.g., 1..5)
    if l.position.offset + 1 < l.source_len &&
       l.source[l.position.offset] == '.' &&
       l.source[l.position.offset + 1] == '.' {

        advance_by(l, 2) // Skip the '..'

        // Check if there's a number after, making it a full range (1..5)
        if l.position.offset < l.source_len && is_digit(l.source[l.position.offset]) {
            for l.position.offset < l.source_len && is_digit(l.source[l.position.offset]) {
                advance_position(l)
            }
            return Token{kind = .Range, text = l.source[start_pos.offset:l.position.offset], position = start_pos}
        }

        // Just a postfix range (1..)
        return Token{kind = .PostfixRange, text = l.source[start_pos.offset:l.position.offset], position = start_pos}
    }

    // Check for floating point
    if l.position.offset < l.source_len && l.source[l.position.offset] == '.' {
        if l.position.offset + 1 < l.source_len && is_digit(l.source[l.position.offset + 1]) {
            advance_position(l) // Skip the '.'

            // Fast path for decimal digits
            for l.position.offset < l.source_len && is_digit(l.source[l.position.offset]) {
                advance_position(l)
            }

            return Token{kind = .Float, text = l.source[start_pos.offset:l.position.offset], position = start_pos}
        }
    }

    return Token{kind = .Integer, text = l.source[start_pos.offset:l.position.offset], position = start_pos}
}

// ===========================================================================
// SECTION 2: AST NODE DEFINITIONS
// ===========================================================================

/*
 * Node is a union of all possible AST node types
 */
Node :: union {
	Pointing,
	PointingPull,
	EventPush,
	EventPull,
	ResonancePush,
	ResonancePull,
	Scope,
	Override,
	Product,
	Branch,
	Identifier,
	Pattern,
	Constraint,
	Operator,
	Execute,
	Literal,
	Property,
	Expand,
	FileSystem,
  Range,
}

/*
 * Pointing represents a pointing declaration (name -> value)
 */
Pointing :: struct {
	name:  ^Node, // Name of the pointing
	value: ^Node, // Value being pointed to
}

/*
 * Pointing pull is a declaration later override derived value
 */
PointingPull :: struct {
	name:  ^Node, // Name of the pointing
	value: ^Node, // Value being pointed from
}

/*
 * EventPull represents a event being pull from resonance >-
 */
EventPull :: struct {
	name:  ^Node, // Name of the pointing
	value: ^Node, // Value being pointed to
}

/*
 * EventPush represents a event being pushed into resonance -<
 */
EventPush :: struct {
	name:  ^Node, // Name of the pointing
	value: ^Node, // Value being pointed to
}

/*
 * ResonancePull is useed to change value of resonance driven -<<
 */
ResonancePull :: struct {
	name:  ^Node, // Name of the pointing
	value: ^Node, // Value being pointed to
}

/*
 * ResonancePush is useed to drive resonance >>-
 */
ResonancePush :: struct {
	name:  ^Node, // Name of the pointing
	value: ^Node, // Value being pointed to
}

/*
 * Identifier represents a named reference
 */
Identifier :: struct {
	name: string, // Name of the identifier
}

/*
 * Scope represents a block of statements enclosed in braces
 */
Scope :: struct {
	value: [dynamic]Node, // Statements in the scope
}

/*
 * Override represents modifications to a base entity
 */
Override :: struct {
	source:    ^Node, // Base entity being modified
	overrides: [dynamic]Node, // Modifications
}

/*
 * Product represents a produced value (-> expr)
 */
Product :: struct {
	value: ^Node, // Value produced
}

/*
 * Pattern represents a pattern match expression
 */
Pattern :: struct {
	target: ^Node, // Value to match against
	value:  [dynamic]Branch, // Pattern branches
}

/*
 * Branch represents a single pattern match branch
 */
Branch :: struct {
	source: ^Node, // Pattern to match
	product: ^Node, // Result if pattern matches
}

/*
 * Constraint represents a type constraint (Type: value)
 */
Constraint :: struct {
	constraint: ^Node, // Type constraint
	value:      ^Node, // Optional value
}

/*
 * ExecutionWrapper represents a single wrapper in a potentially nested execution pattern
 */
ExecutionWrapper :: enum {
    None,
    Sequential,      // !
    Threading,       // < >
    Parallel_CPU,    // [ ]
    Background,      // ( )
    GPU,             // | |
}

/*
 * Execute represents an execution modifier
 */
Execute :: struct {
    value: ^Node,                     // Expression to execute
    wrappers: [dynamic]ExecutionWrapper, // Ordered list of execution wrappers (from outside to inside)
}

/*
 * Operator_Kind defines the types of operators
 */
Operator_Kind :: enum {
	Plus,
	Minus,
	Mult,
	Div,
	Mod, // For %
	Equal,
	LessThan,
	GreaterThan,
	LessEqual,
	GreaterEqual,
	BitAnd, // &
	BitOr, // |
	BitXor, // ^
	BitNot, // ~
}

/*
 * Operator represents a binary operation
 */
Operator :: struct {
	kind:  Operator_Kind, // Type of operation
	left:  ^Node, // Left operand
	right: ^Node, // Right operand
}

/*
 * Literal_Kind defines the types of literal values
 */
Literal_Kind :: enum {
	Integer,
	Float,
	String,
	Hexadecimal,
	Binary,
}

/*
 * Literal represents a literal value in the source
 */
Literal :: struct {
	kind:  Literal_Kind, // Type of literal
	value: string, // String representation of the value
}

/*
 * Property represents a property access (a.b)
 */
Property :: struct {
	source:   ^Node, // Object being accessed
	property: ^Node, // Property being accessed
}

/*
 * Expand represents a content expansion (...expr)
 */
Expand :: struct {
	target: ^Node, // Content to expand
}

/*
 * FileSystem represents a file system reference (@lib.module)
 */
FileSystem :: struct {
	target: ^Node, // Target content in file system
}

/*
 * Range represents a range expression (e.g., 1..5, 1.., ..5)
 */
Range :: struct {
	start: ^Node, // Start of range (may be nil for prefix range)
	end:   ^Node, // End of range (may be nil for postfix range)
}

// ===========================================================================
// SECTION 3: PARSER IMPLEMENTATION
// ===========================================================================

/*
 * Precedence levels for operators, higher value means higher precedence
 */
Precedence :: enum {
    NONE,       // No precedence
    ASSIGNMENT, // =, ->, <-, >-, -<, >>-, -<<
    LOGICAL,    // Reserved for logical operators (&&, ||)
    EQUALITY,   // ==
    COMPARISON, // <, >, <=, >=
    RANGE,      // ..
    TERM,       // +, -
    FACTOR,     // *, /, %
    BITWISE,    // &, |, ^
    UNARY,      // !, ~, unary -
    CALL,       // (), ., : (constraint now at this level)
    PRIMARY,    // Literals, identifiers
}

/*
 * Parse_Rule defines how to parse a given token as prefix or infix
 */
Parse_Rule :: struct {
    prefix:     proc(parser: ^Parser, can_assign: bool) -> ^Node,
    infix:      proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node,
    precedence: Precedence,
}

/*
 * Parser maintains state during parsing
 */
Parser :: struct {
    lexer:           ^Lexer, // Lexer providing tokens
    current_token:   Token, // Current token being processed
    peek_token:      Token, // Next token (lookahead)
    had_error:       bool, // Flag indicating if an error occurred
    panic_mode:      bool, // Flag for panic mode error recovery
    error_count:     int, // Count of errors encountered
}

/*
 * initialize_parser sets up a parser with a lexer
 */
init_parser :: proc(parser: ^Parser, lexer: ^Lexer) {
    parser.lexer = lexer
    parser.had_error = false
    parser.panic_mode = false
    parser.error_count = 0

    // Initialize with first two tokens
    parser.current_token = next_token(lexer)
    parser.peek_token = next_token(lexer)
}

/*
 * advance_token moves to the next token in the stream
 */
advance_token :: #force_inline proc(parser: ^Parser) {
    parser.current_token = parser.peek_token
    parser.peek_token = next_token(parser.lexer)
}

/*
 * check checks if the current token has the expected kind without advancing
 */
check :: #force_inline proc(parser: ^Parser, kind: Token_Kind) -> bool {
    return parser.current_token.kind == kind
}

/*
 * match checks if the current token has the expected kind and advances if true
 */
match :: #force_inline proc(parser: ^Parser, kind: Token_Kind) -> bool {
    if !check(parser, kind) {
        return false
    }
    advance_token(parser)
    return true
}

/*
 * expect_token checks if the current token is of the expected kind,
 * advances to the next token if true, and reports an error if false
 */
expect_token :: #force_inline proc(parser: ^Parser, kind: Token_Kind) -> bool {
    if check(parser, kind) {
        advance_token(parser)
        return true
    }

    error_at_current(parser, fmt.tprintf("Expected %v but got %v", kind, parser.current_token.kind))
    return false
}

/*
 * error_at_current reports an error at the current token
 */
error_at_current :: #force_inline proc(parser: ^Parser, message: string) {
    error_at(parser, parser.current_token, message)
}

/*
 * error_at reports an error at a specific token with line and column info
 */
error_at :: #force_inline proc(parser: ^Parser, token: Token, message: string) {
    // Don't report errors in panic mode to avoid cascading
    if parser.panic_mode do return

    parser.panic_mode = true
    parser.had_error = true
    parser.error_count += 1

    fmt.eprintf("Error at line %d, column %d: ", token.position.line, token.position.column)

    if token.kind == .EOF {
        fmt.eprintf("at end")
    } else if token.kind == .Invalid {
        fmt.eprintf("at '%s'", token.text)
    } else {
        fmt.eprintf("at '%s'", token.text)
    }

    fmt.eprintf(": %s\n", message)
}

/*
 * synchronize recovers from panic mode by skipping tokens until a synchronization point
 */
synchronize :: proc(parser: ^Parser) {
    parser.panic_mode = false

    // Record position to detect lack of progress
    start_pos := parser.current_token.position.offset
    start_kind := parser.current_token.kind

    // Skip tokens until we find a good synchronization point
    for parser.current_token.kind != .EOF {
        // Check if current token is a synchronization point
        if parser.current_token.kind == .Newline ||
           parser.current_token.kind == .RightBrace ||
           parser.current_token.kind == .PointingPush ||
           parser.current_token.kind == .PointingPull {
            advance_token(parser)
            return
        }

        // Current position before advancing
        current_pos := parser.current_token.position.offset
        current_kind := parser.current_token.kind

        // Try to advance
        advance_token(parser)

        // If we're not making progress (position and token kind haven't changed),
        // force advancement to break potential infinite loops
        if parser.current_token.position.offset == current_pos &&
           parser.current_token.kind == current_kind {
            // Stuck at same token - force advancement
            if parser.current_token.kind == .EOF {
                return
            }
            advance_token(parser)
            return
        }
    }
}

/*
 * parse is the main entry point for the parser
 */
parse :: proc(parser: ^Parser) -> ^Node {
    return parse_program(parser)
}

/*
 * get_rule returns the parse rule for a given token kind
 */
get_rule :: #force_inline proc(kind: Token_Kind) -> Parse_Rule {
    #partial switch kind {
    // Integer and Float Literals
    case .Integer:
        return Parse_Rule{prefix = parse_literal, infix = nil, precedence = .NONE}
    case .Float:
        return Parse_Rule{prefix = parse_literal, infix = nil, precedence = .NONE}
    case .Hexadecimal:
        return Parse_Rule{prefix = parse_literal, infix = nil, precedence = .NONE}
    case .Binary:
        return Parse_Rule{prefix = parse_literal, infix = nil, precedence = .NONE}
    case .String_Literal:
        return Parse_Rule{prefix = parse_literal, infix = nil, precedence = .NONE}

    // Identifiers and basic symbols
    case .Identifier:
        return Parse_Rule{prefix = parse_identifier, infix = nil, precedence = .NONE}
    case .LeftBrace:
        return Parse_Rule{prefix = parse_scope, infix = parse_override, precedence = .CALL}
    case .LeftParen:
        return Parse_Rule{prefix = parse_grouping, infix = nil, precedence = .NONE}
    case .RightBrace:
        return Parse_Rule{prefix = nil, infix = nil, precedence = .NONE}
    case .At:
        return Parse_Rule{prefix = parse_reference, infix = nil, precedence = .NONE}

    // Unary operators
    case .BitNot:
        return Parse_Rule{prefix = parse_unary, infix = nil, precedence = .UNARY}
    case .Minus:
        return Parse_Rule{prefix = parse_unary, infix = parse_binary, precedence = .TERM}


    // Postfix operator
    case .Execute:
    return Parse_Rule{prefix = nil, infix = parse_execute, precedence = .ASSIGNMENT}


    // Binary operators
    case .Plus:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .TERM}
    case .Asterisk:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .FACTOR}
    case .Slash:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .FACTOR}
    case .Percent:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .FACTOR}
    case .BitAnd:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .BITWISE}
    case .BitOr:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .BITWISE}
    case .BitXor:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .BITWISE}
    case .Equal:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .EQUALITY}
    case .LessThan:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .COMPARISON}
    case .GreaterThan:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .COMPARISON}
    case .LessEqual:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .COMPARISON}
    case .GreaterEqual:
        return Parse_Rule{prefix = nil, infix = parse_binary, precedence = .COMPARISON}

    // Specialized operators
    case .Colon:
        return Parse_Rule{prefix = nil, infix = parse_constraint, precedence = .CALL}

    // Assignment operators
    case .PointingPush:
        return Parse_Rule{prefix = parse_product_prefix, infix = parse_pointing_push, precedence = .ASSIGNMENT}
    case .PointingPull:
        return Parse_Rule{prefix = parse_pointing_pull_prefix, infix = parse_pointing_pull, precedence = .ASSIGNMENT}
    case .EventPush:
        return Parse_Rule{prefix = parse_event_push_prefix, infix = parse_event_push, precedence = .ASSIGNMENT}
    case .EventPull:
        return Parse_Rule{prefix = parse_event_pull_prefix, infix = parse_event_pull, precedence = .ASSIGNMENT}
    case .ResonancePush:
        return Parse_Rule{prefix = parse_resonance_push_prefix, infix = parse_resonance_push, precedence = .ASSIGNMENT}
    case .ResonancePull:
        return Parse_Rule{prefix = parse_resonance_pull_prefix, infix = parse_resonance_pull, precedence = .ASSIGNMENT}


    // Range notation
    case .DoubleDot:
        return Parse_Rule{prefix = parse_prefix_range, infix = parse_range, precedence = .RANGE}

    // Special cases
    case .Dot:
        return Parse_Rule{prefix = nil, infix = parse_property, precedence = .CALL}
    case .Question:
        return Parse_Rule{prefix = nil, infix = parse_pattern, precedence = .CALL}
    case .Ellipsis:
        return Parse_Rule{prefix = parse_expansion, infix = nil, precedence = .PRIMARY}
    }
    return Parse_Rule{} // Default empty rule
}

/*
 * parse_program parses the entire program as a sequence of statements
 */
parse_program :: proc(parser: ^Parser) -> ^Node {
    scope := Scope{}
    scope.value = make([dynamic]Node, 0, 2)

    // Keep parsing until EOF
    for parser.current_token.kind != .EOF {
        // Skip newlines between statements
        for parser.current_token.kind == .Newline {
            advance_token(parser)
        }

        if parser.current_token.kind == .EOF {
            break
        }

        if node := parse_with_recovery(parser); node != nil {
            append(&scope.value, node^)
        }
    }

    result := new(Node)
    result^ = scope
    return result
}

/*
 * parse_with_recovery attempts to parse a statement and recovers from errors
 */
parse_with_recovery :: proc(parser: ^Parser) -> ^Node {
    if parser.panic_mode {
        synchronize(parser)
    }

    node := parse_statement(parser)

    // Skip any newlines after a statement
    for parser.current_token.kind == .Newline {
        advance_token(parser)
    }

    return node
}

/*
 * parse_statement parses a single statement
 */
parse_statement :: proc(parser: ^Parser) -> ^Node {
    if parser.current_token.kind == .Newline {
        advance_token(parser)
        return nil
    }

    if parser.current_token.kind == .EOF || parser.current_token.kind == .RightBrace {
        // Acceptable empty statements — BUT force advancement
        advance_token(parser)
        return nil
    }

    expr := parse_expression(parser)

    // Defensive: ensure we’re not stuck
    if expr == nil && parser.current_token.kind == .RightBrace {
        error_at_current(parser, "Unexpected }")
        advance_token(parser)
        return nil
    }

    return expr
}

/*
 * parse_expression parses expressions using Pratt parsing
 */
parse_expression :: proc(parser: ^Parser, precedence := Precedence.ASSIGNMENT) -> ^Node {
    if parser.current_token.kind == .EOF || parser.current_token.kind == .RightBrace {
        // Handle empty expressions without error
        return nil
    }

    // Get prefix rule for current token
    rule := get_rule(parser.current_token.kind)
    if rule.prefix == nil {
        // Better error messages for common cases
        if parser.current_token.kind == .Colon {
            error_at_current(parser, "Unexpected ':' without a type constraint")
        } else if parser.current_token.kind == .PointingPush {
            error_at_current(parser, "Unexpected '->' without a name to point from")
        } else if !parser.panic_mode {
            error_at_current(parser, fmt.tprintf("Expected expression, found '%v'", parser.current_token.kind))
        }

        // Advance past the problematic token to avoid getting stuck
        advance_token(parser)
        return nil
    }

    // Remember if we're in a context where assignment is allowed
    can_assign := precedence <= .ASSIGNMENT

    // Parse the prefix expression
    left := rule.prefix(parser, can_assign)

    if left == nil {
        return nil
    }

    // Keep parsing infix expressions as long as they have higher precedence
    for precedence <= get_rule(parser.current_token.kind).precedence {
        rule = get_rule(parser.current_token.kind)
        if rule.infix == nil {
            break
        }

        left = rule.infix(parser, left, can_assign)
        if left == nil {
            return nil
        }
    }

    return left
}

/*
* Implementation of the override postfix rule
*/
parse_override :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create an override node
    override := Override {
        source = left,
        overrides = make([dynamic]Node, 0, 2),
    }

    // Consume left brace (already checked by the caller)
    advance_token(parser)

    // Parse statements inside the braces as overrides
    for parser.current_token.kind != .RightBrace && parser.current_token.kind != .EOF {
        // Skip newlines
        for parser.current_token.kind == .Newline {
            advance_token(parser)
        }

        if parser.current_token.kind == .RightBrace {
            break
        }

        // Parse statement and add to overrides
        if node := parse_statement(parser); node != nil {
            append(&override.overrides, node^)
        } else {
            // Error recovery
            synchronize(parser)

            // Check if we synchronized to the end of the overrides
            if parser.current_token.kind == .RightBrace {
                break
            }
        }

        // Skip newlines
        for parser.current_token.kind == .Newline {
            advance_token(parser)
        }
    }

    // Expect closing brace
    if !match(parser, .RightBrace) {
        error_at_current(parser, "Expected } after overrides")
        return nil
    }

    // Create and return the override node
    result := new(Node)
    result^ = override
    return result
}

/*
 * parse_execute handles postfix execution patterns like expr<[!]>
 */
parse_execute :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create execute node to hold the left expression
    execute := Execute{
      value = left,
      wrappers = make([dynamic]ExecutionWrapper, 0, 2),
    }

    // Process the execution pattern
    found_exclamation := false

    // Stack to track opening symbols for proper nesting
    stack := make([dynamic]Token_Kind, 0, 2)
    defer delete(stack)

    // Continue parsing until we have a complete execution pattern
    for {
        current := parser.current_token.kind

        if current == .Execute {
            // Add sequential execution wrapper
            append_elem(&execute.wrappers, ExecutionWrapper.Sequential)
            found_exclamation = true
            advance_token(parser)
        } else if current == .LeftParen {
            // Start of background execution
            append_elem(&execute.wrappers, ExecutionWrapper.Background)
            append_elem(&stack, Token_Kind.LeftParen)
            advance_token(parser)
        } else if current == .LessThan {
            // Start of threading execution
            append_elem(&execute.wrappers, ExecutionWrapper.Threading)
            append_elem(&stack, Token_Kind.LessThan)
            advance_token(parser)
        } else if current == .LeftBracket {
            // Start of parallel CPU execution
            append_elem(&execute.wrappers, ExecutionWrapper.Parallel_CPU)
            append_elem(&stack, Token_Kind.LeftBracket)
            advance_token(parser)
        } else if current == .BitOr {
            // Check if it's an opening or closing BitOr
            if len(stack) > 0 && stack[len(stack)-1] == Token_Kind.BitOr {
                // Closing BitOr, pop from stack
                ordered_remove(&stack, len(stack)-1)
                advance_token(parser)
            } else {
                // Opening BitOr, push to stack
                append_elem(&execute.wrappers, ExecutionWrapper.GPU)
                append_elem(&stack, Token_Kind.BitOr)
                advance_token(parser)
            }
        } else if current == .RightParen {
            // Check for corresponding opening parenthesis
            if len(stack) == 0 || stack[len(stack)-1] != Token_Kind.LeftParen {
                error_at_current(parser, "Mismatched ')' in execution pattern")
                return nil
            }

            // Pop opening parenthesis from stack
            ordered_remove(&stack, len(stack)-1)
            advance_token(parser)
        } else if current == .GreaterThan {
            // Check for corresponding opening angle bracket
            if len(stack) == 0 || stack[len(stack)-1] != Token_Kind.LessThan {
                error_at_current(parser, "Mismatched '>' in execution pattern")
                return nil
            }

            // Pop opening angle bracket from stack
            ordered_remove(&stack, len(stack)-1)
            advance_token(parser)
        } else if current == .RightBracket {
            // Check for corresponding opening square bracket
            if len(stack) == 0 || stack[len(stack)-1] != Token_Kind.LeftBracket {
                error_at_current(parser, "Mismatched ']' in execution pattern")
                return nil
            }

            // Pop opening square bracket from stack
            ordered_remove(&stack, len(stack)-1)
            advance_token(parser)
        } else {
            // No more execution pattern tokens
            break
        }

        // If stack is empty and we found an exclamation mark, we're done
        if len(stack) == 0 && found_exclamation {
            break
        }
    }

    // Ensure we found an exclamation mark
    if !found_exclamation {
        error_at_current(parser, "Execution pattern must contain '!'")
        return nil
    }

    // Create and return execute node
    result := new(Node)
    result^ = execute

    return result
}

/*
 * parse_product_prefix handles the standalone product expression (-> value)
 */
parse_product_prefix :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Consume the ->
    advance_token(parser)

    product := Product{}

    // Parse the value or handle empty product
    if parser.current_token.kind == .RightBrace || parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        // Empty product - this is valid
        product.value = nil
    } else {
        // Parse the value
        if value := parse_expression(parser); value != nil {
            product.value = value
        } else {
            // Error already reported
            return nil
        }
    }

    result := new(Node)
    result^ = product
    return result
}

/*
 * parse_literal handles literal values (numbers, strings)
 */
parse_literal :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    literal := Literal {
      value = parser.current_token.text,
    }

    #partial switch parser.current_token.kind {
    case .Integer:
        literal.kind = .Integer
    case .Float:
        literal.kind = .Float
    case .Hexadecimal:
        literal.kind = .Hexadecimal
    case .Binary:
        literal.kind = .Binary
    case .String_Literal:
        literal.kind = .String
    case:
        error_at_current(parser, "Unknown literal type")
        return nil
    }


    advance_token(parser)

    result := new(Node)
    result^ = literal
    return result
}

/*
 * parse_identifier handles identifier expressions
 */
parse_identifier :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Create identifier node
    id_node := new(Node)
    id_node^ = Identifier{name = parser.current_token.text}

    // Advance past identifier
    advance_token(parser)

    return id_node
}

/*
 * skip_newlines skips consecutive newline tokens
 */
skip_newlines :: proc(parser: ^Parser) {
    for parser.current_token.kind == .Newline {
        advance_token(parser)
    }
}

/*
 * parse_scope parses a scope block {...} - improved to handle empty scopes
 */
parse_scope :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Consume opening brace
    advance_token(parser)

    scope := Scope{value = make([dynamic]Node, 0, 2)}

    // Allow for empty scopes
    if parser.current_token.kind == .RightBrace {
        advance_token(parser)
        result := new(Node)
        result^ = scope
        return result
    }

    // Parse statements until closing brace
    for parser.current_token.kind != .RightBrace && parser.current_token.kind != .EOF {
        // Skip newlines between statements
        skip_newlines(parser)

        if parser.current_token.kind == .RightBrace {
            break
        }

        // Parse statement with error recovery
        if parser.panic_mode {
            synchronize(parser)

            // After synchronizing, check if we're at the end of the scope
            if parser.current_token.kind == .RightBrace {
                break
            }
        }

        if node := parse_statement(parser); node != nil {
            append(&scope.value, node^)
        }

        // Skip newlines after statements
        skip_newlines(parser)
    }

    // Consume closing brace
    if !match(parser, .RightBrace) {
        error_at_current(parser, "Expected '}' to close scope")
    }

    result := new(Node)
    result^ = scope
    return result
}

/*
 * parse_grouping parses grouping expressions (...)
 */
parse_grouping :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Consume opening parenthesis
    advance_token(parser)

    // Parse expression inside parentheses
    expr := parse_expression(parser)
    if expr == nil {
        // Handle empty parentheses gracefully
        if parser.current_token.kind == .RightParen {
            advance_token(parser)
            // Return an empty scope as a placeholder
            empty_scope := new(Node)
            empty_scope^ = Scope{value = make([dynamic]Node, 0, 2)}
            return empty_scope
        }

        error_at_current(parser, "Expected expression after '('")
        return nil
    }

    // Expect closing parenthesis
    if !expect_token(parser, .RightParen) {
        return nil
    }

    return expr
}

/*
 * parse_unary parses unary operators (-, ~)
 */
parse_unary :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Remember the operator kind
    operator_kind := parser.current_token.kind

    // Advance past the operator
    advance_token(parser)

    // Parse the operand
    operand := parse_expression(parser, .UNARY)
    if operand == nil {
        error_at_current(parser, "Expected expression after unary operator")
        return nil
    }

    // Create operator node
    op := Operator{right = operand}

    // Set operator kind based on token
    #partial switch operator_kind {
    case .Minus:
        op.kind = .Minus
    case .BitNot:
        op.kind = .BitNot
    case:
        error_at_current(parser, "Unexpected unary operator")
        return nil
    }

    result := new(Node)
    result^ = op
    return result
}

/*
 * parse_binary handles binary operators (+, -, *, /, etc.)
 */
parse_binary :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Remember the operator
    operator_kind := parser.current_token.kind
    rule := get_rule(operator_kind)

    // Move past the operator
    advance_token(parser)

    // Parse the right operand with higher precedence
    right := parse_expression(parser, Precedence(int(rule.precedence) + 1))
    if right == nil {
        error_at_current(parser, "Expected expression after binary operator")
        return nil
    }

    // Create operator node
    op := Operator{left = left, right = right}

    // Set operator type
    #partial switch operator_kind {
    case .Plus:          op.kind = .Plus
    case .Minus:         op.kind = .Minus
    case .Asterisk:      op.kind = .Mult
    case .Slash:         op.kind = .Div
    case .Percent:       op.kind = .Mod
    case .BitAnd:        op.kind = .BitAnd
    case .BitOr:         op.kind = .BitOr
    case .BitXor:        op.kind = .BitXor
    case .Equal:         op.kind = .Equal
    case .LessThan:      op.kind = .LessThan
    case .GreaterThan:   op.kind = .GreaterThan
    case .LessEqual:     op.kind = .LessEqual
    case .GreaterEqual:  op.kind = .GreaterEqual
    case:
        error_at_current(parser, fmt.tprintf("Unhandled binary operator type: %v", operator_kind))
        return nil
    }

    result := new(Node)
    result^ = op
    return result
}

/*
 * parse_property handles property access (obj.prop)
 */
parse_property :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Consume the dot
    advance_token(parser)

    // Expect identifier for property name
    if parser.current_token.kind != .Identifier {
        error_at_current(parser, "Expected property name after '.'")
        return nil
    }

    // Get property name
    prop_name := parser.current_token.text
    advance_token(parser)

    // Create property node
    property := Property{source = left}

    // Create property identifier
    prop_id := new(Node)
    prop_id^ = Identifier{name = prop_name}
    property.property = prop_id

    // Return property node
    result := new(Node)
    result^ = property
    return result
}

/*
 * parse_pointing_push handles pointing operator (a -> b)
 */
parse_pointing_push :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create pointing node
    pointing := Pointing{name = left}

    // Consume ->
    advance_token(parser)

    // Handle the case where there's nothing after the arrow
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        pointing.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        pointing.value = value
    }

    result := new(Node)
    result^ = pointing
    return result
}
/*
 * parse_pointing_pull_prefix handles prefix pointing pull operator (<- value)
 */
parse_pointing_pull_prefix :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Create pointing pull node
    pointing_pull := PointingPull{}

    // Consume <-
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        pointing_pull.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        pointing_pull.value = value
    }

    result := new(Node)
    result^ = pointing_pull
    return result
}

/*
 * parse_pointing_pull handles infix pointing pull operator (a <- b)
 */
parse_pointing_pull :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create pointing pull node
    pointing_pull := PointingPull{name = left}

    // Consume <-
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        pointing_pull.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        pointing_pull.value = value
    }

    result := new(Node)
    result^ = pointing_pull
    return result
}

/*
 * parse_event_push_prefix handles prefix event push (>- value)
 */
parse_event_push_prefix :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Create event push node
    event_push := EventPush{}

    // Consume >-
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        event_push.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        event_push.value = value
    }

    result := new(Node)
    result^ = event_push
    return result
}

/*
 * parse_event_push handles event push (a >- b)
 */
parse_event_push :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create event push node
    event_push := EventPush{name = left}

    // Consume >-
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        event_push.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        event_push.value = value
    }

    result := new(Node)
    result^ = event_push
    return result
}

/*
 * parse_event_pull_prefix handles prefix event pull (-< value)
 */
parse_event_pull_prefix :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Create event pull node
    event_pull := EventPull{}

    // Consume -<
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        event_pull.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        event_pull.value = value
    }

    result := new(Node)
    result^ = event_pull
    return result
}

/*
 * parse_event_pull handles event pull (a -< b)
 */
parse_event_pull :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create event pull node
    event_pull := EventPull{name = left}

    // Consume -<
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        event_pull.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        event_pull.value = value
    }

    result := new(Node)
    result^ = event_pull
    return result
}

/*
 * parse_resonance_push_prefix handles prefix resonance push (>>- value)
 */
parse_resonance_push_prefix :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Create resonance push node
    resonance_push := ResonancePush{}

    // Consume >>-
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        resonance_push.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        resonance_push.value = value
    }

    result := new(Node)
    result^ = resonance_push
    return result
}

/*
 * parse_resonance_push handles resonance push (a >>- b)
 */
parse_resonance_push :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create resonance push node
    resonance_push := ResonancePush{name = left}

    // Consume >>-
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        resonance_push.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        resonance_push.value = value
    }

    result := new(Node)
    result^ = resonance_push
    return result
}

/*
 * parse_resonance_pull_prefix handles prefix resonance pull (-<< value)
 */
parse_resonance_pull_prefix :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Create resonance pull node
    resonance_pull := ResonancePull{}

    // Consume -<<
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        resonance_pull.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        resonance_pull.value = value
    }

    result := new(Node)
    result^ = resonance_pull
    return result
}

/*
 * parse_resonance_pull handles resonance pull (a -<< b)
 */
parse_resonance_pull :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create resonance pull node
    resonance_pull := ResonancePull{name = left}

    // Consume -<<
    advance_token(parser)

    // Parse value or handle empty value
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        resonance_pull.value = nil
    } else {
        // Parse value
        value := parse_expression(parser)
        resonance_pull.value = value
    }

    result := new(Node)
    result^ = resonance_pull
    return result
}

/*
 * parse_prefix_range handles prefix range (..5)
 */
parse_prefix_range :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Consume ..
    advance_token(parser)

    // Parse end value or handle empty value
    end: ^Node = nil
    if !(parser.current_token.kind == .RightBrace ||
         parser.current_token.kind == .EOF ||
         parser.current_token.kind == .Newline) {
        end = parse_expression(parser, .RANGE)
    }

    // Create range node
    range := Range{end = end}

    result := new(Node)
    result^ = range
    return result
}

/*
 * parse_range handles range expression (a..b)
 */
parse_range :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Consume ..
    advance_token(parser)

    // Create range node
    range := Range{start = left}

// Check if there's an end expression
    if parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline ||
       parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .RightParen {
        // This is a postfix range (a..)
        range.end = nil
    } else {
        // This is a full range (a..b)
        end := parse_expression(parser, .RANGE)
        range.end = end
    }

    result := new(Node)
    result^ = range
    return result
}

/*
 * parse_constraint handles constraint expressions (Type: value)
 */
parse_constraint :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Check if left is nil before proceeding
    if left == nil {
        error_at_current(parser, "Constraint requires a type before ':'")
        advance_token(parser) // Skip the colon
        return nil
    }

    // Create constraint
    constraint := Constraint{constraint=left}

    // Move past :
    advance_token(parser)

    // Parse value if present, otherwise it's an empty constraint
    if parser.current_token.kind == .RightBrace ||
       parser.current_token.kind == .EOF ||
       parser.current_token.kind == .Newline {
        // Empty constraint (Type:)
        constraint.value = nil
    } else if is_expression_start(parser.current_token.kind) {
        // Constraint with value (Type: value)
        // Use a different precedence level to ensure constraints bind tighter than pointings
        if value := parse_expression(parser, .CALL); value != nil {
            constraint.value = value
        }
    } else {
        // No value, but it's allowed
        constraint.value = nil
    }

    result := new(Node)
    result^ = constraint
    return result
}

/*
 * parse_pattern handles pattern match (target ? {...})
 */
parse_pattern :: proc(parser: ^Parser, left: ^Node, can_assign: bool) -> ^Node {
    // Create pattern node with target
    pattern := Pattern{
      target = left,
      value = make([dynamic]Branch, 0, 2),
    }

    // Consume ? token
    advance_token(parser)

    // Skip newlines between ? and {
    skip_newlines(parser)

    // Expect opening brace
    if !match(parser, .LeftBrace) {
        error_at_current(parser, "Expected { after ? in pattern")
        return nil
    }

    // Skip newlines after opening brace
    skip_newlines(parser)

    // Handle empty pattern block
    for parser.current_token.kind != .RightBrace && parser.current_token.kind != .EOF {
      node := parse_branch(parser)
      if(node!=nil) {
        append(&pattern.value, node^)
      }
      skip_newlines(parser)
    }

    if !match(parser, .RightBrace) {
        error_at_current(parser, "Expected } after pattern branches")
        return nil
    }

    result := new(Node)
    result^ = pattern
    return result
}

/*
 * parse_branch parses a single branch in a pattern match
 */
parse_branch :: proc(parser: ^Parser) -> ^Branch {
    // Don't try to parse a branch if we're at a token that can't start an expression
    if !is_expression_start(parser.current_token.kind) {
        advance_token(parser)  // Skip problematic token
        return nil
    }

    // Create branch
    branch := new(Branch)

    // Parse pattern expression (the left side of ->)
    if pattern := parse_expression(parser, ); pattern != nil {
      #partial switch parse in pattern {

      case Pointing:
        branch.source = parse.name
        branch.product = parse.value
        return branch
      case Constraint:
      if parse.value != nil {
          #partial switch value in parse.value {
          case Product:
            branch.product = value.value
            constraint := Constraint{
              constraint = parse.constraint,
            }
            result := new(Node)
            result^ = constraint
            branch.source = result
            return branch
          }
        }
      }
      branch.source = pattern
    } else {
        // Error already reported
        return nil
    }

    // Expect pointing arrow for pattern branches
    if !match(parser, .PointingPush) {
        error_at_current(parser, "Expected -> in pattern branch")
        return nil
    }

    // Parse the result expression (right side of ->)
    if value := parse_expression(parser); value != nil {
        // Create product node to hold the result
        product := Product{value = value}

        product_node := new(Node)
        product_node^ = product
        branch.product = product_node
    } else {
        // Handle case where there's no expression after ->
        product := Product{}

        product_node := new(Node)
        product_node^ = product
        branch.product = product_node
    }

    return branch
}

/*
 * parse_product parses a product expression (-> value)
 * This is for standalone -> expressions
 */
parse_product :: proc(parser: ^Parser) -> ^Node {
    return parse_product_prefix(parser, false)
}

/*
 * parse_expansion parses a content expansion (...expr)
 */
parse_expansion :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Consume ellipsis token
    advance_token(parser)

    // Get the expression that follows with appropriate precedence
    // Use the UNARY precedence to ensure we get the entire expression
    target := parse_expression(parser, .UNARY)
    if target == nil {
        error_at_current(parser, "Expected expression after ...")
        return nil
    }

    // Create the expansion node with the target
    expand := Expand{target=target}

    result := new(Node)
    result^ = expand
    return result
}

/*
 * parse_reference parses a file system reference (@module.lib)
 */
parse_reference :: proc(parser: ^Parser, can_assign: bool) -> ^Node {
    // Consume @
    advance_token(parser)

    // Check for identifier after @
    if parser.current_token.kind != .Identifier {
        error_at_current(parser, "Expected identifier after @")
        return nil
    }

    // Create FileSystem node
    fs_node := FileSystem{}

    // Parse first identifier
    id_name := parser.current_token.text
    advance_token(parser)

    // Create initial identifier node
    ident_node := new(Node)
    ident_node^ = Identifier{
        name = id_name,
    }

    // If no property access follows, return basic FileSystem node
    if parser.current_token.kind != .Dot {
        fs_node.target = ident_node
        result := new(Node)
        result^ = fs_node
        return result
    }

    // Handle property chain (lib.geometry.Plane)
    current_node := ident_node

    // Process all dots and identifiers in the chain
    for parser.current_token.kind == .Dot {
        // Consume dot
        advance_token(parser)

        // Expect identifier
        if parser.current_token.kind != .Identifier {
            error_at_current(parser, "Expected identifier after dot")
            return nil
        }

        // Get property name
        property_name := parser.current_token.text
        advance_token(parser)

        // Create property node
        property := Property{source = current_node}

        // Create property identifier
        prop_ident := new(Node)
        prop_ident^ = Identifier{
            name = property_name,
        }
        property.property = prop_ident

        // Update current node to this property
        property_node := new(Node)
        property_node^ = property
        current_node = property_node
    }

    // Set final property chain as FileSystem target
    fs_node.target = current_node

    result := new(Node)
    result^ = fs_node
    return result
}

// ===========================================================================
// SECTION 4: UTILITY FUNCTIONS
// ===========================================================================

/*
 * Helper function to check if a token can start an execution pattern
 */
is_execution_pattern_start :: proc(kind: Token_Kind) -> bool {
    return kind == .Execute || kind == .LeftParen || kind == .LessThan ||
           kind == .LeftBracket || kind == .BitOr
}

/*
 * is_expression_start checks if a token can start an expression
 */
is_expression_start :: proc(kind: Token_Kind) -> bool {
    return(
        kind == .Identifier ||
        kind == .Integer ||
        kind == .Float ||
        kind == .String_Literal ||
        kind == .Hexadecimal ||
        kind == .Binary ||
        kind == .LeftBrace ||
        kind == .LeftParen ||
        kind == .At ||
        kind == .BitNot ||
        kind == .Minus ||
        kind == .Execute ||
        kind == .PointingPull ||
        kind == .EventPush ||
        kind == .EventPull ||
        kind == .ResonancePush ||
        kind == .ResonancePull ||
        kind == .DoubleDot ||
        kind == .Question ||
        kind == .Ellipsis ||
        kind == .PointingPush
    )
}

/*
 * is_operator checks if a token is a binary operator
 */
is_operator :: proc(kind: Token_Kind) -> bool {
    return(
        kind == .Equal ||
        kind == .LessThan ||
        kind == .GreaterThan ||
        kind == .LessEqual ||
        kind == .GreaterEqual ||
        kind == .Plus ||
        kind == .Minus ||
        kind == .Asterisk ||
        kind == .Slash ||
        kind == .Percent ||
        kind == .BitAnd ||
        kind == .BitOr ||
        kind == .BitXor ||
        kind == .BitNot ||
        kind == .PointingPush ||
        kind == .PointingPull ||
        kind == .EventPush ||
        kind == .EventPull ||
        kind == .ResonancePush ||
        kind == .ResonancePull ||
        kind == .DoubleDot
    )
}

/*
 * is_execution_modifier checks if a token is an execution modifier
 */
is_execution_modifier :: proc(kind: Token_Kind) -> bool {
    return kind == .Execute || kind == .LeftBrace || kind == .LeftParen
}

/*
 * is_digit checks if a character is a digit
 * Inlined for speed
 */
is_digit :: #force_inline proc(c: u8) -> bool {
    return c >= '0' && c <= '9'
}

/*
 * is_hex_digit checks if a character is a hexadecimal digit
 * Inlined for speed
 */
is_hex_digit :: #force_inline proc(c: u8) -> bool {
    return is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
}

/*
 * is_alpha checks if a character is an alphabetic character
 * Inlined for speed
 */
is_alpha :: #force_inline proc(c: u8) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

/*
 * is_alnum checks if a character is alphanumeric or underscore
 * Inlined for speed
 */
is_alnum :: #force_inline proc(c: u8) -> bool {
    return is_digit(c) || is_alpha(c) || c == '_'
}

/*
 * is_space checks if a character is a whitespace character
 * Inlined for speed
 */
is_space :: #force_inline proc(c: u8) -> bool {
    return c == ' ' || c == '\t' || c == '\r'
}

/*
 * skip_whitespace advances the lexer past any whitespace characters
 * but preserves newline tokens for statement separation
 * Optimized with a direct tight loop
 */
skip_whitespace :: #force_inline proc(l: ^Lexer) {
    for l.position.offset < l.source_len {
        c := l.source[l.position.offset]
        if !is_space(c) {
            break
        }
        advance_position(l)
    }
}

// ===========================================================================
// SECTION 5: DEBUG UTILITIES
// ===========================================================================

/*
 * debug_print_tokens prints a sequence of upcoming tokens for debugging
 */
debug_print_tokens :: proc(parser: ^Parser, count: int) {
    fmt.println("\n=== TOKEN STREAM ===")

    // Store original parser state
    orig_current := parser.current_token
    orig_peek := parser.peek_token

    // Print current and peek tokens
    fmt.printf("Current: %v '%s' at line %d, column %d\n",
               parser.current_token.kind,
               parser.current_token.text,
               parser.current_token.position.line,
               parser.current_token.position.column)

    fmt.printf("Peek: %v '%s' at line %d, column %d\n",
               parser.peek_token.kind,
               parser.peek_token.text,
               parser.peek_token.position.line,
               parser.peek_token.position.column)

    // Print a few tokens ahead
    fmt.println("\nUpcoming tokens:")

    // Create a temporary lexer and parser to scan ahead
    temp_lexer := parser.lexer^ // Make a copy of the lexer
    temp_parser: Parser
    init_parser(&temp_parser, &temp_lexer)

    // Advance to match the current state
    for temp_parser.current_token.position.offset < orig_current.position.offset &&
        temp_parser.current_token.kind != .EOF {
        advance_token(&temp_parser)
    }

    // Print the next 'count' tokens
    for i := 0; i < count && temp_parser.current_token.kind != .EOF; i += 1 {
        fmt.printf(
            "%d: %v '%s' at line %d, column %d\n",
            i + 1,
            temp_parser.current_token.kind,
            temp_parser.current_token.text,
            temp_parser.current_token.position.line,
            temp_parser.current_token.position.column
        )
        advance_token(&temp_parser)
    }

    fmt.println("=== END TOKEN STREAM ===\n")
}

/*
 * print_ast prints the AST with indentation for readability
 */
print_ast :: proc(node: ^Node, indent: int) {
    if node == nil {
        return
    }

    indent_str := strings.repeat(" ", indent)

    #partial switch n in node^ {
    case Pointing:
        fmt.printf("%sPointing ->\n", indent_str)
        if n.name != nil {
            fmt.printf("%s  Name:\n", indent_str)
            print_ast(n.name, indent + 4)
        }
        if n.value != nil {
            fmt.printf("%s  Value:\n", indent_str)
            print_ast(n.value, indent + 4)
        }

    case PointingPull:
        fmt.printf("%sPointingPull <-\n", indent_str)
        if n.name != nil {
            fmt.printf("%s  Name:\n", indent_str)
            print_ast(n.name, indent + 4)
        } else {
            fmt.printf("%s  Name: anonymous\n", indent_str)
        }
        if n.value != nil {
            fmt.printf("%s  Value:\n", indent_str)
            print_ast(n.value, indent + 4)
        }

    case EventPush:
        fmt.printf("%sEventPush >-\n", indent_str)
        if n.name != nil {
            fmt.printf("%s  Name:\n", indent_str)
            print_ast(n.name, indent + 4)
        } else {
            fmt.printf("%s  Name: anonymous\n", indent_str)
        }
        if n.value != nil {
            fmt.printf("%s  Value:\n", indent_str)
            print_ast(n.value, indent + 4)
        }

    case EventPull:
        fmt.printf("%sEventPull -<\n", indent_str)
        if n.name != nil {
            fmt.printf("%s  Name:\n", indent_str)
            print_ast(n.name, indent + 4)
        } else {
            fmt.printf("%s  Name: anonymous\n", indent_str)
        }
        if n.value != nil {
            fmt.printf("%s  Value:\n", indent_str)
            print_ast(n.value, indent + 4)
        }

    case ResonancePush:
        fmt.printf("%sResonancePush >>-\n", indent_str)
        if n.name != nil {
            fmt.printf("%s  Name:\n", indent_str)
            print_ast(n.name, indent + 4)
        } else {
            fmt.printf("%s  Name: anonymous\n", indent_str)
        }
        if n.value != nil {
            fmt.printf("%s  Value:\n", indent_str)
            print_ast(n.value, indent + 4)
        }

    case ResonancePull:
        fmt.printf("%sResonancePull -<<\n", indent_str)
        if n.name != nil {
            fmt.printf("%s  Name:\n", indent_str)
            print_ast(n.name, indent + 4)
        } else {
            fmt.printf("%s  Name: anonymous\n", indent_str)
        }
        if n.value != nil {
            fmt.printf("%s  Value:\n", indent_str)
            print_ast(n.value, indent + 4)
        }

    case Identifier:
        fmt.printf("%sIdentifier: %s\n", indent_str, n.name)

    case Scope:
        fmt.printf("%sScope\n", indent_str)
        for i := 0; i < len(n.value); i += 1 {
            entry_node := new(Node)
            entry_node^ = n.value[i]
            print_ast(entry_node, indent + 2)
        }

    case Override:
        fmt.printf("%sOverride\n", indent_str)
        if n.source != nil {
            fmt.printf("%s  Source:\n", indent_str)
            print_ast(n.source, indent + 4)
            fmt.printf("%s  Overrides:\n", indent_str)
            for i := 0; i < len(n.overrides); i += 1 {
                override_node := new(Node)
                override_node^ = n.overrides[i]
                print_ast(override_node, indent + 4)
            }
        }

    case Property:
        fmt.printf("%sProperty\n", indent_str)
        if n.source != nil {
            fmt.printf("%s  Source:\n", indent_str)
            print_ast(n.source, indent + 4)
        }
        if n.property != nil {
            fmt.printf("%s  Property:\n", indent_str)
            print_ast(n.property, indent + 4)
        }

    case Expand:
        fmt.printf("%sExpand\n", indent_str)
        if n.target != nil {
            fmt.printf("%s  Target:\n", indent_str)
            print_ast(n.target, indent + 4)
        }

    case FileSystem:
        fmt.printf("%sFileSystem\n", indent_str)
        if n.target != nil {
            fmt.printf("%s  Target:\n", indent_str)
            print_ast(n.target, indent + 4)
        }

    case Product:
        fmt.printf("%sProduct ->\n", indent_str)
        if n.value != nil {
            print_ast(n.value, indent + 2)
        }

    case Pattern:
        fmt.printf("%sPattern ?\n", indent_str)
        if n.target != nil {
            fmt.printf("%s  Target:\n", indent_str)
            print_ast(n.target, indent + 4)
        } else {
            fmt.printf("%s  Target: implicit\n", indent_str)
        }
        fmt.printf("%s  Branches\n", indent_str)
        for i := 0; i < len(n.value); i += 1 {
            branch := n.value[i]
            fmt.printf("%s    Branch:\n", indent_str)
            if branch.source != nil {
                fmt.printf("%s      Pattern:\n", indent_str)
                print_ast(branch.source, indent + 8)
            }
            if branch.product != nil {
                fmt.printf("%s      Match:\n", indent_str)
                print_ast(branch.product, indent + 8)
            }
        }

    case Constraint:
        fmt.printf("%sConstraint:\n", indent_str)
        print_ast(n.constraint, indent + 2)
        if n.value != nil {
            fmt.printf("%s  Value:\n", indent_str)
            print_ast(n.value, indent + 4)
        } else {
            fmt.printf("%s  Value: none\n", indent_str)
        }

    case Operator:
        fmt.printf("%sOperator '%v'\n", indent_str, n.kind)
        if n.left != nil {
            fmt.printf("%s  Left:\n", indent_str)
            print_ast(n.left, indent + 4)
        } else {
            fmt.printf("%s  Left: none (unary operator)\n", indent_str)
        }
        if n.right != nil {
            fmt.printf("%s  Right:\n", indent_str)
            print_ast(n.right, indent + 4)
        }

    case Execute:
        // Build the pattern string with proper nesting
        pattern := ""

        // Count each wrapper type
        sequential_count := 0
        threading_open := 0
        parallel_open := 0
        background_open := 0
        gpu_open := 0

        // Count the wrappers
        for wrapper in n.wrappers {
            #partial switch wrapper {
            case .Sequential:
                sequential_count += 1
            case .Threading:
                threading_open += 1
            case .Parallel_CPU:
                parallel_open += 1
            case .Background:
                background_open += 1
            case .GPU:
                gpu_open += 1
            }
        }

        // Build pattern
        for i := 0; i < threading_open; i += 1 { pattern = strings.concatenate({pattern, "<"}) }
        for i := 0; i < parallel_open; i += 1 { pattern = strings.concatenate({pattern, "["}) }
        for i := 0; i < background_open; i += 1 { pattern = strings.concatenate({pattern, "("}) }
        for i := 0; i < gpu_open; i += 1 { pattern = strings.concatenate({pattern, "|"}) }

        // Add the exclamation mark
        pattern = strings.concatenate({pattern, "!"})

        // Add closing symbols
        for i := 0; i < gpu_open; i += 1 { pattern = strings.concatenate({pattern, "|"}) }
        for i := 0; i < background_open; i += 1 { pattern = strings.concatenate({pattern, ")"}) }
        for i := 0; i < parallel_open; i += 1 { pattern = strings.concatenate({pattern, "]"}) }
        for i := 0; i < threading_open; i += 1 { pattern = strings.concatenate({pattern, ">"}) }

        fmt.printf("%sExecute %s\n", indent_str, pattern)
        if n.value != nil {
            print_ast(n.value, indent + 2)
        }

    case Literal:
        fmt.printf("%sLiteral (%v): %s\n", indent_str, n.kind, n.value)

    case Range:
        fmt.printf("%sRange\n", indent_str)
        if n.start != nil {
            fmt.printf("%s  Start:\n", indent_str)
            print_ast(n.start, indent + 4)
        } else {
            fmt.printf("%s  Start: none (prefix range)\n", indent_str)
        }
        if n.end != nil {
            fmt.printf("%s  End:\n", indent_str)
            print_ast(n.end, indent + 4)
        } else {
            fmt.printf("%s  End: none (postfix range)\n", indent_str)
        }

    case:
        fmt.printf("%sUnknown node type\n", indent_str)
    }
}

// ===========================================================================
// SECTION 6: MAIN EXECUTION
// ===========================================================================

/*
 * parse_file initializes a parser with the given lexer and returns the parsed AST
 */
parse_file :: proc(lexer: ^Lexer) -> (^Node, bool) {
    parser: Parser
    init_parser(&parser, lexer)

    // Parse the program
    ast := parse_program(&parser)

    // Return the AST and success status
    return ast, !parser.had_error
}


/*
 * main is the entry point of the compiler
 * It reads a file, parses it, and prints the resulting AST
 */
main :: proc() {
    arena : vmem.Arena
    CHUNK_SIZE :: 8 * 1024 * 1024
    err := vmem.arena_init_growing(&arena, CHUNK_SIZE)
    if(err != nil) {
      panic("Cannot init arena")
    }
    arena_allocator := vmem.arena_allocator(&arena)

    if len(os.args) < 2 {
        fmt.println("Usage: parser <filename> [--debug]")
        os.exit(1)
    }

    filename := os.args[1]
    debug_mode := len(os.args) > 2 && os.args[2] == "--debug"

    source, ok := os.read_entire_file(filename)
    if !ok {
        fmt.printf("Error: Could not read file '%s'\n", filename)
        os.exit(1)
    }
    defer delete(source)

    context.allocator = arena_allocator

    // Initialize lexer
    lexer: Lexer
    init_lexer(&lexer, string(source))

    fmt.println("Parsing file:", filename)

    // Parse the file
    ast, success := parse_file(&lexer)
    if ast == nil {
        fmt.println("Parsing failed completely!")
        os.exit(1)
    }

    if !success {
        fmt.println("Parsing completed with errors!")
    } else {
        fmt.println("Successfully parsed file!")
    }

    // Print AST
    print_ast(ast, 0)

    // If in debug mode, print a summary
    if debug_mode {
        fmt.println("\n=== DEBUG SUMMARY ===")

        // Count different node types in the AST
        scope, ok := ast^.(Scope)
        if ok {
            fmt.printf("Top level statements: %d\n", len(scope.value))
        }

        // This could include more debug info like node counts by type, etc.
        fmt.println("=== END DEBUG SUMMARY ===")
    }

    free_all(arena_allocator)
}
