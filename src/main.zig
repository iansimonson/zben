const std = @import("std");
const log = std.log.scoped(.zben);
const testing = std.testing;

// Parser section
pub const BencodeTypes = enum {
    String,
    Integer,
    List,
    Dictionary,
};

const Dictionary = std.StringHashMap(Value);
const List = std.ArrayList(Value);

pub const Value = union(enum) {
    Empty: void,
    Integer: i64,
    String: []const u8,
    List: List,
    Dictionary: Dictionary,

    pub fn make_emtpy() Value {
        return .{
            .Empty = .{},
        };
    }

    pub fn make_int(v: i64) Value {
        return .{
            .Integer = v,
        };
    }

    pub fn make_string(s: []const u8) Value {
        return .{
            .String = s,
        };
    }

    pub fn make_list(allocator: *std.mem.Allocator) Value {
        return .{
            .List = List.init(allocator),
        };
    }

    pub fn make_dict(allocator: *std.mem.Allocator) Value {
        return .{
            .Dictionary = Dictionary.init(allocator),
        };
    }

    fn maybe_deinit(self: *@This()) void {
        switch (self.*) {
            .Empty, .Integer, .String => {},
            .List => |*l| l.deinit(),
            .Dictionary => |*d| d.deinit(),
        }
    }

    pub fn set_empty(self: *@This()) void {
        maybe_deinit(self);
        self.* = Value.make_emtpy();
    }

    pub fn set_int(self: *@This(), v: i64) void {
        maybe_deinit(self);
        self.* = make_int(v);
    }

    pub fn set_string(self: *@This(), s: []const u8) void {
        maybe_deinit(self);
        self.* = make_string(s);
    }

    pub fn set_list(self: *@This(), alloc: *std.mem.Allocator) !void {
        maybe_deinit(self);
        self.* = make_list(alloc);
    }

    pub fn set_dict(self: *@This(), alloc: *std.mem.Allocator) !void {
        maybe_deinit(self);
        self.* = make_dict(alloc);
    }
};

pub const BencodeTree = struct {
    arena: std.heap.ArenaAllocator,
    root: Value,

    pub fn init(allocator: *std.mem.Allocator) BencodeTree {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .root = Value.make_emtpy(),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.arena.deinit();
    }
};

/// Parses a bencode string into a BencodeTree
/// or returns a parsing error.
/// This parser allocates memory though it will
/// place the entire BencodeTree into an arena
/// so the tree can be cleaned up properly.
pub const Parser = struct {
    parser: ParserImpl,

    /// Construct a Parser with a backing allocator
    pub fn init(allocator: *std.mem.Allocator) @This() {
        return .{
            .parser = ParserImpl.init(allocator),
        };
    }

    /// Construct a parser with a backing allocator
    /// and the string to be parsed
    pub fn initWithData(allocator: *std.mem.Allocator, data: []const u8) @This() {
        return .{
            .parser = ParserImpl.initWithData(allocator, data),
        };
    }

    /// Release any resources acquired by the Parser
    pub fn deinit(self: *@This()) void {
        self.parser.deinit();
    }

    /// Resets the parser state to init and the string to be parsed
    pub fn reset(self: *@This(), data: []const u8) void {
        self.parser.reset(data);
    }

    /// Attempt to parse the string
    pub fn parse(self: *@This()) !BencodeTree {
        return self.parser.parse();
    }
};

pub const BencodeTokens = enum {
    dictionary_begin,
    list_begin,
    integer_begin,
    integer,
    string_length,
    string_split,
    string,
    end,
    EOF,
};

pub const Token = struct {
    token_type: BencodeTokens,
    data: []const u8,
};

/// This tokenizer requires no memory allocation and does *not*
/// take ownership of the given data
/// All returned tokens are slices over the underlying data so users
/// need to ensure the data won't go out of scope
/// TODO: Make this work with streams rather than slices
pub const Lexer = struct {
    lexer: LexerImpl,

    pub fn init() @This() {
        return .{
            .lexer = LexerImpl.init(),
        };
    }

    pub fn initWithData(data: []const u8) @This() {
        return .{
            .lexer = LexerImpl.initWithData(data),
        };
    }

    pub fn reset(self: *@This(), data: []const u8) void {
        self.lexer.reset(data);
    }

    pub fn next(self: *@This()) ?Token {
        return self.lexer.next();
    }
};

////////////////////////////////////////////////
// Implementations below ///////////////////////
////////////////////////////////////////////////

const ParserImpl = struct {
    allocator: *std.mem.Allocator,
    lexer: Lexer,
    state: State,
    object_stack: std.ArrayList(ObjectType),
    value_stack: std.ArrayList(*Value),

    const State = enum {
        start,
        dict_key,
        dict_value,
        list_value,
        valid,
    };

    /// Add to the stack since "end" is generic
    const ObjectType = enum {
        dictionary,
        list,
        integer,
    };

    pub fn init(allocator: *std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .lexer = Lexer.init(),
            .state = .start,
            .object_stack = std.ArrayList(ObjectType).init(allocator),
            .value_stack = std.ArrayList(*Value).init(allocator),
        };
    }

    pub fn initWithData(allocator: *std.mem.Allocator, data: []const u8) @This() {
        return .{
            .allocator = allocator,
            .lexer = Lexer.initWithData(data),
            .state = .start,
            .object_stack = std.ArrayList(ObjectType).init(allocator),
            .value_stack = std.ArrayList(*Value).init(allocator),
        };
    }

    pub fn reset(self: *@This(), data: []const u8) void {
        self.state = .start;
        self.object_stack.resize(0);
        self.value_stack.resize(0);
        self.lexer.reset(data);
    }

    pub fn deinit(self: *@This()) void {
        self.object_stack.deinit();
        self.value_stack.deinit();
    }

    /// Turns the given data into a Tree of items
    pub fn parse(self: *@This()) !BencodeTree {
        var tree = BencodeTree{
            .arena = std.heap.ArenaAllocator.init(self.allocator),
            .root = Value.make_emtpy(),
        };
        const alloc = &tree.arena.allocator;
        errdefer tree.arena.deinit();

        var current_dict_key: []const u8 = "";

        while (self.lexer.next()) |token| {
            log.debug("Parsing token: {}", .{token});
            switch (self.state) {
                .start => {
                    switch (token.token_type) {
                        .dictionary_begin => {
                            try self.object_stack.append(.dictionary);
                            try tree.root.set_dict(alloc);
                            try self.value_stack.append(&tree.root);
                            self.state = .dict_key;
                        },
                        .list_begin => {
                            try self.object_stack.append(.list);
                            try tree.root.set_list(alloc);
                            try self.value_stack.append(&tree.root);
                            self.state = .list_value;
                        },
                        .integer_begin => {
                            const value: i64 = try parseInteger(token, &self.lexer);
                            tree.root.set_int(value);
                            self.state = .valid;
                        },
                        .string_length => {
                            const str = try parseString(token, &self.lexer);
                            tree.root.set_string(try alloc.dupe(u8, str));
                            self.state = .valid;
                        },
                        else => {
                            return error.malformed_data;
                        },
                    }
                },
                .dict_key => {
                    switch (token.token_type) {
                        .string_length => {
                            const cur_key = try parseString(token, &self.lexer);
                            self.state = .dict_value;
                            current_dict_key = cur_key;
                        },
                        .end => {
                            if (stackTop(self.object_stack.items).* != .dictionary) {
                                return error.parse_error;
                            }
                            _ = self.object_stack.pop();
                            _ = self.value_stack.pop();

                            if (self.object_stack.items.len == 0) {
                                std.debug.assert(self.value_stack.items.len == 0);
                                self.state = .valid;
                            } else {
                                switch (stackTop(self.object_stack.items).*) {
                                    .dictionary => self.state = .dict_key,
                                    .list => self.state = .list_value,
                                    else => {
                                        return error.parse_error;
                                    },
                                }
                            }
                        },
                        else => return error.dictionary_key_not_string,
                    }
                },
                .dict_value => {
                    switch (token.token_type) {
                        .list_begin => {
                            var dict_node = stackTop(self.value_stack.items).*;
                            var entry = try dict_node.Dictionary.getOrPutValue(current_dict_key, Value.make_list(alloc));
                            try self.object_stack.append(.list);
                            try self.value_stack.append(&entry.value);
                            self.state = .list_value;
                        },
                        .dictionary_begin => {
                            var dict_node = stackTop(self.value_stack.items).*;
                            var entry = try dict_node.Dictionary.getOrPutValue(current_dict_key, Value.make_dict(alloc));
                            try self.object_stack.append(.dictionary);
                            try self.value_stack.append(&entry.value);
                            self.state = .dict_key;
                        },
                        .string_length => {
                            const str = try parseString(token, &self.lexer);
                            var dict_node = stackTop(self.value_stack.items).*;
                            var entry = try dict_node.Dictionary.getOrPutValue(current_dict_key, Value.make_string(try alloc.dupe(u8, str)));
                            self.state = .dict_key;
                        },
                        .integer_begin => {
                            const value: i64 = try parseInteger(token, &self.lexer);
                            var dict_node = stackTop(self.value_stack.items).*;
                            var entry = try dict_node.Dictionary.getOrPutValue(current_dict_key, Value.make_int(value));
                            self.state = .dict_key;
                        },
                        else => return error.parse_error,
                    }
                },
                .list_value => {
                    switch (token.token_type) {
                        .end => {
                            if (stackTop(self.object_stack.items).* != .list) {
                                return error.parse_error;
                            }
                            _ = self.object_stack.pop();
                            _ = self.value_stack.pop();

                            if (self.object_stack.items.len == 0) {
                                std.debug.assert(self.value_stack.items.len == 0);
                                self.state = .valid;
                            } else {
                                switch (stackTop(self.object_stack.items).*) {
                                    .dictionary => self.state = .dict_key,
                                    .list => self.state = .list_value,
                                    else => {
                                        return error.parse_error;
                                    },
                                }
                            }
                        },
                        .integer_begin => {
                            const value: i64 = try parseInteger(token, &self.lexer);
                            var list_node = stackTop(self.value_stack.items).*;
                            try list_node.List.append(Value.make_int(value));
                        },
                        .string_length => {
                            const str = try parseString(token, &self.lexer);
                            var list_node = stackTop(self.value_stack.items).*;
                            try list_node.List.append(Value.make_string(try alloc.dupe(u8, str)));
                        },
                        .list_begin => {
                            try self.object_stack.append(.list);
                            var list_node = stackTop(self.value_stack.items).*;
                            try list_node.List.append(Value.make_list(alloc));
                            try self.value_stack.append(stackTop(list_node.List.items));
                        },
                        .dictionary_begin => {
                            try self.object_stack.append(.dictionary);
                            var list_node = stackTop(self.value_stack.items).*;
                            try list_node.List.append(Value.make_dict(alloc));
                            try self.value_stack.append(stackTop(list_node.List.items));
                            self.state = .dict_key;
                        },
                        else => {
                            return error.parse_error;
                        },
                    }
                },
                .valid => {
                    // we're already valid, shouldn't get more values
                    return error.malformed_bencode_object;
                },
            }
        }

        if (self.state != .valid) {
            return error.parse_error;
        }

        return tree;
    }
};

fn parseString(token: Token, lexer: *Lexer) ![]const u8 {
    try ensureTokenType(token, .string_length);
    const str_len = try std.fmt.parseInt(usize, token.data, 10);
    const split = lexer.next() orelse return error.missing_token;
    try ensureTokenType(split, .string_split);
    const str = lexer.next() orelse return error.missing_token;
    try ensureTokenType(str, .string);
    return str.data;
}

fn parseInteger(token: Token, lexer: *Lexer) !i64 {
    try ensureTokenType(token, .integer_begin);
    const value = lexer.next() orelse return error.missing_token;
    try ensureTokenType(value, .integer);
    const i = try std.fmt.parseInt(i64, value.data, 10);
    const end_token = lexer.next() orelse return error.missing_token;
    try ensureTokenType(end_token, .end);

    return i;
}

fn ensureTokenType(t: Token, tt: BencodeTokens) !void {
    log.debug("Comparing Tokens: {}, {}", .{ t, tt });
    if (t.token_type != tt) {
        return error.incorrect_token_type;
    }
}

fn stackTop(data: anytype) *@typeInfo(@TypeOf(data)).Pointer.child {
    return &data[data.len - 1];
}

test "parse integer" {
    const data = "i230e";
    var parser = Parser.initWithData(testing.allocator, data);
    defer parser.deinit();
    const tree = try parser.parse();
    defer tree.arena.deinit();

    switch (tree.root) {
        .Integer => |v| {
            testing.expectEqual(v, 230);
        },
        else => {
            testing.expect(false);
        },
    }
}

test "parse string" {
    const data = "5:hello";
    var parser = Parser.initWithData(testing.allocator, data);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    switch (tree.root) {
        .String => |s| {
            testing.expectEqualStrings(s, "hello");
        },
        else => {
            testing.expect(false);
        },
    }
}

fn testValueIsInteger(v: Value, i: i64) bool {
    return switch (v) {
        .Integer => |x| x == i,
        else => false,
    };
}
test "parse basic list of integers" {
    const data = "li2ei3ei4ee";
    var parser = Parser.initWithData(testing.allocator, data);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    switch (tree.root) {
        .List => |l| {
            testing.expectEqual(l.items.len, 3);
            testing.expect(testValueIsInteger(l.items[0], 2));
            testing.expect(testValueIsInteger(l.items[1], 3));
            testing.expect(testValueIsInteger(l.items[2], 4));
        },
        else => {
            testing.expect(false);
        },
    }
}

test "list of list of integers and integers" {
    const data = "lli1ei2ei3eei4ei5ee";

    var parser = Parser.initWithData(testing.allocator, data);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    switch (tree.root) {
        .List => |l| {
            testing.expectEqual(l.items.len, 3);
            switch (l.items[0]) {
                .List => |l2| {
                    testing.expect(testValueIsInteger(l2.items[0], 1));
                    testing.expect(testValueIsInteger(l2.items[1], 2));
                    testing.expect(testValueIsInteger(l2.items[2], 3));
                },
                else => testing.expect(false),
            }
            testing.expect(testValueIsInteger(l.items[1], 4));
            testing.expect(testValueIsInteger(l.items[2], 5));
        },
        else => {
            testing.expect(false);
        },
    }
}

test "parse basic dictionary" {
    const data = "d5:helloi200ee";

    var parser = Parser.initWithData(testing.allocator, data);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    switch (tree.root) {
        .Dictionary => |d| {
            var it = d.iterator();
            while (it.next()) |entry| {
                testing.expectEqualStrings(entry.key, "hello");
                switch (entry.value) {
                    .Integer => |i| testing.expectEqual(i, 200),
                    else => testing.expect(false),
                }
            }
        },
        else => testing.expect(false),
    }
}

test "parse list inside dictionary" {
    const data = "d4:listli1ei2ei3eee";

    var parser = Parser.initWithData(testing.allocator, data);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    switch (tree.root) {
        .Dictionary => |d| {
            const list = d.get("list") orelse return error.list_missing;
            testing.expect(std.meta.activeTag(list) == .List);
            testing.expectEqual(list.List.items.len, 3);
            testing.expect(testValueIsInteger(list.List.items[0], 1));
            testing.expect(testValueIsInteger(list.List.items[1], 2));
            testing.expect(testValueIsInteger(list.List.items[2], 3));
        },
        else => testing.expect(false),
    }
}

/// Implementation is private as it uses the LexerState
/// to conditionally add in some fields when only in
/// particular and the functionality makes it hard to read
/// what's actually in the impl / functions are available
/// Instead there's an Interface wrapper `Lexer`
const LexerImpl = struct {
    state: LexerState,
    data: []const u8,

    const LexerState = union(enum) {
        start: struct {},
        //parsing_dict: struct {},
        parsing_string: struct {
            str_len: usize,
            split: bool,
        },
        parsing_integer: struct {},
    };

    pub fn init() @This() {
        var l: LexerImpl = .{
            .data = undefined,
            .state = LexerState{ .start = .{} },
        };
        l.data.len = 0;
        return l;
    }

    pub fn initWithData(data: []const u8) @This() {
        var self = init();
        self.reset(data);
        return self;
    }

    pub fn reset(self: *@This(), data: []const u8) void {
        self.data = data;
        self.state = LexerState{ .start = .{} };
    }

    pub fn next(self: *@This()) ?Token {
        if (self.data.len == 0) {
            return null;
        }

        switch (self.state) {
            .start => {
                switch (self.data[0]) {
                    'd' => {
                        const token = Token{
                            .token_type = BencodeTokens.dictionary_begin,
                            .data = self.data[0..1],
                        };
                        self.data = self.data[1..];
                        return token;
                    },
                    'l' => {
                        const token = Token{
                            .token_type = BencodeTokens.list_begin,
                            .data = self.data[0..1],
                        };
                        self.data = self.data[1..];
                        return token;
                    },
                    'i' => {
                        self.state = LexerState{ .parsing_integer = .{} };
                        const token = Token{
                            .token_type = BencodeTokens.integer_begin,
                            .data = self.data[0..1],
                        };
                        self.data = self.data[1..];
                        return token;
                    },
                    '0'...'9' => {
                        var i: usize = 0;
                        while (self.data[i] != ':') : (i += 1) {}

                        const str_len = std.fmt.parseInt(usize, self.data[0..i], 10) catch {
                            return null;
                        };

                        self.state = LexerState{ .parsing_string = .{ .str_len = str_len, .split = false } };
                        const token = Token{
                            .token_type = BencodeTokens.string_length,
                            .data = self.data[0..i],
                        };
                        self.data = self.data[i..];
                        return token;
                    },
                    'e' => {
                        const token = Token{
                            .token_type = BencodeTokens.end,
                            .data = self.data[0..1],
                        };
                        self.data = self.data[1..];
                        return token;
                    },
                    else => {
                        return null;
                    },
                }
            },
            .parsing_string => {
                if (!self.state.parsing_string.split) {
                    if (self.data[0] != ':') {
                        return null;
                    }
                    const token = Token{
                        .token_type = BencodeTokens.string_split,
                        .data = self.data[0..1],
                    };
                    self.data = self.data[1..];
                    self.state.parsing_string.split = true;
                    return token;
                } else {
                    const token = Token{
                        .token_type = BencodeTokens.string,
                        .data = self.data[0..self.state.parsing_string.str_len],
                    };

                    self.data = self.data[self.state.parsing_string.str_len..];
                    self.state = LexerState{ .start = .{} };
                    return token;
                }
            },
            .parsing_integer => {
                switch (self.data[0]) {
                    '-', '0'...'9' => {
                        var i: usize = 0;
                        // we can have negative numbers
                        if (self.data[i] == '-') {
                            i += 1;
                        }
                        while (self.data[i] != 'e') : (i += 1) {
                            if (self.data[i] < '0' or self.data[i] > '9') {
                                return null;
                            }
                        }
                        const token = Token{
                            .token_type = BencodeTokens.integer,
                            .data = self.data[0..i],
                        };
                        self.data = self.data[i..];
                        return token;
                    },
                    'e' => {
                        const token = Token{
                            .token_type = BencodeTokens.end,
                            .data = self.data[0..1],
                        };
                        self.data = self.data[1..];
                        self.state = LexerState{ .start = .{} };
                        return token;
                    },
                    else => {
                        return null;
                    },
                }
            },
        }
    }
};

/// Only use in tests.
fn testTokenStream(lexer: *Lexer, expected_tokens: []const Token) void {
    var token_count: usize = 0;
    while (lexer.next()) |token| : (token_count += 1) {
        const cur_expect_token = expected_tokens[token_count];
        testing.expectEqual(cur_expect_token.token_type, token.token_type);
        testing.expectEqualStrings(cur_expect_token.data, token.data);
    }
    testing.expectEqual(expected_tokens.len, token_count);
}

test "basic dictionary tokens" {
    const data = "d5:hello5:worlde";
    var lex = Lexer.initWithData(data);

    const expected_tokens = [_]Token{
        .{ .token_type = .dictionary_begin, .data = "d" },
        .{ .token_type = .string_length, .data = "5" },
        .{ .token_type = .string_split, .data = ":" },
        .{ .token_type = .string, .data = "hello" },
        .{ .token_type = .string_length, .data = "5" },
        .{ .token_type = .string_split, .data = ":" },
        .{ .token_type = .string, .data = "world" },
        .{ .token_type = .end, .data = "e" },
    };
}

test "parse a list" {
    const data = "l5:helloi230ee";
    var lex = Lexer.initWithData(data);

    const expected_tokens = [_]Token{
        .{ .token_type = .list_begin, .data = "l" },
        .{ .token_type = .string_length, .data = "5" },
        .{ .token_type = .string_split, .data = ":" },
        .{ .token_type = .string, .data = "hello" },
        .{ .token_type = .integer_begin, .data = "i" },
        .{ .token_type = .integer, .data = "230" },
        .{ .token_type = .end, .data = "e" },
        .{ .token_type = .end, .data = "e" },
    };

    testTokenStream(&lex, expected_tokens[0..]);
}

test "list nested in dictionary" {
    const data = "d5:hellol3:eat4:firei230eee";
    var lex = Lexer.initWithData(data);

    const expected_tokens = [_]Token{
        .{ .token_type = .dictionary_begin, .data = "d" },
        .{ .token_type = .string_length, .data = "5" },
        .{ .token_type = .string_split, .data = ":" },
        .{ .token_type = .string, .data = "hello" },
        .{ .token_type = .list_begin, .data = "l" },
        .{ .token_type = .string_length, .data = "3" },
        .{ .token_type = .string_split, .data = ":" },
        .{ .token_type = .string, .data = "eat" },
        .{ .token_type = .string_length, .data = "4" },
        .{ .token_type = .string_split, .data = ":" },
        .{ .token_type = .string, .data = "fire" },
        .{ .token_type = .integer_begin, .data = "i" },
        .{ .token_type = .integer, .data = "230" },
        .{ .token_type = .end, .data = "e" },
        .{ .token_type = .end, .data = "e" },
        .{ .token_type = .end, .data = "e" },
    };

    testTokenStream(&lex, expected_tokens[0..]);
}
