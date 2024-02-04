const std = @import("std");
const print = std.debug.print;

const Self = @This();

pub const Tag = enum {
    string,
    quoted_string,
    classname,
    id,
    star,

    compare_equal,
    compare_in_whitespace_sep,
    compare_starts_with,
    compare_ends_with,
    compare_contains,
    compare_starts_with_hyphen_sep,

    square_bracket_open,
    square_bracket_close,
    bracket_open,
    bracket_close,

    combinator_next_sibling,
    combinator_child,
    combinator_column,
    combinator_subsequent_sibling,
    combinator_descendant,
    combinator_namespace,

    space,
    quotation_mark,
    colon,

    unknown,
};

const States = enum {
    clear,
    read_classname,
    read_id,
    read_string,
    space,

    read_quoted_string,

    pipe,
    caret,
    dollar,
    star,
    tilde,
};
pub const Token = struct {
    tag: Tag,
    info: ?[]const u8 = null,
    removed: bool = false,
};
const TokenStack = std.ArrayList(Token);

curr_pos: u16 = 0,
text: []const u8,
token_stack: std.ArrayList(Token),

state: States = .clear,

pub fn init(allocator: std.mem.Allocator, selector_text: []const u8) !Self {
    var stack = std.ArrayList(Token).init(allocator);
    return .{
        .token_stack = stack,
        .text = selector_text,
    };
}

pub fn deinit(self: *Self) void {
    self.token_stack.deinit();
}

pub fn getToken(self: *Self, index: u16) ?Token {
    if (index < self.token_stack.items.len) {
        return self.token_stack.items[index];
    }
    return null;
}

pub fn printTokens(self: *Self) void {
    for (self.token_stack.items) |obj| {
        print("Token: .tag = {any}, info: {s}\n", .{ obj.tag, obj.info orelse "" });
    }
}

// TODO: Read number
pub fn nextToken(self: *Self) !?Self.Token {
    self.state = .clear;
    if (self.curr_pos > self.text.len) {
        return null;
    }
    var found_combinator: Tag = Tag.classname;
    _ = found_combinator;
    var read_start_pos: u16 = 0;
    while (self.curr_pos <= self.text.len) {
        const ch: ?u8 = if (self.curr_pos < self.text.len) self.text[self.curr_pos] else null;
        switch (self.state) {
            .clear => {
                if (ch == null) {
                    return null;
                }
                switch (ch.?) {
                    '"' => {
                        self.state = .read_quoted_string;
                        read_start_pos = self.curr_pos + 1;
                    },
                    '.' => {
                        self.state = .read_classname;
                        read_start_pos = self.curr_pos + 1;
                    },
                    '#' => {
                        self.state = .read_id;
                        read_start_pos = self.curr_pos + 1;
                    },
                    ' ' => {
                        self.state = .space;
                        read_start_pos = self.curr_pos + 1;
                    },
                    '~' => {
                        self.state = .tilde;
                    },
                    '^' => {
                        self.state = .caret;
                    },
                    '$' => {
                        self.state = .dollar;
                    },
                    '*' => {
                        self.state = .star;
                    },
                    '|' => {
                        self.state = .pipe;
                    },
                    'a'...'z', 'A'...'Z', '-' => {
                        self.state = .read_string;
                        read_start_pos = self.curr_pos;
                    },
                    '[' => {
                        self.curr_pos += 1;
                        return .{ .tag = .square_bracket_open };
                    },
                    ']' => {
                        self.curr_pos += 1;
                        return .{ .tag = .square_bracket_close };
                    },
                    ':' => {
                        self.curr_pos += 1;
                        return .{ .tag = .colon };
                    },
                    '(' => {
                        self.curr_pos += 1;
                        return .{ .tag = .bracket_open };
                    },
                    ')' => {
                        self.curr_pos += 1;
                        return .{ .tag = .bracket_close };
                    },
                    '=' => {
                        self.curr_pos += 1;
                        return .{ .tag = .compare_equal };
                    },
                    '>' => {
                        self.curr_pos += 1;
                        return .{ .tag = .combinator_child };
                    },
                    '+' => {
                        self.curr_pos += 1;
                        return .{ .tag = .combinator_next_sibling };
                    },

                    else => {},
                }
                self.curr_pos += 1;
            },
            .space => {
                if (ch == null) {
                    return .{ .tag = .space };
                }
                switch (ch.?) {
                    ' ' => self.curr_pos += 1,
                    else => {
                        return .{ .tag = .space };
                    },
                }
            },
            .pipe => {
                if (ch == null) {
                    return .{ .tag = .combinator_namespace };
                }
                switch (ch.?) {
                    '|' => {
                        self.curr_pos += 1;
                        return .{ .tag = .combinator_column };
                    },
                    '=' => {
                        self.curr_pos += 1;
                        return .{ .tag = .compare_starts_with_hyphen_sep };
                    },
                    else => {
                        return .{ .tag = .combinator_namespace };
                    },
                }
            },
            .caret => {
                if (ch == null) {
                    return .{ .tag = .unknown };
                }
                switch (ch.?) {
                    '=' => {
                        self.curr_pos += 1;
                        return .{ .tag = .compare_starts_with };
                    },
                    else => {
                        return .{ .tag = .unknown };
                    },
                }
            },
            .dollar => {
                if (ch == null) {
                    return .{ .tag = .unknown };
                }
                switch (ch.?) {
                    '=' => {
                        self.curr_pos += 1;
                        return .{ .tag = .compare_ends_with };
                    },
                    else => {
                        return .{ .tag = .unknown };
                    },
                }
            },
            .star => {
                if (ch == null) {
                    return .{ .tag = .star };
                }
                switch (ch.?) {
                    '=' => {
                        self.curr_pos += 1;
                        return .{ .tag = .compare_contains };
                    },
                    else => {
                        return .{ .tag = .star };
                    },
                }
            },
            .tilde => {
                if (ch == null) {
                    return .{ .tag = .combinator_subsequent_sibling };
                }
                switch (ch.?) {
                    '=' => {
                        self.curr_pos += 1;
                        return .{ .tag = .compare_in_whitespace_sep };
                    },
                    else => {
                        return .{ .tag = .combinator_subsequent_sibling };
                    },
                }
            },
            .read_classname, .read_id, .read_string => {
                if (ch == null) {
                    var token = Token{
                        .tag = switch (self.state) {
                            .read_classname => Tag.classname,
                            .read_id => Tag.id,
                            .read_string => Tag.string,
                            else => Tag.unknown,
                        },
                        .info = self.text[read_start_pos..self.curr_pos],
                    };
                    return token;
                }
                switch (ch.?) {
                    'a'...'z', 'A'...'Z', '-' => {},
                    else => {
                        var token = Token{
                            .tag = switch (self.state) {
                                .read_classname => Tag.classname,
                                .read_id => Tag.id,
                                .read_string => Tag.string,
                                else => Tag.unknown,
                            },
                            .info = self.text[read_start_pos..self.curr_pos],
                        };
                        return token;
                    },
                }
                self.curr_pos += 1;
            },
            .read_quoted_string => {
                if (ch == null) {
                    // Error end quotation mark not found
                    // TODO: Deal with error
                    return null;
                }
                // TODO: Deel with \", escaping "
                switch (ch.?) {
                    '"' => {
                        self.curr_pos += 1;
                        var token = Token{
                            .tag = .quoted_string,
                            .info = self.text[read_start_pos .. self.curr_pos - 1],
                        };
                        return token;
                    },
                    else => {},
                }
                self.curr_pos += 1;
            },
        }
    }
    return null;
}

pub fn run(self: *Self) !void {
    while (try self.nextToken()) |token| {
        try self.token_stack.append(token);
    }
}
