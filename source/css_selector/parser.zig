const std = @import("std");
const print = std.debug.print;

const ElementType = @import("../Dom.zig").ElementType;
const Tokenizer = @import("./tokenizer.zig");

const Self = @This();

allocator: std.mem.Allocator,
tokenizer: Tokenizer = undefined,
pairs: std.ArrayList(Pair),

pub fn init(allocator: std.mem.Allocator, input: []const u8) !Self {
    var tokenizer = try Tokenizer.init(allocator, input);
    var pairs = std.ArrayList(Pair).init(allocator);
    return .{
        .pairs = pairs,
        .allocator = allocator,
        .tokenizer = tokenizer,
    };
}

pub fn printPairs(self: *Self) void {
    print("DISPLAY PAIRS:\n", .{});
    for (self.pairs.items) |pair| {
        print("combinator: {any}\n", .{pair.combinator});
        for (pair.selectors.items) |selector| {
            switch (selector) {
                .attribute => |v| print("   attribute: {s} = {s}, compare_type: {any}\n", .{ v.name, v.value, v.compare_type }),
                .type => |v| print("   type {}\n", .{v.type}),
                .pseudo_class => |v| print("   pseudo-class {s}\n", .{v.name}),
            }
        }
        print("\n", .{});
    }
}

pub fn deinit(self: *Self) void {
    self.tokenizer.deinit();
    for (self.pairs.items) |pair| pair.selectors.deinit();
    self.pairs.deinit();
}

const CompareType = enum {
    equal,
    contains,
    start_with,
    ends_with,
    contained_in_whitespace_sep,
    start_with_hyphen_sep,
};

const Combinators = enum {
    subsequent_sibling,
    next_sibling,
    child,
    column,
    descendant,
};

const SelectorAttribure = struct {
    name: []const u8,
    value: []const u8,
    compare_type: CompareType = .equal,
};

pub const Selector = union(enum) {
    attribute: SelectorAttribure,
    type: struct {
        type: ElementType,
    },
    pseudo_class: struct {
        name: []const u8,
    },
};

pub const Pair = struct {
    const ESelf = @This();

    combinator: Combinators,
    selectors: std.ArrayList(Selector),

    pub fn init(allocator: std.mem.Allocator, combinator: ?Combinators) ESelf {
        var selectors = std.ArrayList(Selector).init(allocator);
        return .{
            .combinator = combinator orelse .descendant,
            .selectors = selectors,
        };
    }

    pub fn deinit(self: *ESelf) void {
        self.selectors.deinit();
    }

    fn setCombinator(self: *ESelf, combinator: Combinators) void {
        self.combinator = combinator;
    }

    fn addAttribute(self: *ESelf, attr: SelectorAttribure) !void {
        var sel = Selector{ .attribute = attr };
        try self.selectors.append(sel);
    }

    fn addElementType(self: *ESelf, element_type: ElementType) !void {
        var sel = Selector{ .type = .{
            .type = element_type,
        } };
        try self.selectors.append(sel);
    }
};

const States = enum {
    clear,
    parse_selectors,
    space,
};

const ParsingErrors = error{
    SomethingWrong,
    ElementTypeDoesNotExist,
    UnexpectedToken,
    UnexpectedlyEnded,
};

fn parse(self: *Self) !void {
    var state: States = .clear;
    var curr_pair: Pair = Pair.init(self.allocator, Combinators.descendant);
    errdefer curr_pair.deinit();

    var i: u16 = 0;
    var space_found_combinator = false;

    while (i < self.tokenizer.token_stack.items.len) {
        const token = self.tokenizer.token_stack.items[i];
        switch (state) {
            .clear => {
                switch (token.tag) {
                    .string, .square_bracket_open => {
                        state = .parse_selectors;
                        continue;
                    },
                    Tokenizer.Tag.combinator_child => curr_pair.setCombinator(Combinators.child),
                    Tokenizer.Tag.combinator_column => curr_pair.setCombinator(Combinators.column),
                    Tokenizer.Tag.combinator_descendant => curr_pair.setCombinator(Combinators.descendant),
                    Tokenizer.Tag.combinator_next_sibling => curr_pair.setCombinator(Combinators.next_sibling),
                    Tokenizer.Tag.combinator_subsequent_sibling => curr_pair.setCombinator(Combinators.subsequent_sibling),
                    Tokenizer.Tag.space => state = .space,
                    else => {},
                }
                i += 1;
            },
            .space => {
                switch (token.tag) {
                    Tokenizer.Tag.combinator_child => {
                        curr_pair.setCombinator(Combinators.child);
                        space_found_combinator = true;
                    },
                    Tokenizer.Tag.combinator_column => {
                        curr_pair.setCombinator(Combinators.column);
                        space_found_combinator = true;
                    },
                    Tokenizer.Tag.combinator_descendant => {
                        curr_pair.setCombinator(Combinators.descendant);
                        space_found_combinator = true;
                    },
                    Tokenizer.Tag.combinator_next_sibling => {
                        curr_pair.setCombinator(Combinators.next_sibling);
                        space_found_combinator = true;
                    },
                    Tokenizer.Tag.combinator_subsequent_sibling => {
                        curr_pair.setCombinator(Combinators.subsequent_sibling);
                        space_found_combinator = true;
                    },
                    Tokenizer.Tag.space => {},
                    else => {
                        if (!space_found_combinator) {
                            curr_pair.setCombinator(Combinators.descendant);
                        }
                        space_found_combinator = false;
                        state = .clear;
                        continue;
                    },
                }
                i += 1;
            },
            .parse_selectors => {
                //print("HERE\n", .{});
                switch (token.tag) {
                    Tokenizer.Tag.string => {
                        var elem_type = ElementType.fromStringHtml(token.info.?);
                        if (elem_type == null) {
                            return ParsingErrors.ElementTypeDoesNotExist;
                        }
                        try curr_pair.addElementType(elem_type.?);
                        i += 1;
                    },
                    Tokenizer.Tag.classname => {
                        try curr_pair.addAttribute(.{
                            .name = "class",
                            .value = token.info.?,
                            .compare_type = CompareType.contained_in_whitespace_sep,
                        });
                        i += 1;
                    },
                    Tokenizer.Tag.id => {
                        try curr_pair.addAttribute(.{
                            .name = "id",
                            .value = token.info.?,
                            .compare_type = CompareType.equal,
                        });
                        i += 1;
                    },
                    Tokenizer.Tag.square_bracket_open => {
                        var attr = try parseAttributeSelector(&self.tokenizer, &i);
                        try curr_pair.addAttribute(attr);
                    },
                    else => {
                        try self.pairs.append(curr_pair);
                        curr_pair = Pair.init(self.allocator, Combinators.descendant);
                        state = .clear;
                    },
                }
            },
        }
    }
    if (curr_pair.selectors.items.len > 0) {
        try self.pairs.append(curr_pair);
    } else {
        curr_pair.deinit();
    }
}

pub fn parseAttributeSelector(tokenizer: *Tokenizer, cursor: *u16) !SelectorAttribure {
    var attr_name: []const u8 = undefined;
    var value: []const u8 = undefined;
    var compare_type: CompareType = undefined;

    var token = try expectToken(tokenizer, cursor, .square_bracket_open);
    cursor.* += 1;

    token = try expectToken(tokenizer, cursor, .string);
    cursor.* += 1;
    attr_name = token.info.?;

    token = try expectToken(tokenizer, cursor, null);
    cursor.* += 1;
    switch (token.tag) {
        Tokenizer.Tag.compare_contains => compare_type = CompareType.contains,
        Tokenizer.Tag.compare_equal => compare_type = CompareType.equal,
        Tokenizer.Tag.compare_ends_with => compare_type = CompareType.ends_with,
        Tokenizer.Tag.compare_starts_with => compare_type = CompareType.start_with,
        Tokenizer.Tag.compare_in_whitespace_sep => compare_type = CompareType.contained_in_whitespace_sep,
        Tokenizer.Tag.compare_starts_with_hyphen_sep => compare_type = CompareType.start_with_hyphen_sep,
        else => return ParsingErrors.UnexpectedToken,
    }

    token = try expectToken(tokenizer, cursor, null);
    cursor.* += 1;
    switch (token.tag) {
        Tokenizer.Tag.string, Tokenizer.Tag.quoted_string => value = token.info.?,
        else => return ParsingErrors.UnexpectedToken,
    }

    token = try expectToken(tokenizer, cursor, .square_bracket_close);
    cursor.* += 1;

    return .{
        .name = attr_name,
        .value = value,
        .compare_type = compare_type,
    };
}

fn expectToken(tokenizer: *Tokenizer, cursor: *u16, tag: ?Tokenizer.Tag) !Tokenizer.Token {
    var token: ?Tokenizer.Token = tokenizer.getToken(cursor.*);
    if (token == null) {
        return ParsingErrors.UnexpectedlyEnded;
    }
    if (tag) |t| {
        if (token.?.tag == t) {
            return token.?;
        }
        print("Found token {}\n", .{token.?.tag});
        return ParsingErrors.UnexpectedToken;
    }
    return token.?;
}

fn skipSpace(tokenizer: *Tokenizer, cursor: *u16) void {
    var token: ?Tokenizer.Token = tokenizer.getToken(cursor);
    while (token != null and token.?.tag == .space) {
        cursor.* += 1;
        token = tokenizer.getToken(cursor);
    }
}

pub fn run(self: *Self) !void {
    try self.tokenizer.run();
    self.tokenizer.printTokens();
    print("\n\n", .{});

    try self.parse();
}
