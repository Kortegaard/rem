// Copyright (C) 2021-2023 Chadwain Holness
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

const Dom = @import("../Dom.zig");

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const ComptimeStringMap = std.ComptimeStringMap;
const MultiArrayList = std.MultiArrayList;
const CssParser = @import("../css_selector/parser.zig");
const compareStrings = @import("../util.zig").compareString;

pub const Document = struct {
    doctype: ?*DocumentType = null,
    element: ?*Element = null,
    cdata: ArrayListUnmanaged(*CharacterData) = .{},
    cdata_endpoints: [3]Endpoints = .{.{ .begin = 0, .end = 0 }} ** 3,
    cdata_current_endpoint: u2 = 0,
    quirks_mode: QuirksMode = .no_quirks,

    pub const Endpoints = struct {
        begin: usize,
        end: usize,

        pub fn sliceOf(self: Endpoints, array: anytype) @TypeOf(array) {
            return array[self.begin..self.end];
        }
    };

    pub const QuirksMode = enum {
        no_quirks,
        quirks,
        limited_quirks,
    };

    pub fn deinit(self: *Document, allocator: Allocator) void {
        self.cdata.deinit(allocator);
    }
};

pub const DocumentFormatter = struct {
    document: *const Document,
    dom: *const Dom,
    allocator: Allocator,

    pub fn print(self: DocumentFormatter, writer: anytype) !void {
        try std.fmt.format(writer, "Document: {s}\n", .{@tagName(self.document.quirks_mode)});

        try printDocumentCdatas(writer, self.document, 0);

        if (self.document.doctype) |doctype| {
            try std.fmt.format(writer, "  DocumentType: name={s} publicId={s} systemId={s}\n", .{ doctype.name, doctype.publicId, doctype.systemId });
        }

        try printDocumentCdatas(writer, self.document, 1);

        const ConstElementOrCharacterData = union(enum) {
            element: *const Element,
            cdata: *const CharacterData,
        };
        var node_stack = ArrayListUnmanaged(struct { node: ConstElementOrCharacterData, depth: usize }){};
        defer node_stack.deinit(self.allocator);

        if (self.document.element) |document_element| {
            try node_stack.append(self.allocator, .{ .node = .{ .element = document_element }, .depth = 1 });
        }

        while (node_stack.items.len > 0) {
            const item = node_stack.pop();
            var len = item.depth;
            while (len > 0) : (len -= 1) {
                try std.fmt.format(writer, "  ", .{});
            }
            switch (item.node) {
                .element => |element| {
                    try std.fmt.format(writer, "Element: type={s} local_name={s} namespace={s} attributes=[", .{
                        @tagName(element.element_type),
                        element.localName(self.dom),
                        @tagName(element.namespace()),
                    });
                    const num_attributes = element.numAttributes();
                    if (num_attributes > 0) {
                        try writer.writeAll(" ");
                        const attribute_slice = element.attributes.slice();
                        var i: u32 = 0;
                        while (i < num_attributes) : (i += 1) {
                            const key = attribute_slice.items(.key)[i];
                            const value = attribute_slice.items(.value)[i];
                            if (key.prefix == .none) {
                                try std.fmt.format(writer, "\"{s}\"=\"{}\" ", .{ key.local_name, std.zig.fmtEscapes(value) });
                            } else {
                                try std.fmt.format(writer, "\"{s}:{s}\"=\"{}\" ", .{ @tagName(key.prefix), key.local_name, std.zig.fmtEscapes(value) });
                            }
                        }
                    }
                    try std.fmt.format(writer, "]\n", .{});

                    // Add children to stack
                    var num_children = element.children.items.len;
                    while (num_children > 0) : (num_children -= 1) {
                        const node = switch (element.children.items[num_children - 1]) {
                            .element => |e| ConstElementOrCharacterData{ .element = e },
                            .cdata => |c| ConstElementOrCharacterData{ .cdata = c },
                        };
                        try node_stack.append(self.allocator, .{ .node = node, .depth = item.depth + 1 });
                    }
                },
                .cdata => |cdata| try printCdata(writer, cdata),
            }
        }

        try printDocumentCdatas(writer, self.document, 2);
    }

    fn printDocumentCdatas(writer: anytype, document: *const Document, endpoint_index: u2) !void {
        const endpoint = document.cdata_endpoints[endpoint_index];
        for (endpoint.sliceOf(document.cdata.items)) |cdata| {
            try printCdata(writer, cdata);
        }
    }

    fn printCdata(writer: anytype, cdata: *const CharacterData) !void {
        const interface = switch (cdata.interface) {
            .text => "Text",
            .comment => "Comment",
        };
        try std.fmt.format(writer, "{s}: \"{}\"\n", .{ interface, std.zig.fmtEscapes(cdata.data.items) });
    }
};

pub const DocumentType = struct {
    name: []u8,
    publicId: []u8,
    systemId: []u8,

    pub fn init(allocator: Allocator, doctype_name: ?[]const u8, public_identifier: ?[]const u8, system_identifier: ?[]const u8) !DocumentType {
        const name = doctype_name orelse "";
        const publicId = public_identifier orelse "";
        const systemId = system_identifier orelse "";
        const strings = try allocator.alloc(u8, name.len + publicId.len + systemId.len);

        var result = @as(DocumentType, undefined);
        var index: usize = 0;
        result.name = strings[index .. index + name.len];
        index += name.len;
        result.publicId = strings[index .. index + publicId.len];
        index += publicId.len;
        result.systemId = strings[index .. index + systemId.len];

        @memcpy(result.name, name);
        @memcpy(result.publicId, publicId);
        @memcpy(result.systemId, systemId);

        return result;
    }

    pub fn deinit(self: *DocumentType, allocator: Allocator) void {
        const memory = self.name.ptr[0 .. self.name.len + self.publicId.len + self.systemId.len];
        allocator.free(memory);
    }
};

pub const Namespace = enum {
    html,
    svg,
    mathml,
};

pub const ElementType = enum {
    // This is the complete list of conforming HTML elements.
    // (https://html.spec.whatwg.org/multipage/indices.html#elements-3)
    html_a,
    html_abbr,
    html_address,
    html_area,
    html_article,
    html_aside,
    html_audio,
    html_b,
    html_base,
    html_bdi,
    html_bdo,
    html_blockquote,
    html_body,
    html_br,
    html_button,
    html_canvas,
    html_caption,
    html_cite,
    html_code,
    html_col,
    html_colgroup,
    html_data,
    html_datalist,
    html_dd,
    html_del,
    html_details,
    html_dfn,
    html_dialog,
    html_div,
    html_dl,
    html_dt,
    html_em,
    html_embed,
    html_fieldset,
    html_figcaption,
    html_figure,
    html_footer,
    html_form,
    html_h1,
    html_h2,
    html_h3,
    html_h4,
    html_h5,
    html_h6,
    html_head,
    html_header,
    html_hgroup,
    html_hr,
    html_html,
    html_i,
    html_iframe,
    html_img,
    html_input,
    html_ins,
    html_kbd,
    html_label,
    html_legend,
    html_li,
    html_link,
    html_main,
    html_map,
    html_mark,
    html_menu,
    html_meta,
    html_meter,
    html_nav,
    html_noscript,
    html_object,
    html_ol,
    html_optgroup,
    html_option,
    html_output,
    html_p,
    html_param,
    html_picture,
    html_pre,
    html_progress,
    html_q,
    html_rp,
    html_rt,
    html_ruby,
    html_s,
    html_samp,
    html_script,
    html_section,
    html_select,
    html_slot,
    html_small,
    html_source,
    html_span,
    html_strong,
    html_style,
    html_sub,
    html_summary,
    html_sup,
    html_table,
    html_tbody,
    html_td,
    html_template,
    html_textarea,
    html_tfoot,
    html_th,
    html_thead,
    html_time,
    html_title,
    html_tr,
    html_track,
    html_u,
    html_ul,
    html_var,
    html_video,
    html_wbr,

    // This is the complete list of obsolete and non-conforming elements.
    // (https://html.spec.whatwg.org/multipage/obsolete.html#non-conforming-features)
    html_acronym,
    html_applet,
    html_basefont,
    html_bgsound,
    html_big,
    html_blink,
    html_center,
    html_dir,
    html_font,
    html_frame,
    html_frameset,
    html_isindex,
    html_keygen,
    html_listing,
    html_marquee,
    html_menuitem,
    html_multicol,
    html_nextid,
    html_nobr,
    html_noembed,
    html_noframes,
    html_plaintext,
    html_rb,
    html_rtc,
    html_spacer,
    html_strike,
    html_tt,
    html_xmp,

    mathml_math,
    mathml_mi,
    mathml_mo,
    mathml_mn,
    mathml_ms,
    mathml_mtext,
    mathml_annotation_xml,

    svg_svg,
    svg_foreign_object,
    svg_desc,
    svg_title,
    svg_script,

    /// The type of a custom HTML element.
    custom_html,
    /// The type of a MathML element that this DOM implementation doesn't know about.
    some_other_mathml,
    /// The type of an SVG element that this DOM implementation doesn't know about.
    some_other_svg,

    pub fn namespace(self: ElementType) Namespace {
        // TODO: Some metaprogramming to make this less fragile.
        const html_lowest = std.meta.fieldInfo(ElementType, .html_a).value;
        const html_highest = std.meta.fieldInfo(ElementType, .html_xmp).value;

        const mathml_lowest = std.meta.fieldInfo(ElementType, .mathml_math).value;
        const mathml_highest = std.meta.fieldInfo(ElementType, .mathml_annotation_xml).value;

        const svg_lowest = std.meta.fieldInfo(ElementType, .svg_svg).value;
        const svg_highest = std.meta.fieldInfo(ElementType, .svg_script).value;

        const value = @intFromEnum(self);
        if ((value >= html_lowest and value <= html_highest) or self == .custom_html) {
            return .html;
        } else if ((value >= mathml_lowest and value <= mathml_highest) or self == .some_other_mathml) {
            return .mathml;
        } else if ((value >= svg_lowest and value <= svg_highest) or self == .some_other_svg) {
            return .svg;
        } else {
            unreachable;
        }
    }

    const html_map = html_map: {
        @setEvalBranchQuota(5000);
        break :html_map ComptimeStringMap(ElementType, .{
            .{ "a", .html_a },
            .{ "abbr", .html_abbr },
            .{ "address", .html_address },
            .{ "area", .html_area },
            .{ "article", .html_article },
            .{ "aside", .html_aside },
            .{ "audio", .html_audio },
            .{ "b", .html_b },
            .{ "base", .html_base },
            .{ "bdi", .html_bdi },
            .{ "bdo", .html_bdo },
            .{ "blockquote", .html_blockquote },
            .{ "body", .html_body },
            .{ "br", .html_br },
            .{ "button", .html_button },
            .{ "canvas", .html_canvas },
            .{ "caption", .html_caption },
            .{ "cite", .html_cite },
            .{ "code", .html_code },
            .{ "col", .html_col },
            .{ "colgroup", .html_colgroup },
            .{ "data", .html_data },
            .{ "datalist", .html_datalist },
            .{ "dd", .html_dd },
            .{ "del", .html_del },
            .{ "details", .html_details },
            .{ "dfn", .html_dfn },
            .{ "dialog", .html_dialog },
            .{ "div", .html_div },
            .{ "dl", .html_dl },
            .{ "dt", .html_dt },
            .{ "em", .html_em },
            .{ "embed", .html_embed },
            .{ "fieldset", .html_fieldset },
            .{ "figcaption", .html_figcaption },
            .{ "figure", .html_figure },
            .{ "footer", .html_footer },
            .{ "form", .html_form },
            .{ "h1", .html_h1 },
            .{ "h2", .html_h2 },
            .{ "h3", .html_h3 },
            .{ "h4", .html_h4 },
            .{ "h5", .html_h5 },
            .{ "h6", .html_h6 },
            .{ "head", .html_head },
            .{ "header", .html_header },
            .{ "hgroup", .html_hgroup },
            .{ "hr", .html_hr },
            .{ "html", .html_html },
            .{ "i", .html_i },
            .{ "iframe", .html_iframe },
            .{ "img", .html_img },
            .{ "input", .html_input },
            .{ "ins", .html_ins },
            .{ "kbd", .html_kbd },
            .{ "label", .html_label },
            .{ "legend", .html_legend },
            .{ "li", .html_li },
            .{ "link", .html_link },
            .{ "main", .html_main },
            .{ "map", .html_map },
            .{ "mark", .html_mark },
            .{ "menu", .html_menu },
            .{ "meta", .html_meta },
            .{ "meter", .html_meter },
            .{ "nav", .html_nav },
            .{ "noscript", .html_noscript },
            .{ "object", .html_object },
            .{ "ol", .html_ol },
            .{ "optgroup", .html_optgroup },
            .{ "option", .html_option },
            .{ "output", .html_output },
            .{ "p", .html_p },
            .{ "param", .html_param },
            .{ "picture", .html_picture },
            .{ "pre", .html_pre },
            .{ "progress", .html_progress },
            .{ "q", .html_q },
            .{ "rp", .html_rp },
            .{ "rt", .html_rt },
            .{ "ruby", .html_ruby },
            .{ "s", .html_s },
            .{ "samp", .html_samp },
            .{ "script", .html_script },
            .{ "section", .html_section },
            .{ "select", .html_select },
            .{ "slot", .html_slot },
            .{ "small", .html_small },
            .{ "source", .html_source },
            .{ "span", .html_span },
            .{ "strong", .html_strong },
            .{ "style", .html_style },
            .{ "sub", .html_sub },
            .{ "summary", .html_summary },
            .{ "sup", .html_sup },
            .{ "table", .html_table },
            .{ "tbody", .html_tbody },
            .{ "td", .html_td },
            .{ "template", .html_template },
            .{ "textarea", .html_textarea },
            .{ "tfoot", .html_tfoot },
            .{ "th", .html_th },
            .{ "thead", .html_thead },
            .{ "time", .html_time },
            .{ "title", .html_title },
            .{ "tr", .html_tr },
            .{ "track", .html_track },
            .{ "u", .html_u },
            .{ "ul", .html_ul },
            .{ "var", .html_var },
            .{ "video", .html_video },
            .{ "wbr", .html_wbr },

            .{ "acronym", .html_acronym },
            .{ "applet", .html_applet },
            .{ "basefont", .html_basefont },
            .{ "bgsound", .html_bgsound },
            .{ "big", .html_big },
            .{ "blink", .html_blink },
            .{ "center", .html_center },
            .{ "dir", .html_dir },
            .{ "font", .html_font },
            .{ "frame", .html_frame },
            .{ "frameset", .html_frameset },
            .{ "isindex", .html_isindex },
            .{ "keygen", .html_keygen },
            .{ "listing", .html_listing },
            .{ "marquee", .html_marquee },
            .{ "menuitem", .html_menuitem },
            .{ "multicol", .html_multicol },
            .{ "nextid", .html_nextid },
            .{ "nobr", .html_nobr },
            .{ "noembed", .html_noembed },
            .{ "noframes", .html_noframes },
            .{ "plaintext", .html_plaintext },
            .{ "rb", .html_rb },
            .{ "rtc", .html_rtc },
            .{ "spacer", .html_spacer },
            .{ "strike", .html_strike },
            .{ "tt", .html_tt },
            .{ "xmp", .html_xmp },
        });
    };

    const mathml_map = ComptimeStringMap(ElementType, .{
        .{ "math", .mathml_math },
        .{ "mi", .mathml_mi },
        .{ "mo", .mathml_mo },
        .{ "mn", .mathml_mn },
        .{ "ms", .mathml_ms },
        .{ "mtext", .mathml_mtext },
        .{ "annotation-xml", .mathml_annotation_xml },
    });

    const svg_map = ComptimeStringMap(ElementType, .{
        .{ "svg", .svg_svg },
        .{ "foreignObject", .svg_foreign_object },
        .{ "desc", .svg_desc },
        .{ "title", .svg_title },
        .{ "script", .svg_script },
    });

    /// Get an HTML element's ElementType from its tag name.
    pub fn fromStringHtml(tag_name: []const u8) ?ElementType {
        return html_map.get(tag_name);
    }

    /// Get a MathML element's ElementType from its tag name.
    pub fn fromStringMathMl(tag_name: []const u8) ?ElementType {
        return mathml_map.get(tag_name);
    }

    /// Get an SVG element's ElementType from its tag name.
    pub fn fromStringSvg(tag_name: []const u8) ?ElementType {
        return svg_map.get(tag_name);
    }

    /// Returns the local name of an element based solely on its ElementType, or null if it cannot be determined.
    pub fn toLocalName(self: ElementType) ?[]const u8 {
        const tag_name = @tagName(self);
        return switch (self) {
            .html_a,
            .html_abbr,
            .html_address,
            .html_area,
            .html_article,
            .html_aside,
            .html_audio,
            .html_b,
            .html_base,
            .html_bdi,
            .html_bdo,
            .html_blockquote,
            .html_body,
            .html_br,
            .html_button,
            .html_canvas,
            .html_caption,
            .html_cite,
            .html_code,
            .html_col,
            .html_colgroup,
            .html_data,
            .html_datalist,
            .html_dd,
            .html_del,
            .html_details,
            .html_dfn,
            .html_dialog,
            .html_div,
            .html_dl,
            .html_dt,
            .html_em,
            .html_embed,
            .html_fieldset,
            .html_figcaption,
            .html_figure,
            .html_footer,
            .html_form,
            .html_h1,
            .html_h2,
            .html_h3,
            .html_h4,
            .html_h5,
            .html_h6,
            .html_head,
            .html_header,
            .html_hgroup,
            .html_hr,
            .html_html,
            .html_i,
            .html_iframe,
            .html_img,
            .html_input,
            .html_ins,
            .html_kbd,
            .html_label,
            .html_legend,
            .html_li,
            .html_link,
            .html_main,
            .html_map,
            .html_mark,
            .html_menu,
            .html_meta,
            .html_meter,
            .html_nav,
            .html_noscript,
            .html_object,
            .html_ol,
            .html_optgroup,
            .html_option,
            .html_output,
            .html_p,
            .html_param,
            .html_picture,
            .html_pre,
            .html_progress,
            .html_q,
            .html_rp,
            .html_rt,
            .html_ruby,
            .html_s,
            .html_samp,
            .html_script,
            .html_section,
            .html_select,
            .html_slot,
            .html_small,
            .html_source,
            .html_span,
            .html_strong,
            .html_style,
            .html_sub,
            .html_summary,
            .html_sup,
            .html_table,
            .html_tbody,
            .html_td,
            .html_template,
            .html_textarea,
            .html_tfoot,
            .html_th,
            .html_thead,
            .html_time,
            .html_title,
            .html_tr,
            .html_track,
            .html_u,
            .html_ul,
            .html_var,
            .html_video,
            .html_wbr,

            .html_acronym,
            .html_applet,
            .html_basefont,
            .html_bgsound,
            .html_big,
            .html_blink,
            .html_center,
            .html_dir,
            .html_font,
            .html_frame,
            .html_frameset,
            .html_isindex,
            .html_keygen,
            .html_listing,
            .html_marquee,
            .html_menuitem,
            .html_multicol,
            .html_nextid,
            .html_nobr,
            .html_noembed,
            .html_noframes,
            .html_plaintext,
            .html_rb,
            .html_rtc,
            .html_spacer,
            .html_strike,
            .html_tt,
            .html_xmp,
            => tag_name[5..],

            .mathml_math,
            .mathml_mi,
            .mathml_mo,
            .mathml_mn,
            .mathml_ms,
            .mathml_mtext,
            => tag_name[7..],
            .mathml_annotation_xml => "annotation-xml",

            .svg_svg,
            .svg_desc,
            .svg_title,
            .svg_script,
            => tag_name[4..],
            .svg_foreign_object => "foreignObject",

            .custom_html,
            .some_other_mathml,
            .some_other_svg,
            => null,
        };
    }
};

/// The type for the children of an Element node.
pub const ElementOrCharacterData = union(enum) {
    element: *Element,
    cdata: *CharacterData,
};

/// The type for the parent of an Element node.
pub const ParentNode = union(enum) {
    element: *Element,
    document,
};

pub const AttributePrefix = enum {
    none,
    xlink,
    xml,
    xmlns,
};

pub const AttributeNamespace = enum {
    none,
    xlink,
    xml,
    xmlns,
};

pub const ElementAttributesKey = struct {
    prefix: AttributePrefix,
    namespace: AttributeNamespace,
    local_name: []const u8,

    pub fn eql(lhs: ElementAttributesKey, rhs: ElementAttributesKey) bool {
        switch (lhs.prefix) {
            .none, .xlink, .xml, .xmlns => if (lhs.prefix != rhs.prefix) return false,
        }
        switch (lhs.namespace) {
            .none, .xlink, .xml, .xmlns => if (lhs.namespace != rhs.namespace) return false,
        }
        return std.mem.eql(u8, lhs.local_name, rhs.local_name);
    }
};

pub const Attribute = struct {
    key: ElementAttributesKey,
    value: []const u8,
};

pub const SimpleAttribute = struct {
    key: []const u8,
    value: []const u8,
};

/// The type for the attributes of an Element node.
pub const ElementAttributes = MultiArrayList(Attribute);

pub const Element = struct {
    element_type: ElementType,
    parent: ?ParentNode,
    attributes: ElementAttributes,
    children: ArrayListUnmanaged(ElementOrCharacterData),

    pub const IteratorStrategy = enum {
        descendant,
        child,
    };

    const ItStrategy = union(IteratorStrategy) {
        descendant: IteratorStyleDescendant,
        child: IteratorStyleChild,

        pub fn next(self: *ItStrategy) anyerror!?*Element {
            switch (self.*) {
                inline else => |case| return @TypeOf(case).next(self),
            }
        }

        pub fn restart(self: *ItStrategy) !void {
            switch (self.*) {
                inline else => |case| try @TypeOf(case).restart(self),
            }
        }

        pub fn reuse(self: *ItStrategy, elem: *Element) !void {
            self.setBaseElem(elem);
            try self.restart();
        }

        fn setBaseElem(self: *ItStrategy, elem: *Element) void {
            switch (self.*) {
                inline else => |case| @TypeOf(case).setBaseElem(self, elem),
            }
        }

        pub fn deinit(self: *ItStrategy) void {
            switch (self.*) {
                inline else => |case| @TypeOf(case).deinit(self),
            }
        }
    };

    pub const IteratorStyleChild = struct {
        const ESelf = @This();

        base_elem: *Element,
        curr_child: u16 = 0,

        pub fn init(elem: *Element) ESelf {
            return .{ .base_elem = elem };
        }

        pub fn deinit(ptr: *anyopaque) void {
            _ = ptr;
        }

        pub fn restart(ptr: *anyopaque) !void {
            const self: *ESelf = @ptrCast(@alignCast(ptr));
            self.curr_child = 0;
        }

        pub fn setBaseElem(ptr: *anyopaque, elem: *Element) void {
            const self: *ESelf = @ptrCast(@alignCast(ptr));
            self.base_elem = elem;
        }

        pub fn next(ptr: *ItStrategy) !?*Element {
            const self: *ESelf = @ptrCast(@alignCast(ptr));
            if (self.curr_child >= self.base_elem.children.items.len) {
                return null;
            }
            while (self.curr_child < self.base_elem.children.items.len) {
                var elem = self.base_elem.children.items[self.curr_child];
                const node: ?*Element = switch (elem) {
                    .element => |e| e,
                    .cdata => null,
                };
                if (node == null) {
                    self.curr_child += 1;
                    continue;
                }
                self.curr_child += 1;
                return node;
            }
            return null;
        }
    };

    pub const IteratorStyleDescendant = struct {
        const ESelf = @This();
        elem_stack: std.ArrayList(*Element),
        base_elem: *Element,

        pub fn init(allocator: std.mem.Allocator, elem: *Element) !ESelf {
            var ar = std.ArrayList(*Element).init(allocator);
            try ar.append(elem);
            return .{
                .elem_stack = ar,
                .base_elem = elem,
            };
        }

        pub fn setBaseElem(ptr: *anyopaque, elem: *Element) void {
            const self: *ESelf = @ptrCast(@alignCast(ptr));
            self.base_elem = elem;
        }

        pub fn deinit(ptr: *anyopaque) void {
            const self: *ESelf = @ptrCast(@alignCast(ptr));
            self.elem_stack.deinit();
        }

        pub fn restart(ptr: *anyopaque) !void {
            const self: *ESelf = @ptrCast(@alignCast(ptr));
            while (self.elem_stack.popOrNull()) |_| {}
            try self.elem_stack.append(self.base_elem);
            //std.debug.print("restart -- {any}\n", .{self.elem_stack.items.len});
        }

        pub fn next(ptr: *ItStrategy) !?*Element {
            const self: *ESelf = @ptrCast(@alignCast(ptr));
            while (self.elem_stack.items.len != 0) {
                const a = try self.nextInternal();
                if (a == null) {
                    return null;
                }
                return a.?;
            }
            return null;
        }

        pub fn nextInternal(self: *ESelf) !?*Element {
            if (self.elem_stack.items.len == 0) {
                return null;
            }
            const element = self.elem_stack.pop();
            var num_children = element.children.items.len;
            while (num_children > 0) : (num_children -= 1) {
                const node: ?*Element = switch (element.children.items[num_children - 1]) {
                    .element => |e| e,
                    .cdata => null,
                };
                if (node == null) {
                    continue;
                }
                try self.elem_stack.append(node.?);
            }
            return element;
        }
    };

    const IteratorElementCssSelector = struct {
        allocator: Allocator,
        base_elem: *Element,
        css_parser: CssParser,
        iterators: std.ArrayList(*IteratorElement),
        moving_cursor: i32 = 0,

        pub fn init(allocator: Allocator, elem: *Element, css_selector_text: []const u8) !IteratorElementCssSelector {
            var mparser = try CssParser.init(allocator, css_selector_text);
            try mparser.run();

            var its = std.ArrayList(*IteratorElement).init(allocator);

            return .{
                .base_elem = elem,
                .allocator = allocator,
                .css_parser = mparser,
                .iterators = its,
            };
        }

        pub fn next(self: *IteratorElementCssSelector) !?*Element {
            if (self.iterators.items.len == 0) {
                if (self.css_parser.pairs.items.len == 0) {
                    return null;
                }
                const pair = &self.css_parser.pairs.items[self.iterators.items.len];
                var new_it = try self.allocator.create(IteratorElement);
                new_it.* = try self.base_elem.iteratorElement(self.allocator, .descendant);
                try new_it.addFilterSelectorSlice(pair.selectors.items);
                try self.iterators.append(new_it);
            }
            var moving_cursor = self.iterators.items.len - 1;
            var it = self.iterators.items[moving_cursor];
            var el: ?*Element = null;

            while (moving_cursor >= 0 and moving_cursor < self.iterators.items.len) {
                it = self.iterators.items[moving_cursor];
                el = try it.next();
                if (el != null) {
                    if (moving_cursor == self.css_parser.pairs.items.len - 1) {
                        return el;
                    }
                    if (moving_cursor + 1 >= self.iterators.items.len) {
                        const pair = &self.css_parser.pairs.items[self.iterators.items.len];
                        var new_iterator_strat: IteratorStrategy = switch (pair.combinator) {
                            .descendant => IteratorStrategy.descendant,
                            .child => IteratorStrategy.child,
                            .subsequent_sibling => IteratorStrategy.child, // TODO: CHANGE
                            .next_sibling => IteratorStrategy.child, // TODO: CHANGE
                            else => IteratorStrategy.descendant,
                        };
                        var new_it = try self.allocator.create(IteratorElement);
                        new_it.* = try el.?.iteratorElement(self.allocator, new_iterator_strat);
                        try new_it.addFilterSelectorSlice(pair.selectors.items);
                        try self.iterators.append(new_it);
                    } else {
                        it = self.iterators.items[moving_cursor + 1];
                        try it.reuse(el.?);
                    }
                    moving_cursor += 1;
                    continue;
                }
                if (moving_cursor == 0) {
                    return null;
                }
                moving_cursor -= 1;
            }
            return null;
        }

        pub fn deinit(self: *IteratorElementCssSelector) void {
            for (self.iterators.items) |it| {
                it.deinit();
                self.allocator.destroy(it);
            }
            self.iterators.deinit();
            self.css_parser.deinit();
        }
    };

    const IteratorElement = struct {
        allocator: Allocator,
        base_elem: *Element,

        filtering_selectors: std.ArrayList(CssParser.Selector),
        filter_func: ?*const fn (elem: *const Element) bool = null,

        iterator_strategy: ?ItStrategy = null,

        pub fn init(allocator: Allocator, elem: *Element) IteratorElement {
            var sels = std.ArrayList(CssParser.Selector).init(allocator);
            return .{
                .filtering_selectors = sels,
                .base_elem = elem,
                .allocator = allocator,
            };
        }

        pub fn setStrategy(self: *IteratorElement, strategy: IteratorStrategy) !void {
            if (self.iterator_strategy != null) {
                self.iterator_strategy.?.deinit();
            }
            self.iterator_strategy = switch (strategy) {
                .descendant => .{ .descendant = try IteratorStyleDescendant.init(self.allocator, self.base_elem) },
                .child => .{ .child = IteratorStyleChild.init(self.base_elem) },
            };
        }

        pub fn restart(self: *IteratorElement) !void {
            if (self.iterator_strategy != null) {
                try self.iterator_strategy.?.restart();
            }
        }

        pub fn reuse(self: *IteratorElement, elem: *Element) !void {
            self.base_elem = elem;
            if (self.iterator_strategy != null) {
                try self.iterator_strategy.?.reuse(elem);
            }
        }

        pub fn deinit(self: *IteratorElement) void {
            if (self.iterator_strategy != null) {
                self.iterator_strategy.?.deinit();
            }

            for (self.filtering_selectors.items) |sel| {
                switch (sel) {
                    .attribute => |att| {
                        self.allocator.free(att.name);
                        self.allocator.free(att.value);
                    },
                    .pseudo_class => |pc| {
                        self.allocator.free(pc.name);
                    },
                    else => {},
                }
            }
            self.filtering_selectors.deinit();
        }

        //ok
        pub fn addFilterSelector(self: *IteratorElement, selector: CssParser.Selector) !void {
            var fil = try selector.clone(self.allocator);
            try self.filtering_selectors.append(fil);
        }

        //ok
        pub fn addFilterSelectorSlice(self: *IteratorElement, selectors: []CssParser.Selector) !void {
            for (selectors) |sel| {
                try self.addFilterSelector(sel);
            }
        }

        pub fn setFilterFunction(self: *IteratorElement, filter_func: ?*const fn (elem: *const Element) bool) !IteratorElement {
            self.filter_func = filter_func;
        }

        //ok
        pub fn filterAttribute(self: *IteratorElement, key: []const u8, value: []const u8, compare_type: CssParser.CompareType) !void {
            const key_dupe = try self.allocator.dupe(u8, key);
            const value_dupe = try self.allocator.dupe(u8, value);
            var selector = CssParser.SelectorAttribure{
                .name = key_dupe,
                .value = value_dupe,
                .compare_type = compare_type,
            };
            self.addFilterSelector(.{ .attribute = selector });
        }

        //ok
        pub fn filterElementType(self: *IteratorElement, element_type: ElementType) void {
            self.addFilterSelector(.{ .type = .{ .type = element_type } });
        }

        pub fn next(self: *IteratorElement) !?*Element {
            if (self.iterator_strategy == null) {
                return null;
            }
            while (true) {
                var el = try self.iterator_strategy.?.next();
                if (el == null) {
                    return null;
                }
                if (self.filterFunc(el.?)) {
                    return el.?;
                }
            }
        }

        fn filterFunc(self: *IteratorElement, elem: *const Element) bool {
            var res = true;
            if (self.filter_func) |ff| {
                res = ff(elem);
            }
            for (self.filtering_selectors.items) |selector| {
                res = res and elem.ofSelectorType(selector);
            }
            return res;
        }
    };

    pub fn iteratorElement(self: *Element, allocator: Allocator, strategy: ?IteratorStrategy) !IteratorElement {
        var iterator = IteratorElement.init(allocator, self);
        if (strategy) |strat| {
            try iterator.setStrategy(strat);
        }
        return iterator;
    }

    pub fn iteratorElementCssSelector(self: *Element, allocator: Allocator, css_selector: []const u8) !IteratorElementCssSelector {
        var iterator = try IteratorElementCssSelector.init(allocator, self, css_selector);
        return iterator;
    }

    pub fn ofSelectorType(self: *const Element, selector: CssParser.Selector) bool {
        switch (selector) {
            .attribute => |attr| {
                const attr_slice = self.attributes.slice();
                for (attr_slice.items(.key), attr_slice.items(.value)) |key, value| {
                    if (!std.mem.eql(u8, attr.name, key.local_name)) {
                        continue;
                    }
                    if (compareStrings(value, attr.value, attr.compare_type)) {
                        return true;
                    }
                }
            },
            .type => |x| {
                return self.element_type == x.type;
            },
            .pseudo_class => {
                std.debug.print("Function not implemented yet\n", .{});
            },
        }
        return false;
    }

    pub fn deinit(self: *Element, allocator: Allocator) void {
        const attr_slice = self.attributes.slice();
        for (attr_slice.items(.key), attr_slice.items(.value)) |key, value| {
            allocator.free(key.local_name);
            allocator.free(value);
        }
        self.attributes.deinit(allocator);
        self.children.deinit(allocator);
    }

    pub fn namespace(self: Element) Namespace {
        return self.element_type.namespace();
    }

    pub fn localName(self: *const Element, dom: *const Dom) []const u8 {
        return self.element_type.toLocalName() orelse dom.local_names.get(self) orelse unreachable;
    }

    pub fn numAttributes(self: Element) u32 {
        return @intCast(self.attributes.len);
    }

    pub fn appendAttribute(self: *Element, allocator: Allocator, key: ElementAttributesKey, value: []const u8) !void {
        // TOOD: This should implement https://dom.spec.whatwg.org/#concept-element-attributes-append
        const key_local_name_copy = try allocator.dupe(u8, key.local_name);
        errdefer allocator.free(key_local_name_copy);
        const value_copy = try allocator.dupe(u8, value);
        errdefer allocator.free(value_copy);
        try self.attributes.append(allocator, .{ .key = .{ .prefix = key.prefix, .namespace = key.namespace, .local_name = key_local_name_copy }, .value = value_copy });
    }

    pub fn appendAttributeIfNotExists(self: *Element, allocator: Allocator, key: ElementAttributesKey, value: []const u8) !void {
        if (self.getAttribute(key) == null) {
            try self.appendAttribute(allocator, key, value);
        }
    }

    pub fn getAttribute(self: Element, key: ElementAttributesKey) ?[]const u8 {
        const slice = self.attributes.slice();
        for (slice.items(.key), slice.items(.value)) |k, v| {
            if (key.eql(k)) {
                return v;
            }
        }
        return null;
    }

    pub fn lastChild(self: *Element) ?ElementOrCharacterData {
        if (self.children.items.len != 0) {
            return self.children.items[self.children.items.len - 1];
        } else {
            return null;
        }
    }

    pub fn indexOfChild(self: *Element, child: ElementOrCharacterData) ?usize {
        for (self.children.items, 0..) |c, i| {
            if (std.meta.eql(child, c)) return i;
        } else return null;
    }

    pub fn childBefore(self: *Element, child: ElementOrCharacterData) ?ElementOrCharacterData {
        if (self.children.items.len == 0) return null;
        if (std.meta.eql(self.children.items[0], child)) return null;
        const index = self.indexOfChild(child).?;
        return self.children.items[index - 1];
    }
};

pub const CharacterDataInterface = enum {
    // NOTE: CharacterData is an abstract interface.
    text,
    comment,
};

pub const CharacterData = struct {
    data: ArrayListUnmanaged(u8) = .{},
    interface: CharacterDataInterface,

    pub fn init(allocator: Allocator, data: []const u8, interface: CharacterDataInterface) !CharacterData {
        var result = CharacterData{ .interface = interface };
        try result.data.appendSlice(allocator, data);
        return result;
    }

    pub fn deinit(self: *CharacterData, allocator: Allocator) void {
        self.data.deinit(allocator);
    }

    // TODO: Move this function to mutation.
    pub fn append(self: *CharacterData, allocator: Allocator, data: []const u8) !void {
        try self.data.appendSlice(allocator, data);
    }
};
