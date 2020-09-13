zben
======
Zig Bencode Parser
---------

Super basic Bencode parsing into a bencode Value which could be a Integer, String, List, or Dictionary.

There are a couple happy path tests but that's about it.

### Example Usage
```
const zben = @import("zben");
const gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = &gpa.allocator;

pub fn main() !void {
    const str = "d4:testi50ee";
    var parser = zben.Parser.initWithData(alloc, str);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // do things with the tree.root
}
```