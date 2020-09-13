zben
======
Zig Bencode Parser
---------

Super basic Bencode parsing into a bencode Value which could be a Integer, String, List, or Dictionary.

Got the decoding done only to find zig bencode encoding/decoding libraries exist :) so will probably abandon this now.

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