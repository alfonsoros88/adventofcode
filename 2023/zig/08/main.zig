const std = @import("std");

const input_string = @embedFile("input.txt");

fn Pair(comptime T: type) type {
    return struct {
        left: T,
        right: T,
    };
}

fn makePair(comptime T: type, left: T, right: T) Pair(T) {
    return Pair(T){
        .left = left,
        .right = right,
    };
}

const Map = std.StringHashMap(Pair([]const u8));

fn parseNode(allocator: std.mem.Allocator, input: []const u8) Map {
    var nodes = std.StringHashMap(Pair([]const u8)).init(allocator);

    var it = std.mem.tokenizeAny(u8, input, " =(,)\n");
    while (it.next()) |start| {
        const left = it.next().?;
        const right = it.next().?;
        nodes.put(start, makePair([]const u8, left, right)) catch unreachable;
    }

    return nodes;
}

const LoopIterator = struct {
    slice: []const u8,
    cursor: usize,

    fn next(self: *LoopIterator) ?u8 {
        if (self.cursor >= self.slice.len) {
            self.cursor = 0;
        }
        const c = self.slice[self.cursor];
        self.cursor += 1;
        return c;
    }

    fn init(slice: []const u8) LoopIterator {
        return .{
            .slice = slice,
            .cursor = 0,
        };
    }
};

fn stepsToReachZZZ(instructions: []const u8, nodes: Map) usize {
    var steps: usize = 0;

    var loop = LoopIterator.init(instructions);
    var current_node: []const u8 = "AAA";
    while (loop.next()) |choice| {
        if (std.mem.eql(u8, current_node, "ZZZ")) {
            break;
        }

        switch (choice) {
            'L' => {
                current_node = nodes.get(current_node).?.left;
                steps += 1;
            },
            'R' => {
                current_node = nodes.get(current_node).?.right;
                steps += 1;
            },
            else => unreachable,
        }
    }

    return steps;
}

fn allEndWithZ(nodes: []const []const u8) bool {
    for (nodes) |name| {
        if (!std.mem.endsWith(u8, name, "Z")) {
            return false;
        }
    }
    return true;
}

fn stepsCountParallel(allocator: std.mem.Allocator, instructions: []const u8, map: Map) usize {
    var cursors = std.ArrayList([]const u8).init(allocator);
    defer cursors.deinit();

    var key_it = map.keyIterator();
    while (key_it.next()) |name| {
        if (std.mem.endsWith(u8, name.*, "A")) {
            cursors.append(name.*) catch unreachable;
        }
    }

    var steps: usize = 0;
    var it = LoopIterator.init(instructions);
    while (it.next()) |choice| {
        var break_loop = true;

        for (cursors.items) |*cursor| {
            switch (choice) {
                'L' => {
                    cursor.* = map.get(cursor.*).?.left;
                },
                'R' => {
                    cursor.* = map.get(cursor.*).?.right;
                },
                else => unreachable,
            }

            if (!std.mem.endsWith(u8, cursor.*, "Z")) {
                break_loop = false;
            }
        }
        steps += 1;

        if (break_loop) {
            break;
        }

        if (steps % 1000000 == 0) {
            std.debug.print("steps: {}\n", .{steps});
        }
    }

    return steps;
}

fn gcd(a: u64, b: u64) u64 {
    if (a == 0) {
        return b;
    } else {
        return gcd(b % a, a);
    }
}

fn lcm(a: u64, b: u64) u64 {
    return a / gcd(a, b) * b;
}

fn actualSolutionForSomeReason(allocator: std.mem.Allocator, instructions: []const u8, map: Map) usize {
    const Cursor = struct {
        value: []const u8,
        update: bool = true,
    };
    var cursors = std.ArrayList(Cursor).init(allocator);
    defer cursors.deinit();

    var key_it = map.keyIterator();
    while (key_it.next()) |name| {
        if (std.mem.endsWith(u8, name.*, "A")) {
            cursors.append(Cursor{ .value = name.* }) catch unreachable;
        }
    }
    const cursor_count = cursors.items.len;

    var counts = std.ArrayList(u64).init(allocator);
    defer counts.deinit();
    counts.ensureTotalCapacity(cursor_count) catch unreachable;

    var it = LoopIterator.init(instructions);
    var steps: usize = 0;
    while (counts.items.len < cursor_count) {
        steps += 1;
        const choice = it.next().?;

        for (cursors.items) |*cursor| {
            if (!cursor.update) {
                continue;
            }

            switch (choice) {
                'L' => {
                    cursor.value = map.get(cursor.value).?.left;
                },
                'R' => {
                    cursor.value = map.get(cursor.value).?.right;
                },
                else => unreachable,
            }

            if (std.mem.endsWith(u8, cursor.value, "Z")) {
                cursor.update = false;
                counts.append(@as(u64, steps)) catch unreachable;
            }
        }
    }

    var lcm_val = counts.items[0];
    for (1..counts.items.len) |i| {
        lcm_val = lcm(lcm_val, counts.items[i]);
    }

    return lcm_val;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var it = std.mem.tokenizeSequence(u8, input_string, "\n\n");

    const instructions = std.mem.trim(u8, it.next().?, "\n");

    var nodes = parseNode(allocator, it.next().?);
    defer nodes.deinit();

    std.debug.print("part 1: {}\n", .{stepsToReachZZZ(instructions, nodes)});
    std.debug.print("part 2: {}\n", .{actualSolutionForSomeReason(allocator, instructions, nodes)});
}
