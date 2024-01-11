const std = @import("std");

const input_string = @embedFile("input.txt");

const Range = struct {
    src: usize,
    dst: usize,
    size: usize,

    const Self = @This();

    fn map(self: Self, in: usize) ?usize {
        if (in >= self.src and in <= self.src + self.size) {
            const delta = in - self.src;
            return self.dst + delta;
        }
        return null;
    }

    fn invMap(self: Self, out: usize) ?usize {
        if (out >= self.dst and out <= self.dst + self.size) {
            const delta = out - self.dst;
            return self.src + delta;
        }
        return null;
    }
};

fn parseUsize(str: []const u8) usize {
    return std.fmt.parseUnsigned(usize, str, 10) catch unreachable;
}

fn parseRange(line: []const u8) Range {
    var it = std.mem.tokenizeSequence(u8, line, " ");
    return Range{
        .dst = parseUsize(it.next().?),
        .src = parseUsize(it.next().?),
        .size = parseUsize(it.next().?),
    };
}

const Map = struct {
    ranges: std.ArrayList(Range),

    const Self = @This();

    fn map(self: Self, seed: usize) usize {
        for (self.ranges.items) |range| {
            if (range.map(seed)) |val| {
                return val;
            }
        }
        return seed;
    }

    fn invMap(self: Self, out: usize) usize {
        for (self.ranges.items) |range| {
            if (range.invMap(out)) |val| {
                return val;
            }
        }
        return out;
    }

    fn deinit(self: Self) void {
        self.ranges.deinit();
    }
};

fn parseMap(allocator: std.mem.Allocator, line: []const u8) Map {
    var it = std.mem.tokenizeSequence(u8, line, "\n");
    _ = it.next(); // skip title

    var ranges = std.ArrayList(Range).init(allocator);
    while (it.next()) |range| {
        const new_range = ranges.addOne() catch unreachable;
        new_range.* = parseRange(range);
    }

    return Map{ .ranges = ranges };
}

fn parseSeeds(allocator: std.mem.Allocator, line: []const u8) std.ArrayList(usize) {
    const start = std.mem.indexOf(u8, line, ":").? + 1;
    var seeds = std.ArrayList(usize).init(allocator);

    var it = std.mem.tokenizeSequence(u8, line[start..], " ");
    while (it.next()) |elem| {
        const num = std.fmt.parseUnsigned(usize, elem, 10) catch unreachable;
        seeds.append(num) catch unreachable;
    }

    return seeds;
}

fn mapSeed(seed: usize, maps: []const Map) usize {
    var current = seed;
    for (maps) |map| {
        current = map.map(current);
    }
    return current;
}

fn locationToSeed(location: usize, maps: []const Map) usize {
    var idx = maps.len;
    var current: usize = location;
    while (idx > 0) : (idx -= 1) {
        current = maps[idx - 1].invMap(current);
    }
    return current;
}

fn inSeedRange(seed: usize, seeds: []const usize) bool {
    var i: usize = 0;
    while (i < seeds.len) : (i += 2) {
        const base = seeds[i];
        const size = seeds[i + 1];
        if (seed > base and seed <= base + size) {
            return true;
        }
    }
    return false;
}

pub fn main() !void {
    var iter = std.mem.tokenizeSequence(u8, input_string, "\n\n");

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var maps = std.ArrayList(Map).init(allocator);
    defer {
        for (maps.items) |item| {
            item.deinit();
        }
        maps.deinit();
    }

    const seeds = parseSeeds(allocator, iter.next().?);
    defer seeds.deinit();

    while (iter.next()) |block| {
        const new_map = maps.addOne() catch unreachable;
        new_map.* = parseMap(allocator, block);
    }

    var min_location: usize = std.math.maxInt(usize);
    for (seeds.items) |seed| {
        const location = mapSeed(seed, maps.items);
        min_location = @min(min_location, location);
    }

    std.debug.print("part 1: {}\n", .{min_location});

    // part 2
    var location: usize = 0;
    while (true) : (location += 1) {
        const seed = locationToSeed(location, maps.items);
        if (inSeedRange(seed, seeds.items)) {
            std.debug.print("part 2: {}\n", .{location});
            break;
        }
    }
}
