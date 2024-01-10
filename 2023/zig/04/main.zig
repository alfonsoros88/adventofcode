const std = @import("std");

const input_string = @embedFile("input.txt");

fn matchingCount(allocator: std.mem.Allocator, line: []const u8) usize {
    const parse_start = std.mem.indexOf(u8, line, ": ").? + 1;
    var it = std.mem.tokenizeSequence(u8, line[parse_start..], " ");
    var score: usize = 0;
    var is_left_side: bool = true;

    var map = std.AutoHashMap(u64, bool).init(allocator);
    defer map.deinit();

    while (it.next()) |word| {
        if (std.mem.eql(u8, word, "|")) {
            is_left_side = false;
        } else {
            const num = std.fmt.parseUnsigned(u64, word, 10) catch unreachable;
            if (is_left_side) {
                map.put(num, true) catch unreachable;
            } else {
                if (map.get(num) != null) {
                    score += 1;
                }
            }
        }
    }

    return score;
}

fn cardScore(matching_count: usize) usize {
    var score: usize = 0;
    if (matching_count > 0) {
        score = @as(u64, 1) << @truncate(matching_count - 1);
    }
    return score;
}

fn cardId(line: []const u8) u64 {
    const id_start = line[std.mem.indexOfAny(u8, line, "0123456789").?..];
    const id_end = std.mem.indexOf(u8, id_start, ":").?;
    return std.fmt.parseUnsigned(u64, id_start[0..id_end], 10) catch unreachable;
}

pub fn main() !void {
    var iter = std.mem.tokenizeSequence(u8, input_string, "\n");

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var card_count = std.AutoHashMap(u64, u64).init(allocator);
    defer card_count.deinit();

    var total_score: usize = 0;
    while (iter.next()) |line| {
        std.debug.print("{s}\n", .{line});
        const card_id = cardId(line);
        const matching_count = matchingCount(allocator, line);
        const card_score = cardScore(matching_count);

        const card_entry = card_count.getOrPutValue(card_id, 0) catch unreachable;
        card_entry.value_ptr.* += 1;
        const card_current_count = card_entry.value_ptr.*;

        for (0..matching_count) |count| {
            const entry = card_count.getOrPutValue(card_id + count + 1, 0) catch unreachable;
            entry.value_ptr.* += 1 * card_current_count;
        }

        std.debug.print("Card {} score: {} - count: {}\n", .{ card_id, card_score, card_entry.value_ptr.* });
        total_score += card_score;
    }

    var count_it = card_count.valueIterator();
    var total_card_count: usize = 0;
    while (count_it.next()) |count| {
        total_card_count += @truncate(count.*);
    }

    std.debug.print("part 1: {}\n", .{total_score});
    std.debug.print("part 2: {}\n", .{total_card_count});
}
