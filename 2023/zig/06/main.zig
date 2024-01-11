const std = @import("std");

const input_string = @embedFile("input.txt");

fn countColumns() usize {
    const start = std.mem.indexOf(u8, input_string, ":").? + 1;
    const end = std.mem.indexOf(u8, input_string, "\n").?;
    var it = std.mem.tokenizeSequence(u8, input_string[start..end], " ");
    var size: usize = 0;
    while (it.next() != null) {
        size += 1;
    }
    return size;
}

const RACES: usize = countColumns();

const TIMES: [RACES]u64 = init: {
    var it = std.mem.tokenizeSequence(u8, input_string, "\n");
    const times = it.next().?;

    var times_it = std.mem.tokenizeSequence(u8, times, " ");
    _ = times_it.next(); // ignore header

    var times_array: [RACES]u64 = undefined;
    for (&times_array) |*item| {
        item.* = std.fmt.parseUnsigned(u64, times_it.next().?, 10) catch unreachable;
    }

    break :init times_array;
};

const DISTANCES: [RACES]u64 = init: {
    var it = std.mem.tokenizeSequence(u8, input_string, "\n");
    _ = it.next().?;
    const distances = it.next().?;

    var distances_it = std.mem.tokenizeSequence(u8, distances, " ");
    _ = distances_it.next(); // ignore header

    var distances_array: [RACES]u64 = undefined;
    for (&distances_array) |*item| {
        item.* = std.fmt.parseUnsigned(u64, distances_it.next().?, 10) catch unreachable;
    }

    break :init distances_array;
};

fn wins(total_time: u64, distance: u64) usize {
    var count: usize = 0;
    for (0..total_time) |t| {
        const d = (total_time - t) * t;
        if (d > distance) {
            count += 1;
        }
    }
    return count;
}

fn countWins(r: usize) usize {
    const time = TIMES[r];
    const dist = DISTANCES[r];
    return wins(time, dist);
}

fn concatNumbers(numbers: []const u64) u64 {
    var result: u64 = 0;
    for (numbers) |number| {
        const digits = std.fmt.count("{}", .{number});
        result *= std.math.powi(u64, 10, digits) catch unreachable;
        result += number;
    }
    return result;
}

pub fn main() !void {
    var count_wins: usize = 1;
    for (0..RACES) |r| {
        count_wins *= countWins(r);
    }

    std.debug.print("part 1: {}\n", .{count_wins});

    const actual_time = concatNumbers(&TIMES);
    const actual_distance = concatNumbers(&DISTANCES);
    const real_count_wins = wins(actual_time, actual_distance);

    std.debug.print("part 2: {}\n", .{real_count_wins});
}
