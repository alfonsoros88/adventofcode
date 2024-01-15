const std = @import("std");

const input_string = @embedFile("input.txt");

fn iterCount(it: anytype) usize {
    var count: usize = 0;
    while (it.next()) |_| {
        count += 1;
    }
    return count;
}

const rows: usize = init: {
    @setEvalBranchQuota(1000000);
    var it = std.mem.tokenizeSequence(u8, input_string, "\n");
    break :init iterCount(&it);
};
const cols: usize = init: {
    @setEvalBranchQuota(1000000);
    var it = std.mem.tokenizeSequence(u8, input_string, "\n");
    const first_line = it.next().?;
    var col_it = std.mem.tokenizeAny(u8, first_line, " ");
    break :init iterCount(&col_it);
};

const measurements: [rows][cols]i64 = init: {
    @setEvalBranchQuota(1000000);
    var array: [rows][cols]i64 = undefined;
    var numbers = std.mem.tokenizeAny(u8, input_string, " \n");
    for (0..rows) |row| {
        for (0..cols) |col| {
            array[row][col] = std.fmt.parseInt(i64, numbers.next().?, 10) catch unreachable;
        }
    }
    break :init array;
};

fn extrapolate(values: []const i64) i64 {
    var diffs: [cols - 1]i64 = undefined;

    var all_zeroes = true;
    for (0..values.len - 1) |i| {
        diffs[i] = values[i + 1] - values[i];
        if (diffs[i] != 0) {
            all_zeroes = false;
        }
    }

    if (all_zeroes) {
        return values[0];
    }

    return extrapolate(diffs[0 .. values.len - 1]) + values[values.len - 1];
}

fn extrapolate_backwards(values: []const i64) i64 {
    var diffs: [cols - 1]i64 = undefined;

    var all_zeroes = true;
    for (0..values.len - 1) |i| {
        diffs[i] = values[i + 1] - values[i];
        if (diffs[i] != 0) {
            all_zeroes = false;
        }
    }

    if (all_zeroes) {
        return values[0];
    }

    return values[0] - extrapolate_backwards(diffs[0 .. values.len - 1]);
}

pub fn main() !void {
    var sum: i64 = 0;
    var sum_backwards: i64 = 0;

    for (measurements) |row| {
        sum += extrapolate(&row);
        sum_backwards += extrapolate_backwards(&row);
    }

    std.debug.print("part 1: {}\n", .{sum});
    std.debug.print("part 2: {}\n", .{sum_backwards});
}
