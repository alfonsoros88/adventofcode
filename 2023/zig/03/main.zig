const std = @import("std");
const ArrayList = std.ArrayList;

const input_string: []const u8 = @embedFile("input.txt");
const cols: usize = std.mem.indexOf(u8, input_string, "\n").?;
const rows: usize = input_string.len / cols - 1;

fn index(r: usize, c: usize) usize {
    return (cols + 1) * r + c;
}

fn cell(r: usize, c: usize) u8 {
    return input_string[index(r, c)];
}

const Rect = struct {
    x: i64,
    y: i64,
};

fn hasSymbol(a: Rect, b: Rect) bool {
    const start_row: usize = @intCast(@max(a.x, 0));
    const end_row: usize = @intCast(@min(b.x, rows));
    const start_col: usize = @intCast(@max(a.y, 0));
    const end_col: usize = @intCast(@min(b.y, cols));

    for (start_row..end_row) |row| {
        for (start_col..end_col) |col| {
            switch (cell(row, col)) {
                '0'...'9', '.' => {},
                else => {
                    return true;
                },
            }
        }
    }
    return false;
}

fn asi64(num: anytype) i64 {
    return @intCast(num);
}

fn asusize(num: anytype) usize {
    return @intCast(num);
}

fn dec(val: usize) usize {
    return @max(asusize(asi64(val) - 1), 0);
}

fn inc(val: usize, max: usize) usize {
    return @min(asusize(asi64(val) + 2), max);
}

fn numberStart(idx: usize) usize {
    var i = idx;
    while (i > 0) : (i -= 1) {
        switch (input_string[i]) {
            '0'...'9' => {
                continue;
            },
            else => {
                return i + 1;
            },
        }
    }
    return 0;
}

fn gearRatio(row: usize, col: usize) usize {
    const r1 = dec(row);
    const r2 = inc(row, rows);
    const c1 = dec(col);
    const c2 = inc(col, cols);

    var numbers = [_]?u64{ null, null };
    for (r1..r2) |r| {
        var c = c1;
        while (c < c2) : (c += 1) {
            switch (cell(r, c)) {
                '0'...'9' => {
                    const idx = index(r, c);
                    const num_start = numberStart(idx);
                    const num_end = std.mem.indexOfNonePos(u8, input_string, idx, "0123456789").?;
                    const number_str = input_string[num_start..num_end];
                    const number: u64 = std.fmt.parseUnsigned(u64, number_str, 10) catch unreachable;

                    if (numbers[0] == null) {
                        numbers[0] = number;
                    } else if (numbers[1] == null) {
                        numbers[1] = number;
                    } else {
                        return 0;
                    }

                    if (num_end - 1 > idx) {
                        break;
                    }
                },
                else => {},
            }
        }
    }

    if (numbers[0] != null and numbers[1] != null) {
        return numbers[0].? * numbers[1].?;
    } else {
        return 0;
    }
}

pub fn main() !void {
    var iter = std.mem.split(u8, input_string, "\n");

    while (iter.next()) |line| {
        if (line.len == 0) {
            break;
        }
    }

    var part_one: usize = 0;
    var part_two: usize = 0;

    for (0..rows) |row| {
        var col: usize = 0;
        while (col < cols) : (col += 1) {
            const c = cell(row, col);
            switch (c) {
                '0'...'9' => {
                    const start_index = index(row, col);
                    const ul = Rect{
                        .x = @as(i64, @intCast(row)) - 1,
                        .y = @as(i64, @intCast(col)) - 1,
                    };

                    const end_index = std.mem.indexOfNonePos(u8, input_string, start_index, "0123456789").?;
                    const number_str = input_string[start_index..end_index];
                    const number = try std.fmt.parseUnsigned(u64, number_str, 10);

                    col += end_index - start_index - 1;
                    const lr = Rect{
                        .x = @as(i64, @intCast(row)) + 2,
                        .y = @as(i64, @intCast(col)) + 2,
                    };

                    if (hasSymbol(ul, lr)) {
                        part_one += number;
                    } else {}
                },
                '*' => {
                    // find two numbers
                    part_two += gearRatio(row, col);
                },
                else => {},
            }
        }
    }

    std.debug.print("part 1: {}\n", .{part_one});
    std.debug.print("part 2: {}\n", .{part_two});
}
