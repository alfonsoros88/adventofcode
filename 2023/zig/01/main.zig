const std = @import("std");

const input_string = @embedFile("input.txt");

fn first_number(line: []const u8) u64 {
    for (line) |char| {
        if (std.ascii.isDigit(char)) {
            return char - '0';
        }
    }
    return 0;
}

fn last_number(line: []const u8) u64 {
    var i: usize = line.len;
    while (i >= 1) {
        const char = line[i - 1];
        if (std.ascii.isDigit(char)) {
            return char - '0';
        }
        i -= 1;
    }
    return 0;
}

const number_str = [10][]const u8{
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
};

fn first_number_revisited(line: []const u8) u64 {
    if (line.len == 0) {
        return 0;
    }

    const char = line[0];
    if (std.ascii.isDigit(char)) {
        return char - '0';
    }

    const len = line.len;
    for (number_str, 0..) |number, index| {
        const num_len = number.len;
        if (len >= num_len and std.mem.eql(u8, line[0..num_len], number)) {
            return index;
        }
    }

    return first_number_revisited(line[1..]);
}

fn last_number_revisited(line: []const u8) u64 {
    if (line.len == 0) {
        return 0;
    }

    const len = line.len;
    const char = line[len - 1];
    if (std.ascii.isDigit(char)) {
        return char - '0';
    }

    for (number_str, 0..) |number, index| {
        const num_len = number.len;
        if (len >= num_len and std.mem.eql(u8, line[len - num_len ..], number)) {
            return index;
        }
    }

    return last_number_revisited(line[0 .. len - 1]);
}

pub fn main() !void {
    var iter = std.mem.split(u8, input_string, "\n");
    var sum: u64 = 0;
    var sum2: u64 = 0;
    while (iter.next()) |line| {
        const fst = first_number(line);
        const lst = last_number(line);

        const fst2 = first_number_revisited(line);
        const lst2 = last_number_revisited(line);

        sum += fst * 10 + lst;
        sum2 += fst2 * 10 + lst2;
    }
    std.debug.print("sum part1: {}\n", .{sum});
    std.debug.print("sum part2: {}\n", .{sum2});
}
