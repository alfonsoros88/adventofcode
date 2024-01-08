const std = @import("std");
const ArrayList = std.ArrayList;

const input_string = @embedFile("input.txt");

const Color = enum {
    red,
    green,
    blue,
};

const Sample = struct {
    red: u64 = 0,
    green: u64 = 0,
    blue: u64 = 0,
};

fn parseColor(line: []const u8) Color {
    if (std.mem.eql(u8, line, "red")) {
        return Color.red;
    } else if (std.mem.eql(u8, line, "green")) {
        return Color.green;
    } else if (std.mem.eql(u8, line, "blue")) {
        return Color.blue;
    } else {
        @panic("whoops");
    }
}

const GameParseError = std.mem.Allocator.Error || std.fmt.ParseIntError;
const Game = ArrayList(Sample);

fn parseGame(allocator: std.mem.Allocator, line: []const u8, game: *Game) GameParseError!void {
    game.* = Game.init(allocator);

    const pos = std.mem.indexOf(u8, line, ": ") orelse @panic("whoops");
    var samples = std.mem.splitSequence(u8, line[pos + 2 ..], "; ");
    while (samples.next()) |sample| {
        const current = try game.addOne();
        current.* = Sample{};

        var colors = std.mem.splitSequence(u8, sample, ", ");
        while (colors.next()) |color| {
            var words = std.mem.splitSequence(u8, color, " ");
            const count_str = words.next() orelse @panic("whoops");
            const color_str = words.next() orelse @panic("whoops");

            const count = try std.fmt.parseUnsigned(u64, count_str, 10);
            switch (parseColor(color_str)) {
                Color.red => {
                    current.*.red = count;
                },
                Color.green => {
                    current.*.green = count;
                },
                Color.blue => {
                    current.*.blue = count;
                },
            }
        }
    }
}

const Games = ArrayList(Game);

fn isPossible(game: Game) bool {
    const max_red = 12;
    const max_green = 13;
    const max_blue = 14;
    for (game.items) |sample| {
        if (sample.red > max_red) {
            return false;
        } else if (sample.green > max_green) {
            return false;
        } else if (sample.blue > max_blue) {
            return false;
        }
    }
    return true;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var iter = std.mem.splitSequence(u8, input_string, "\n");

    var games = Games.init(allocator);
    defer {
        for (games.items) |game| {
            game.deinit();
        }
        games.deinit();
    }

    while (iter.next()) |line| {
        if (line.len == 0) {
            break;
        }

        const game = try games.addOne();
        try parseGame(allocator, line, game);
    }

    var possible_sum: u64 = 0;
    for (games.items, 1..) |game, index| {
        if (isPossible(game)) {
            possible_sum += index;
        }
    }

    std.debug.print("part 1: {}\n", .{possible_sum});

    var part_two: u64 = 0;
    for (games.items) |game| {
        var min_red: u64 = 0;
        var min_green: u64 = 0;
        var min_blue: u64 = 0;

        for (game.items) |sample| {
            min_red = @max(sample.red, min_red);
            min_green = @max(sample.green, min_green);
            min_blue = @max(sample.blue, min_blue);
        }

        part_two += min_red * min_green * min_blue;
    }

    std.debug.print("part 2: {}\n", .{part_two});
}
