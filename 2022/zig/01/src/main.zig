const std = @import("std");

fn getInputFilePathFromArgs() ![]const u8 {
    var iter = std.process.ArgIteratorPosix.init();
    _ = iter.skip(); // skip the program name
    return iter.next() orelse error.ArgMissing;
}

fn openInputFile(path: []const u8) !std.fs.File {
    const cwd = std.fs.cwd();
    return cwd.openFile(path, .{ .mode = std.fs.File.OpenMode.read_only });
}

pub fn main() !void {
    const input_file_path = try getInputFilePathFromArgs();
    const input_file = try openInputFile(input_file_path);
    defer input_file.close();

    var elfs = [_]i32{0} ** 256;
    var elfs_count: usize = 0;

    var line = [_]u8{0} ** 256;
    var file_reader = std.io.bufferedReader(input_file.reader());
    while (try file_reader.reader().readUntilDelimiterOrEof(&line, '\n')) |string| {
        if (std.fmt.parseInt(i32, string, 10)) |number| {
            elfs[elfs_count] += number;
        } else |_| {
            elfs_count += 1;
        }
    }

    std.sort.sort(i32, &elfs, {}, comptime std.sort.desc(i32));

    const part1 = elfs[0];
    std.debug.print("part 1: {}\n", .{part1});

    var part2: i32 = 0;
    for (elfs[0..3]) |i| {
        part2 += i;
    }

    std.debug.print("part 2: {}\n", .{part2});
}
