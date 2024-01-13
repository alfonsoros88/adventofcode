const std = @import("std");

const input_string = @embedFile("input.txt");

const Hand = struct {
    cards: []const u8,
    bid: u64,
    hand_score: u64 = 0,
};

fn countLines(text: []const u8) usize {
    var size: usize = 0;
    var it = std.mem.tokenizeSequence(u8, text, "\n");
    while (it.next() != null) : (size += 1) {}
    return size;
}

const cardTypes: []const u8 = "23456789TJQKA";

fn cardValue(card: u8) usize {
    return std.mem.indexOfScalar(u8, cardTypes, card).?;
}

fn cardValueWithJoker(card: u8) usize {
    return std.mem.indexOfScalar(u8, "J23456789TQKA", card).?;
}

fn cardCompFunc(comptime val: fn (u8) usize) fn (void, Hand, Hand) bool {
    return struct {
        pub fn inner(_: void, a: Hand, b: Hand) bool {
            if (a.hand_score != b.hand_score) {
                return a.hand_score < b.hand_score;
            }
            for (a.cards, b.cards) |l, r| {
                const l_val = val(l);
                const r_val = val(r);
                if (l_val != r_val) {
                    return l_val < r_val;
                }
            }
            return false;
        }
    }.inner;
}

fn handScore(cards: []const u8) u64 {
    var card_count = [_]u64{0} ** cardTypes.len;
    for (cards) |card| {
        const idx = cardValue(card);
        card_count[idx] += 1;
    }

    var score: u64 = 0;
    for (card_count) |count| {
        switch (count) {
            5 => {
                score += 1000;
            },
            4 => {
                score += 900;
            },
            3 => {
                score += 500;
            },
            2 => {
                score += 200;
            },
            else => {},
        }
    }
    return score;
}

fn parseHands(allocator: std.mem.Allocator) std.ArrayList(Hand) {
    var it = std.mem.tokenizeAny(u8, input_string, " \n");

    var hands = std.ArrayList(Hand).init(allocator);

    while (it.next()) |cards| {
        const hand = hands.addOne() catch unreachable;
        const bid = std.fmt.parseUnsigned(u64, it.next().?, 10) catch unreachable;
        const hand_score = handScore(cards);
        hand.cards = cards;
        hand.bid = bid;
        hand.hand_score = hand_score;
    }

    return hands;
}

fn totalScore(hands: []const Hand) u64 {
    var score: u64 = 0;
    for (hands, 1..) |hand, rank| {
        score += hand.bid * rank;
    }
    return score;
}

fn handScoreWithJoker(cards: []const u8) u64 {
    var card_count = [_]u64{0} ** cardTypes.len;
    var joker_count: usize = 0;
    for (cards) |card| {
        if (card == 'J') {
            joker_count += 1;
        } else {
            const idx = cardValue(card);
            card_count[idx] += 1;
        }
    }

    std.mem.sort(u64, &card_count, {}, comptime std.sort.desc(u64));
    card_count[0] += joker_count;

    var score: u64 = 0;
    for (card_count) |count| {
        switch (count) {
            5 => {
                score += 1000;
            },
            4 => {
                score += 900;
            },
            3 => {
                score += 500;
            },
            2 => {
                score += 200;
            },
            else => {},
        }
    }
    return score;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var hands = parseHands(allocator);
    defer hands.deinit();

    std.mem.sort(Hand, hands.items, {}, comptime cardCompFunc(cardValue));

    const total_score = totalScore(hands.items);
    std.debug.print("part 1: {}\n", .{total_score});

    for (hands.items) |*hand| {
        hand.hand_score = handScoreWithJoker(hand.cards);
    }

    std.mem.sort(Hand, hands.items, {}, comptime cardCompFunc(cardValueWithJoker));

    const total_score_with_joker = totalScore(hands.items);
    std.debug.print("part 2: {}\n", .{total_score_with_joker});
}
