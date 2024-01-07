const std = @import("std");

// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least (2 * (a.len + b.len) + 1) long
// Caller asserts that a and b are not both of length 0
pub fn calculateShortestEditDistance(
    comptime T: type,
    a: []const T,
    b: []const T,
    diagonal_lines_best_xs: []usize,
) usize {
    std.debug.assert(a.len != 0 or b.len != 0);

    const max_depth = a.len + b.len;
    // Number of diagonals, both positive or negative (some are off the board)
    const required_scratch_len = 2 * max_depth + 1;
    // -max_depth ... max_depth (0 is included, which is why the + 1 is present)
    std.debug.assert(diagonal_lines_best_xs.len >= required_scratch_len);

    if (std.debug.runtime_safety) {
        @memset(diagonal_lines_best_xs[0..required_scratch_len], undefined);
    }

    // Zig does not have signed indices for slices, so we just used unsigned integers
    // For example, if max_depth = 2 and I want to get diagonal_lines_best_xs[-1], I would
    // do diagonal_lines_best_xs[max_depth - 1], and likewise if I want diagonal_lines_best_xs[1]
    // I would do diagonal_lines_best_xs[max_depth + 1].

    // Called to create valid case for first move which is off the board.
    diagonal_lines_best_xs[max_depth + 1] = 0;

    // Our long-term objective is for whatever move we perform
    // to move towards the bottom right of the board. Our short-term
    // objective is for every move to move towards the current diagonal.

    for (0..max_depth + 1) |depth| {
        // -max_depth, -max_depth + 2 ... max_depth - 2, max_depth
        var diagonal: usize = max_depth - depth;
        while (diagonal <= max_depth + depth) : (diagonal += 2) {
            const should_go_right =
                // "Most right" diagonal (top bound); diagonal is to the
                // right of current position, so we can only go right from here.
                (diagonal == max_depth + depth or
                // If we arrive here, we must pick one of two cases (left or right); comparators:
                // == : left diagonal is closer to bottom right, so pick it (move right)
                // <  : right diagonal has higher X (move down)
                // >  : left diagonal has higher X (move right)
                diagonal_lines_best_xs[diagonal - 1] >= diagonal_lines_best_xs[diagonal + 1]) and
                // "Most left" diagonal (bottom bound); diagonal is below the
                // current position, so we can only go down from here.
                diagonal != max_depth - depth;

            if (should_go_right) {
                // Going right means that we're increasing X by one with our move originating
                // from the left and heading to our current diagonal. Thus, we need to copy
                // the diagonal to the left os us' X, which is diagonal - 1, and then add 1 to it.
                diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal - 1] + 1;
            } else {
                // Going down means that we're keeping the X the same with our move originating
                // from above and heading to our current diagonal. Thus, we need to copy the
                // diagonal above us' X, which is diagonal + 1.
                diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal + 1];
            }

            // Extract x and y from formula `diagonal = x - y`
            var x = diagonal_lines_best_xs[diagonal];
            var y = x + max_depth - diagonal;

            // Follow any snakes
            while (x < a.len and y < b.len and a[x] == b[y]) {
                x += 1;
                y += 1;
                diagonal_lines_best_xs[diagonal] = x;
            }

            // Are we at the bottom right corner?
            if (x >= a.len and y >= b.len) return depth;
        }
    }

    unreachable;
}

pub fn main() !void {
    var seed: u64 = undefined;
    try std.os.getrandom(std.mem.asBytes(&seed));

    var rng = std.rand.DefaultPrng.init(seed);

    var scratch_buf: [4097]usize = undefined;

    var str_a: [1024]u8 = undefined;
    rng.random().bytes(&str_a);

    var str_b: [1024]u8 = str_a;

    for (&str_b) |*c| {
        if (rng.random().boolean()) {
            c.* = rng.random().int(u8);
        }
    }

    var timer = try std.time.Timer.start();

    const a = calculateShortestEditDistance(u8, &str_a, &str_b, &scratch_buf);
    const a_time = timer.lap();

    std.debug.print("{d}\n", .{a});
    std.debug.print("{d}ns == {d}ms\n", .{ a_time, @as(f32, @floatFromInt(a_time)) / @as(f32, @floatFromInt(std.time.ns_per_ms)) });
}
