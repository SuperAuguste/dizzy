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

    // Called to create valid case for first move which is off the board
    diagonal_lines_best_xs[max_depth + 1] = 0;

    for (0..max_depth + 1) |depth| {
        // -max_depth, -max_depth + 2 ... max_depth - 2, max_depth
        var diagonal: usize = max_depth - depth;
        while (diagonal <= max_depth + depth) : (diagonal += 2) {
            if (diagonal == max_depth - depth) {
                // "Most left" diagonal (bottom bound)
                // We can only go down from here

                // Going down means we do not change our X, so we take the X of the diagonal to the right ("above")
                // and store that as our current diagonal's X, showing that no change in X has occurred
                diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal + 1];
            } else if (diagonal == max_depth + depth) {
                // "Most right" diagonal (top bound)
                // We can only go right from here

                diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal - 1] + 1;
            } else {
                // We're not at the most left or most right; we must pick one of two options

                if (diagonal_lines_best_xs[diagonal - 1] == diagonal_lines_best_xs[diagonal + 1]) {
                    // Two options have equal x; to pick the closest to the bottom right,
                    // we pick the closest y to the bottom right, which is always the left diagonal

                    // Move right from left
                    diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal - 1] + 1;
                } else if (diagonal_lines_best_xs[diagonal - 1] < diagonal_lines_best_xs[diagonal + 1]) {
                    // Right diagonal has greater X, although it may be the same distance to the bottom right
                    // we prioritize X movement so we pick the right diagonal.

                    // Move down from up

                    diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal + 1];
                } else if (diagonal_lines_best_xs[diagonal - 1] > diagonal_lines_best_xs[diagonal + 1]) {
                    // Left diagonal has greater X, although it may be same distance to the bottom right
                    // we prioritize X movement so we pick the left diagonal.

                    // Move right from left
                    diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal - 1] + 1;
                }
            }

            var x = diagonal_lines_best_xs[diagonal];
            var y = x + max_depth - diagonal;

            while (x < a.len and y < b.len and a[x] == b[y]) {
                x += 1;
                y += 1;
                diagonal_lines_best_xs[diagonal] = x;
            }

            if (x >= a.len and y >= b.len) return depth;
        }
    }

    unreachable;
}

pub fn main() !void {
    var scratch_buf: [128]usize = undefined;
    std.log.info("{d}", .{calculateShortestEditDistance(u8, "aaaa", "bbbb", &scratch_buf)});
}
