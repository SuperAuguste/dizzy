const std = @import("std");

/// Context must contain:
///   - fn eql(context: Context, a: T, b: T) bool
pub fn SesFinder(comptime T: type, comptime Context: type) type {
    return struct {
        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least (2 * (a.len + b.len) + 1) long
        pub fn calculateLength(
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
        ) u32 {
            if (std.meta.fields(Context).len != 0) @compileError("You must call `calculateLengthContext`.");
            return calculateLengthContext(.{}, a, b, diagonal_lines_best_xs);
        }

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least (2 * (a.len + b.len) + 1) long
        pub fn calculateLengthContext(
            context: Context,
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
        ) u32 {
            if (a.len == 0 and b.len == 0) return 0;

            const max_depth: u32 = @intCast(a.len + b.len);
            // Number of diagonals, both positive or negative (some are off the board)
            const required_scratch_len: u32 = 2 * max_depth + 1;
            // -max_depth ... max_depth (0 is included, which is why the + 1 is present)
            std.debug.assert(diagonal_lines_best_xs.len >= required_scratch_len);

            // Zig does not have signed indices for slices, so we just used unsigned integers
            // For example, if max_depth = 2 and I want to get diagonal_lines_best_xs[-1], I would
            // do diagonal_lines_best_xs[max_depth - 1], and likewise if I want diagonal_lines_best_xs[1]
            // I would do diagonal_lines_best_xs[max_depth + 1].

            // Called to create valid case for first move which is off the board.
            diagonal_lines_best_xs[max_depth + 1] = 0;

            // Our long-term objective is for whatever move we perform
            // to move towards the bottom right of the board. Our short-term
            // objective is for every move to move towards the current diagonal.

            var depth: u32 = 0;
            while (depth <= max_depth) : (depth += 1) {
                // -max_depth, -max_depth + 2 ... max_depth - 2, max_depth
                var diagonal: u32 = max_depth - depth;
                while (diagonal <= max_depth + depth) : (diagonal += 2) {
                    const should_go_right =
                        // "Most left" diagonal (bottom bound); diagonal is below the
                        // current position, so we can only go down from here.
                        diagonal != max_depth - depth and
                        // "Most right" diagonal (top bound); diagonal is to the
                        // right of current position, so we can only go right from here.
                        (diagonal == max_depth + depth or
                        // If we arrive here, we must pick one of two cases (left or right); comparators:
                        // == : left diagonal is closer to bottom right, so pick it (move right)
                        // <  : right diagonal has higher X (move down)
                        // >  : left diagonal has higher X (move right)
                        diagonal_lines_best_xs[diagonal - 1] >= diagonal_lines_best_xs[diagonal + 1]);

                    if (should_go_right) {
                        // Going right means that we're increasing X by one with our move originating
                        // from the left and heading to our current diagonal. Thus, we need to copy
                        // the diagonal to the left of us' X, which is diagonal - 1, and then add 1 to it.
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
                    while (x < a.len and y < b.len and context.eql(a[x], b[y])) {
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

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least (2 * (a.len + b.len) + 1) long
        /// Result should be identical to `calculateLength`
        pub fn calculateReverseLength(
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
        ) u32 {
            if (std.meta.fields(Context).len != 0) @compileError("You must call `calculateReverseLengthContext`.");
            return calculateReverseLengthContext(.{}, a, b, diagonal_lines_best_xs);
        }

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least (2 * (a.len + b.len) + 1) long
        /// Result should be identical to `calculateLengthContext`
        pub fn calculateReverseLengthContext(
            context: Context,
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
        ) u32 {
            if (a.len == 0 and b.len == 0) return 0;

            const max_depth: u32 = @intCast(a.len + b.len);
            // Number of diagonals, both positive or negative (some are off the board)
            const required_scratch_len: u32 = 2 * max_depth + 1;
            // -max_depth ... max_depth (0 is included, which is why the + 1 is present)
            std.debug.assert(diagonal_lines_best_xs.len >= required_scratch_len);

            // Zig does not have signed indices for slices, so we just used unsigned integers
            // For example, if max_depth = 2 and I want to get diagonal_lines_best_xs[-1], I would
            // do diagonal_lines_best_xs[max_depth - 1], and likewise if I want diagonal_lines_best_xs[1]
            // I would do diagonal_lines_best_xs[max_depth + 1].

            // Called to create valid case for first move which is off the board.
            diagonal_lines_best_xs[max_depth - 1] = @intCast(a.len);

            // Our long-term objective is for whatever move we perform
            // to move towards the bottom right of the board. Our short-term
            // objective is for every move to move towards the current diagonal.

            var depth: u32 = 0;
            while (depth <= max_depth) : (depth += 1) {
                // -max_depth, -max_depth + 2 ... max_depth - 2, max_depth
                var diagonal: u32 = max_depth - depth;
                while (diagonal <= max_depth + depth) : (diagonal += 2) {
                    const should_go_left =
                        // "Most left" diagonal (bottom bound); diagonal is below the
                        // current position, so we can only go up from here.
                        diagonal != max_depth + depth and
                        // "Most right" diagonal (top bound); diagonal is to the
                        // right of current position, so we can only go left from here.
                        (diagonal == max_depth - depth or
                        // If we arrive here, we must pick one of two cases (left or right); comparators:
                        // == : right diagonal is closer to top left, so pick it (move left)
                        // <  : left diagonal has lower X (move up)
                        // >  : right diagonal has lower X (move left)
                        diagonal_lines_best_xs[diagonal - 1] >= diagonal_lines_best_xs[diagonal + 1]);

                    if (should_go_left) {
                        // Going left means that we're decreasing X by one with our move originating
                        // from the right and heading to our current diagonal. Thus, we need to copy
                        // the diagonal to the right of us' X, which is diagonal + 1, and then sub 1 from it.
                        // Saturating sub used to prevents underflow; should not affect behavior.
                        diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal + 1] -| 1;
                    } else {
                        // Going up means that we're keeping the X the same with our move originating
                        // from below and heading to our current diagonal. Thus, we need to copy the
                        // diagonal below us' X, which is diagonal - 1.
                        diagonal_lines_best_xs[diagonal] = diagonal_lines_best_xs[diagonal - 1];
                    }

                    // Extract x and y from formula `diagonal = x - y - (a - b)`
                    var x = diagonal_lines_best_xs[diagonal];
                    var y = @as(i64, @intCast(x + max_depth + b.len)) - @as(i64, @intCast(diagonal + a.len));

                    // Follow any snakes
                    // Note the x - 1 and y - 1; if we don't subtract, we're looking
                    // at the wrong snake
                    while (x > 0 and y > 0 and context.eql(a[x - 1], b[@intCast(y - 1)])) {
                        x -= 1;
                        y -= 1;
                        diagonal_lines_best_xs[diagonal] = x;
                    }

                    // Are we at the top left corner?
                    if (x <= 0 and y <= 0) return depth;
                }
            }

            unreachable;
        }
    };
}

/// Use this for values that can be compared
/// with a simple ==.
pub fn PrimitiveSesFinder(comptime T: type) type {
    const Context = struct {
        inline fn eql(context: @This(), a: T, b: T) bool {
            _ = context;
            return a == b;
        }
    };

    return SesFinder(T, Context);
}

pub fn main() !void {
    var seed: u64 = undefined;
    try std.os.getrandom(std.mem.asBytes(&seed));

    var rng = std.rand.DefaultPrng.init(seed);

    var scratch_buf: [4097]u32 = undefined;

    const letters = "abcdefghijklmnopqrstuvwxyz";

    var str_a: [1024]u8 = undefined;
    for (&str_a) |*c| {
        c.* = letters[rng.random().uintLessThan(usize, letters.len)];
    }

    var str_b: [1000]u8 = str_a[0..1000].*;

    for (&str_b) |*c| {
        if (rng.random().boolean()) {
            c.* = letters[rng.random().uintLessThan(usize, letters.len)];
        }
    }

    const Sum = struct { a: u8, b: u8 };

    const sum_a = [_]Sum{ .{ .a = 1, .b = 2 }, .{ .a = 2, .b = 1 }, .{ .a = 3, .b = 4 } };
    const sum_b = [_]Sum{ .{ .a = 1, .b = 2 }, .{ .a = 1, .b = 1 }, .{ .a = 0, .b = 2 } };

    const SumContext = struct {
        fn eql(context: @This(), a: Sum, b: Sum) bool {
            _ = context;
            return a.a + a.b == b.a + b.b;
        }
    };

    var timer = try std.time.Timer.start();

    const a = PrimitiveSesFinder(u8).calculateLength(&str_a, &str_b, &scratch_buf);
    const a_time = timer.lap();
    const a_reverse = PrimitiveSesFinder(u8).calculateReverseLength(&str_a, &str_b, &scratch_buf);
    const a_reverse_time = timer.lap();

    const b = SesFinder(Sum, SumContext).calculateLength(&sum_a, &sum_b, &scratch_buf);
    const b_time = timer.lap();

    std.debug.print("a\n", .{});
    std.debug.print("{d} == {d}\n", .{ a, a_reverse });
    std.debug.print("{d}ns == {d}ms; {d}ns == {d}ms\n", .{
        a_time,
        @as(f32, @floatFromInt(a_time)) / @as(f32, @floatFromInt(std.time.ns_per_ms)),
        a_reverse_time,
        @as(f32, @floatFromInt(a_reverse_time)) / @as(f32, @floatFromInt(std.time.ns_per_ms)),
    });

    std.debug.print("\nb\n", .{});
    std.debug.print("{d}\n", .{b});
    std.debug.print("{d}ns == {d}ms\n", .{ b_time, @as(f32, @floatFromInt(b_time)) / @as(f32, @floatFromInt(std.time.ns_per_ms)) });
}
