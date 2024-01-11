const std = @import("std");

pub const Edit = struct {
    pub const Kind = enum {
        /// range in a
        equal,
        /// range in b
        insert,
        /// range in a
        delete,
    };
    pub const Range = struct { start: u32, end: u32 };

    kind: Kind,
    range: Range,
};

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
            return calculateLengthContext(a, b, diagonal_lines_best_xs, .{});
        }

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least (2 * (a.len + b.len) + 1) long
        pub fn calculateLengthContext(
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
            context: Context,
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
            return calculateReverseLengthContext(a, b, diagonal_lines_best_xs, .{});
        }

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least (2 * (a.len + b.len) + 1) long
        /// Result should be identical to `calculateLengthContext`
        pub fn calculateReverseLengthContext(
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) u32 {
            if (a.len == 0 and b.len == 0) return 0;

            const max_depth: u32 = @intCast(a.len + b.len);
            // Number of diagonals, both positive or negative (some are off the board)
            const required_scratch_len: u32 = 2 * max_depth + 1;
            // -max_depth ... max_depth (0 is included, which is why the + 1 is present)
            std.debug.assert(diagonal_lines_best_xs.len >= required_scratch_len);

            // Called to create valid case for first move which is off the board.
            diagonal_lines_best_xs[max_depth - 1] = @intCast(a.len);

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

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least (4 * (a.len + b.len) + 2) long
        pub fn compute(
            allocator: std.mem.Allocator,
            edits: *std.ArrayListUnmanaged(Edit),
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
        ) error{OutOfMemory}!void {
            if (std.meta.fields(Context).len != 0) @compileError("You must call `computeContext`.");
            return computeContext(allocator, edits, a, b, diagonal_lines_best_xs, .{});
        }

        pub fn computeContext(
            allocator: std.mem.Allocator,
            edits: *std.ArrayListUnmanaged(Edit),
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) error{OutOfMemory}!void {
            try computeContextInternal(allocator, edits, a, b, diagonal_lines_best_xs, context, 0, 0);
        }

        pub fn computeContextInternal(
            allocator: std.mem.Allocator,
            edits: *std.ArrayListUnmanaged(Edit),
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
            context: Context,
            a_offset_from_start: u32,
            b_offset_from_start: u32,
        ) error{OutOfMemory}!void {
            // Skip equals at front
            var start: u32 = 0;
            while (start < a.len and
                start < b.len and
                context.eql(a[start], b[start]))
            {
                start += 1;
            }

            if (start != 0) {
                try edits.append(allocator, .{
                    .kind = .equal,
                    .range = .{
                        .start = a_offset_from_start,
                        .end = a_offset_from_start + start,
                    },
                });
            }

            var a_sliced = a[start..];
            var b_sliced = b[start..];

            // Skip equals in back
            var end_offset: u32 = 0;
            while (a_sliced.len > end_offset and
                b_sliced.len > end_offset and
                context.eql(a_sliced[a_sliced.len - end_offset - 1], b_sliced[b_sliced.len - end_offset - 1]))
            {
                end_offset += 1;
            }

            a_sliced = a_sliced[0 .. a_sliced.len - end_offset];
            b_sliced = b_sliced[0 .. b_sliced.len - end_offset];

            if (a_sliced.len > 0 and b_sliced.len > 0) {
                const bounds = try computeMiddleSnake(
                    a_sliced,
                    b_sliced,
                    diagonal_lines_best_xs,
                    context,
                );

                // Both have chars; more middle snakes to be found!
                try computeContextInternal(
                    allocator,
                    edits,
                    a_sliced[0..bounds.x1],
                    b_sliced[0..bounds.y1],
                    diagonal_lines_best_xs,
                    context,
                    a_offset_from_start + start,
                    b_offset_from_start + start,
                );

                if (bounds.x1 != bounds.x2) {
                    try edits.append(allocator, .{
                        .kind = .equal,
                        .range = .{
                            .start = a_offset_from_start + start + bounds.x1,
                            .end = a_offset_from_start + start + bounds.x2,
                        },
                    });
                }

                try computeContextInternal(
                    allocator,
                    edits,
                    a_sliced[bounds.x2..],
                    b_sliced[bounds.y2..],
                    diagonal_lines_best_xs,
                    context,
                    a_offset_from_start + start + bounds.x2,
                    b_offset_from_start + start + bounds.y2,
                );
            } else if (a_sliced.len > 0) {
                // a has chars, b has none; we must delete from a to match b
                try edits.append(allocator, .{
                    .kind = .delete,
                    .range = .{
                        .start = a_offset_from_start,
                        .end = @intCast(a_offset_from_start + a_sliced.len),
                    },
                });
            } else if (b_sliced.len > 0) {
                // b has chars, a has none; we must insert from b
                try edits.append(allocator, .{
                    .kind = .insert,
                    .range = .{
                        .start = b_offset_from_start,
                        .end = @intCast(b_offset_from_start + b_sliced.len),
                    },
                });
            }

            if (end_offset != 0) {
                try edits.append(allocator, .{
                    .kind = .equal,
                    .range = .{
                        .start = a_offset_from_start + @as(u32, @intCast(a.len)) - end_offset,
                        .end = a_offset_from_start + @as(u32, @intCast(a.len)),
                    },
                });
            }
        }

        pub const MiddleSnakeBounds = struct { x1: u32, y1: u32, x2: u32, y2: u32 };

        fn computeMiddleSnake(
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) error{OutOfMemory}!MiddleSnakeBounds {
            const max_depth: u32 = @intCast(a.len + b.len);
            // Number of diagonals for forwards and reverse, both positive or negative (some are off the board)
            const required_scratch_len: u32 = 2 * max_depth + 1;
            // -max_depth ... max_depth (0 is included, which is why the + 1 is present) x2
            std.debug.assert(diagonal_lines_best_xs.len >= required_scratch_len * 2);

            const forward_diagonal_best_xs = diagonal_lines_best_xs[required_scratch_len..];
            forward_diagonal_best_xs[max_depth + 1] = 0;

            const backward_diagonal_best_xs = diagonal_lines_best_xs[0..required_scratch_len];
            backward_diagonal_best_xs[max_depth - 1] = @intCast(a.len);

            const delta = @as(i64, @intCast(a.len)) - @as(i64, @intCast(b.len));
            const is_delta_even = @rem(delta, 2) == 0;

            var depth: u32 = 0;
            while (depth <= (a.len + b.len + 1) / 2) : (depth += 1) {
                var diagonal: u32 = max_depth - depth;
                while (diagonal <= max_depth + depth) : (diagonal += 2) {
                    const should_go_right =
                        diagonal != max_depth - depth and
                        (diagonal == max_depth + depth or
                        forward_diagonal_best_xs[diagonal - 1] >= forward_diagonal_best_xs[diagonal + 1]);

                    if (should_go_right) {
                        forward_diagonal_best_xs[diagonal] = forward_diagonal_best_xs[diagonal - 1] + 1;
                    } else {
                        forward_diagonal_best_xs[diagonal] = forward_diagonal_best_xs[diagonal + 1];
                    }

                    var x = forward_diagonal_best_xs[diagonal];
                    var y = x + max_depth - diagonal;

                    while (x < a.len and y < b.len and context.eql(a[x], b[y])) {
                        x += 1;
                        y += 1;
                        forward_diagonal_best_xs[diagonal] = x;
                    }

                    const backward_diagonal = @as(i64, @intCast(diagonal)) - delta;

                    if (!is_delta_even and
                        // Bounds check
                        backward_diagonal >= max_depth - depth and backward_diagonal <= max_depth + depth and
                        // Does it overlap?
                        forward_diagonal_best_xs[diagonal] >= backward_diagonal_best_xs[@intCast(backward_diagonal)])
                    {
                        return .{
                            .x1 = backward_diagonal_best_xs[@intCast(backward_diagonal)],
                            .y1 = @intCast(@as(i64, @intCast(backward_diagonal_best_xs[@intCast(backward_diagonal)] + max_depth)) - backward_diagonal - delta),
                            .x2 = x,
                            .y2 = y,
                        };
                    }
                }

                diagonal = max_depth - depth;
                while (diagonal <= max_depth + depth) : (diagonal += 2) {
                    const should_go_left =
                        diagonal != max_depth + depth and
                        (diagonal == max_depth - depth or
                        backward_diagonal_best_xs[diagonal - 1] >= backward_diagonal_best_xs[diagonal + 1]);

                    if (should_go_left) {
                        backward_diagonal_best_xs[diagonal] = backward_diagonal_best_xs[diagonal + 1] -| 1;
                    } else {
                        backward_diagonal_best_xs[diagonal] = backward_diagonal_best_xs[diagonal - 1];
                    }

                    var x = backward_diagonal_best_xs[diagonal];
                    var y = @as(i64, @intCast(x + max_depth)) - @as(i64, @intCast(diagonal)) - delta;

                    while (x > 0 and y > 0 and context.eql(a[x - 1], b[@intCast(y - 1)])) {
                        x -= 1;
                        y -= 1;
                        backward_diagonal_best_xs[diagonal] = x;
                    }

                    const forward_diagonal: u32 = @intCast(@as(i64, @intCast(diagonal)) + delta);

                    if (is_delta_even and
                        // Bounds check
                        forward_diagonal >= max_depth - depth and forward_diagonal <= max_depth + depth and
                        // Does it overlap?
                        forward_diagonal_best_xs[forward_diagonal] >= backward_diagonal_best_xs[diagonal])
                    {
                        return .{
                            .x1 = x,
                            .y1 = @intCast(y),
                            .x2 = forward_diagonal_best_xs[forward_diagonal],
                            .y2 = forward_diagonal_best_xs[forward_diagonal] + max_depth - forward_diagonal,
                        };
                    }
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

    var scratch_buf: [16_000]u32 = undefined;

    const letters = "abcdefghijklmnopqrstuvwxyz";

    var str_a: [16]u8 = undefined;
    for (&str_a) |*c| {
        c.* = letters[rng.random().uintLessThan(usize, letters.len)];
    }

    var str_b: [16]u8 = str_a[0..16].*;

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

    var edits = std.ArrayListUnmanaged(Edit){};
    try PrimitiveSesFinder(u8).compute(std.heap.page_allocator, &edits, &str_a, &str_b, &scratch_buf);
    const a_compute_time = timer.lap();

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

    std.debug.print("{any}\n", .{edits.items});
    std.debug.print("{d}ns == {d}ms\n", .{
        a_compute_time,
        @as(f32, @floatFromInt(a_compute_time)) / @as(f32, @floatFromInt(std.time.ns_per_ms)),
    });

    std.debug.print("A: {s}\n", .{str_a});
    std.debug.print("B: {s}\n", .{str_b});

    for (edits.items) |edit| {
        switch (edit.kind) {
            .equal => {
                std.debug.print("{s}", .{str_a[edit.range.start..edit.range.end]});
                // std.debug.print("={s}\n", .{str_a[edit.range.start..edit.range.end]});
            },
            .insert => {
                std.debug.print("{s}", .{str_b[edit.range.start..edit.range.end]});
                // std.debug.print("+{s}\n", .{str_b[edit.range.start..edit.range.end]});
            },
            .delete => {
                // std.debug.print("-{s}\n", .{str_a[edit.range.start..edit.range.end]});
            },
        }
    }

    std.debug.print("\nb\n", .{});
    std.debug.print("{d}\n", .{b});
    std.debug.print("{d}ns == {d}ms\n", .{ b_time, @as(f32, @floatFromInt(b_time)) / @as(f32, @floatFromInt(std.time.ns_per_ms)) });
}
