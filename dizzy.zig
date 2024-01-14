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
///   - fn eql(context: Context, a_index: u32, b_index: u32) bool
pub fn Differ(comptime Context: type) type {
    return struct {
        const DiagonalHelper = struct {
            max_depth: i32,
            diagonal_lines_best_xs: []u32,

            pub fn init(a_len: u32, b_len: u32, diagonal_lines_best_xs: []u32) DiagonalHelper {
                const max_depth: i32 = @intCast(a_len + b_len);
                // Number of diagonals, both positive or negative (some are off the board)
                const required_scratch_len: i32 = 2 * max_depth + 1;
                // -max_depth ... max_depth (0 is included, which is why the + 1 is present)
                std.debug.assert(diagonal_lines_best_xs.len >= required_scratch_len);

                return .{
                    .max_depth = max_depth,
                    .diagonal_lines_best_xs = diagonal_lines_best_xs,
                };
            }

            pub fn getBestX(helper: *DiagonalHelper, diagonal: i32) u32 {
                return helper.diagonal_lines_best_xs[@intCast(helper.max_depth + diagonal)];
            }

            pub fn setBestX(helper: *DiagonalHelper, diagonal: i32, value: u32) void {
                helper.diagonal_lines_best_xs[@intCast(helper.max_depth + diagonal)] = value;
            }
        };

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(2 * (a.len + b.len) + 1)` long
        pub fn calculateSesLength(
            a_len: u32,
            b_len: u32,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) u32 {
            if (a_len == 0 and b_len == 0) return 0;
            if (a_len == 0) return b_len;
            if (b_len == 0) return a_len;

            var diagonals = DiagonalHelper.init(a_len, b_len, diagonal_lines_best_xs);

            // Called to create valid case for first move which is off the board.
            diagonals.setBestX(1, 0);

            // Our long-term objective is for whatever move we perform
            // to move towards the bottom right of the board. Our short-term
            // objective is for every move to move towards the current diagonal.

            var depth: i32 = 0;
            while (depth <= diagonals.max_depth) : (depth += 1) {
                // -max_depth, -max_depth + 2 ... max_depth - 2, max_depth
                var diagonal = -depth;
                while (diagonal <= depth) : (diagonal += 2) {
                    const should_go_right =
                        // "Most left" diagonal (bottom bound); diagonal is below the
                        // current position, so we can only go down from here.
                        diagonal != -depth and
                        // "Most right" diagonal (top bound); diagonal is to the
                        // right of current position, so we can only go right from here.
                        (diagonal == depth or
                        // If we arrive here, we must pick one of two cases (left or right); comparators:
                        // == : left diagonal is closer to bottom right, so pick it (move right)
                        // <  : right diagonal has higher X (move down)
                        // >  : left diagonal has higher X (move right)
                        diagonals.getBestX(diagonal - 1) >= diagonals.getBestX(diagonal + 1));

                    if (should_go_right) {
                        // Going right means that we're increasing X by one with our move originating
                        // from the left and heading to our current diagonal. Thus, we need to copy
                        // the diagonal to the left of us' X, which is diagonal - 1, and then add 1 to it.
                        diagonals.setBestX(diagonal, diagonals.getBestX(diagonal - 1) + 1);
                    } else {
                        // Going down means that we're keeping the X the same with our move originating
                        // from above and heading to our current diagonal. Thus, we need to copy the
                        // diagonal above us' X, which is diagonal + 1.
                        diagonals.setBestX(diagonal, diagonals.getBestX(diagonal + 1));
                    }

                    // Extract x and y from formula `diagonal = x - y`
                    var x: i32 = @intCast(diagonals.getBestX(diagonal));
                    var y: i32 = x - diagonal;

                    // Follow any snakes
                    while (x < a_len and y < b_len and context.eql(@intCast(x), @intCast(y))) {
                        x += 1;
                        y += 1;
                        diagonals.setBestX(diagonal, @intCast(x));
                    }

                    // Are we at the bottom right corner?
                    if (x >= a_len and y >= b_len) return @intCast(depth);
                }
            }

            unreachable;
        }

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(2 * (a.len + b.len) + 1)` long
        /// Result should be identical to `calculateLengthContext`
        pub fn calculateReverseSesLength(
            a_len: u32,
            b_len: u32,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) u32 {
            if (a_len == 0 and b_len == 0) return 0;
            if (a_len == 0) return b_len;
            if (b_len == 0) return a_len;

            var diagonals = DiagonalHelper.init(a_len, b_len, diagonal_lines_best_xs);

            // Called to create valid case for first move which is off the board.
            diagonals.setBestX(-1, @intCast(a_len));

            var depth: i32 = 0;
            while (depth <= diagonals.max_depth) : (depth += 1) {
                // -max_depth, -max_depth + 2 ... max_depth - 2, max_depth
                var diagonal = -depth;
                while (diagonal <= depth) : (diagonal += 2) {
                    const should_go_left =
                        // "Most left" diagonal (bottom bound); diagonal is below the
                        // current position, so we can only go up from here.
                        diagonal != depth and
                        // "Most right" diagonal (top bound); diagonal is to the
                        // right of current position, so we can only go left from here.
                        (diagonal == -depth or
                        // If we arrive here, we must pick one of two cases (left or right); comparators:
                        // == : right diagonal is closer to top left, so pick it (move left)
                        // <  : left diagonal has lower X (move up)
                        // >  : right diagonal has lower X (move left)
                        diagonals.getBestX(diagonal - 1) >= diagonals.getBestX(diagonal + 1));

                    if (should_go_left) {
                        // Going left means that we're decreasing X by one with our move originating
                        // from the right and heading to our current diagonal. Thus, we need to copy
                        // the diagonal to the right of us' X, which is diagonal + 1, and then sub 1 from it.
                        // Saturating sub used to prevents underflow; should not affect behavior.
                        diagonals.setBestX(diagonal, diagonals.getBestX(diagonal + 1) -| 1);
                    } else {
                        // Going up means that we're keeping the X the same with our move originating
                        // from below and heading to our current diagonal. Thus, we need to copy the
                        // diagonal below us' X, which is diagonal - 1.
                        diagonals.setBestX(diagonal, diagonals.getBestX(diagonal - 1));
                    }

                    // Extract x and y from formula `diagonal = x - y + (a - b)`
                    var x: i32 = @intCast(diagonals.getBestX(diagonal));
                    var y: i32 = -diagonal + x - @as(i32, @intCast(a_len)) + @as(i32, @intCast(b_len));

                    // Follow any snakes
                    // Note the x - 1 and y - 1; if we don't subtract, we're looking
                    // at the wrong snake
                    while (x > 0 and y > 0 and context.eql(@intCast(x - 1), @intCast(y - 1))) {
                        x -= 1;
                        y -= 1;
                        diagonals.setBestX(diagonal, @intCast(x));
                    }

                    // Are we at the top left corner?
                    if (x <= 0 and y <= 0) return @intCast(depth);
                }
            }

            unreachable;
        }

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(4 * (a.len + b.len) + 2)` long
        pub fn diff(
            allocator: std.mem.Allocator,
            edits: *std.ArrayListUnmanaged(Edit),
            a_len: u32,
            b_len: u32,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) error{OutOfMemory}!void {
            try diffInternal(allocator, edits, a_len, b_len, diagonal_lines_best_xs, context, 0, 0);
        }

        pub fn diffInternal(
            allocator: std.mem.Allocator,
            edits: *std.ArrayListUnmanaged(Edit),
            a_len: u32,
            b_len: u32,
            diagonal_lines_best_xs: []u32,
            context: Context,
            a_offset_from_start: u32,
            b_offset_from_start: u32,
        ) error{OutOfMemory}!void {
            // Skip equals at front
            var start_offset: u32 = 0;
            while (start_offset < a_len and
                start_offset < b_len and
                context.eql(a_offset_from_start + start_offset, b_offset_from_start + start_offset))
            {
                start_offset += 1;
            }

            if (start_offset != 0) {
                try edits.append(allocator, .{
                    .kind = .equal,
                    .range = .{
                        .start = a_offset_from_start,
                        .end = a_offset_from_start + start_offset,
                    },
                });
            }

            // Skip equals in back
            var end_offset: u32 = 0;
            while (a_len - start_offset > end_offset and
                b_len - start_offset > end_offset and
                context.eql(a_offset_from_start + a_len - end_offset - 1, b_offset_from_start + b_len - end_offset - 1))
            {
                end_offset += 1;
            }

            const a_sliced_len = a_len - start_offset - end_offset;
            const b_sliced_len = b_len - start_offset - end_offset;

            if (a_sliced_len > 0 and b_sliced_len > 0) {
                const bounds = try computeMiddleSnake(
                    a_offset_from_start + start_offset,
                    b_offset_from_start + start_offset,
                    a_sliced_len,
                    b_sliced_len,
                    diagonal_lines_best_xs,
                    context,
                );

                // Both have chars; more middle snakes to be found!
                try diffInternal(
                    allocator,
                    edits,
                    bounds.x1,
                    bounds.y1,
                    diagonal_lines_best_xs,
                    context,
                    a_offset_from_start + start_offset,
                    b_offset_from_start + start_offset,
                );

                if (bounds.x1 != bounds.x2) {
                    try edits.append(allocator, .{
                        .kind = .equal,
                        .range = .{
                            .start = a_offset_from_start + start_offset + bounds.x1,
                            .end = a_offset_from_start + start_offset + bounds.x2,
                        },
                    });
                }

                try diffInternal(
                    allocator,
                    edits,
                    a_sliced_len - bounds.x2,
                    b_sliced_len - bounds.y2,
                    diagonal_lines_best_xs,
                    context,
                    a_offset_from_start + start_offset + bounds.x2,
                    b_offset_from_start + start_offset + bounds.y2,
                );
            } else if (a_sliced_len > 0) {
                // a has chars, b has none; we must delete from a to match b
                try edits.append(allocator, .{
                    .kind = .delete,
                    .range = .{
                        .start = a_offset_from_start + start_offset,
                        .end = @intCast(a_offset_from_start + a_len - end_offset),
                    },
                });
            } else if (b_sliced_len > 0) {
                // b has chars, a has none; we must insert from b
                try edits.append(allocator, .{
                    .kind = .insert,
                    .range = .{
                        .start = b_offset_from_start + start_offset,
                        .end = @intCast(b_offset_from_start + b_len - end_offset),
                    },
                });
            }

            if (end_offset != 0) {
                try edits.append(allocator, .{
                    .kind = .equal,
                    .range = .{
                        .start = a_offset_from_start + @as(u32, @intCast(a_len)) - end_offset,
                        .end = a_offset_from_start + @as(u32, @intCast(a_len)),
                    },
                });
            }
        }

        pub const MiddleSnakeBounds = struct {
            x1: u32,
            y1: u32,
            x2: u32,
            y2: u32,
            ses_len: u32,
        };

        fn computeMiddleSnake(
            a_offset_from_start: u32,
            b_offset_from_start: u32,
            a_len: u32,
            b_len: u32,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) error{OutOfMemory}!MiddleSnakeBounds {
            const max_depth: i32 = @intCast(a_len + b_len);
            // Number of diagonals for forwards and reverse, both positive or negative (some are off the board)
            const required_scratch_len: u32 = 2 * @as(u32, @intCast(max_depth)) + 1;
            // -max_depth ... max_depth (0 is included, which is why the + 1 is present) x2
            std.debug.assert(diagonal_lines_best_xs.len >= required_scratch_len * 2);

            const forward_diagonal_best_xs = diagonal_lines_best_xs[0..required_scratch_len];
            var forward_diagonals = DiagonalHelper.init(a_len, b_len, forward_diagonal_best_xs);
            forward_diagonals.setBestX(1, 0);

            const backward_diagonal_best_xs = diagonal_lines_best_xs[required_scratch_len .. required_scratch_len * 2];
            var backward_diagonals = DiagonalHelper.init(a_len, b_len, backward_diagonal_best_xs);
            backward_diagonals.setBestX(-1, @intCast(a_len));

            const delta = @as(i32, @intCast(a_len)) - @as(i32, @intCast(b_len));
            const is_delta_even = @rem(delta, 2) == 0;

            var depth: i32 = 0;
            while (depth <= (a_len + b_len + 1) / 2) : (depth += 1) {
                var diagonal = -depth;
                while (diagonal <= depth) : (diagonal += 2) {
                    const should_go_right =
                        diagonal != -depth and
                        (diagonal == depth or
                        forward_diagonals.getBestX(diagonal - 1) >= forward_diagonals.getBestX(diagonal + 1));

                    if (should_go_right) {
                        forward_diagonals.setBestX(diagonal, forward_diagonals.getBestX(diagonal - 1) + 1);
                    } else {
                        forward_diagonals.setBestX(diagonal, forward_diagonals.getBestX(diagonal + 1));
                    }

                    var x: i32 = @intCast(forward_diagonals.getBestX(diagonal));
                    var y = x - diagonal;
                    const x1 = x;
                    const y1 = y;

                    while (x < a_len and y < b_len and context.eql(a_offset_from_start + @as(u32, @intCast(x)), b_offset_from_start + @as(u32, @intCast(y)))) {
                        x += 1;
                        y += 1;
                        forward_diagonals.setBestX(diagonal, @intCast(x));
                    }

                    const backward_diagonal = @as(i32, @intCast(diagonal)) - delta;

                    if (!is_delta_even and
                        // Bounds check
                        backward_diagonal >= -depth and backward_diagonal <= depth and
                        // Does it overlap?
                        forward_diagonals.getBestX(diagonal) >= backward_diagonals.getBestX(@intCast(backward_diagonal)))
                    {
                        return .{
                            .x1 = @intCast(x1),
                            .y1 = @intCast(y1),
                            .x2 = @intCast(x),
                            .y2 = @intCast(y),
                            .ses_len = @intCast(2 * depth - 1),
                        };
                    }
                }

                diagonal = -depth;
                while (diagonal <= depth) : (diagonal += 2) {
                    const should_go_left =
                        diagonal != depth and
                        (diagonal == -depth or
                        backward_diagonals.getBestX(diagonal - 1) >= backward_diagonals.getBestX(diagonal + 1));

                    if (should_go_left) {
                        backward_diagonals.setBestX(diagonal, backward_diagonals.getBestX(diagonal + 1) -| 1);
                    } else {
                        backward_diagonals.setBestX(diagonal, backward_diagonals.getBestX(diagonal - 1));
                    }

                    var x: i32 = @intCast(backward_diagonals.getBestX(diagonal));
                    var y = x - diagonal - delta;
                    const x2 = x;
                    const y2 = y;

                    while (x > 0 and y > 0 and context.eql(a_offset_from_start + @as(u32, @intCast(x)) - 1, b_offset_from_start + @as(u32, @intCast(y)) - 1)) {
                        x -= 1;
                        y -= 1;
                        backward_diagonals.setBestX(diagonal, @intCast(x));
                    }

                    const forward_diagonal = @as(i32, @intCast(diagonal)) + delta;

                    if (is_delta_even and
                        // Bounds check
                        forward_diagonal >= -depth and forward_diagonal <= depth and
                        // Does it overlap?
                        forward_diagonals.getBestX(@intCast(forward_diagonal)) >= backward_diagonals.getBestX(diagonal))
                    {
                        return .{
                            .x1 = @intCast(x),
                            .y1 = @intCast(y),
                            .x2 = @intCast(x2),
                            .y2 = @intCast(y2),
                            .ses_len = @intCast(2 * depth),
                        };
                    }
                }
            }

            unreachable;
        }
    };
}

/// Context must contain:
///   - fn eql(context: Context, a: T, b: T) bool
pub fn SliceDiffer(comptime T: type, comptime Context: type) type {
    const SliceContext = struct {
        a: []const T,
        b: []const T,
        context: Context,

        inline fn eql(context: @This(), a_index: u32, b_index: u32) bool {
            return context.context.eql(context.a[a_index], context.b[b_index]);
        }
    };

    return struct {
        const DifferImpl = Differ(SliceContext);

        /// Number of characters inserted and deleted
        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(2 * (a.len + b.len) + 1)` long
        pub fn calculateSesLength(a: []const T, b: []const T, diagonal_lines_best_xs: []u32) u32 {
            return calculateSesLengthContext(a, b, diagonal_lines_best_xs, .{});
        }

        /// Number of characters inserted and deleted
        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(2 * (a.len + b.len) + 1)` long
        pub fn calculateSesLengthContext(
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) u32 {
            return DifferImpl.calculateSesLength(
                @intCast(a.len),
                @intCast(b.len),
                diagonal_lines_best_xs,
                .{
                    .a = a,
                    .b = b,
                    .context = context,
                },
            );
        }

        /// Number of characters inserted and deleted
        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(2 * (a.len + b.len) + 1)` long
        pub fn calculateReverseSesLength(a: []const T, b: []const T, diagonal_lines_best_xs: []u32) u32 {
            return calculateReverseSesLengthContext(a, b, diagonal_lines_best_xs, .{});
        }

        /// Number of characters inserted and deleted
        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(2 * (a.len + b.len) + 1)` long
        pub fn calculateReverseSesLengthContext(
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) u32 {
            return DifferImpl.calculateReverseSesLength(
                @intCast(a.len),
                @intCast(b.len),
                diagonal_lines_best_xs,
                .{
                    .a = a,
                    .b = b,
                    .context = context,
                },
            );
        }

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(4 * (a.len + b.len) + 2)` long
        pub fn diff(
            allocator: std.mem.Allocator,
            edits: *std.ArrayListUnmanaged(Edit),
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
        ) error{OutOfMemory}!void {
            return diffContext(allocator, edits, a, b, diagonal_lines_best_xs, .{});
        }

        /// Caller asserts that the scratch buffer `diagonal_lines_best_xs` must be at least `(4 * (a.len + b.len) + 2)` long
        pub fn diffContext(
            allocator: std.mem.Allocator,
            edits: *std.ArrayListUnmanaged(Edit),
            a: []const T,
            b: []const T,
            diagonal_lines_best_xs: []u32,
            context: Context,
        ) error{OutOfMemory}!void {
            return DifferImpl.diff(
                allocator,
                edits,
                @intCast(a.len),
                @intCast(b.len),
                diagonal_lines_best_xs,
                .{
                    .a = a,
                    .b = b,
                    .context = context,
                },
            );
        }
    };
}

pub fn PrimitiveSliceDiffer(comptime T: type) type {
    return SliceDiffer(T, struct {
        inline fn eql(context: @This(), a: T, b: T) bool {
            _ = context;
            return a == b;
        }
    });
}

test {
    const allocator = std.testing.allocator;

    var rng = std.rand.DefaultPrng.init(0);
    var random = rng.random();

    var a = std.ArrayListUnmanaged(u8){};
    var b = std.ArrayListUnmanaged(u8){};
    var scratch = std.ArrayListUnmanaged(u32){};
    var edits = std.ArrayListUnmanaged(Edit){};
    var diffed_b = std.ArrayListUnmanaged(u8){};

    defer {
        a.deinit(allocator);
        b.deinit(allocator);
        scratch.deinit(allocator);
        edits.deinit(allocator);
        diffed_b.deinit(allocator);
    }

    for (0..100) |a_len| {
        for (0..100) |b_len| {
            try a.ensureTotalCapacity(allocator, a_len);
            a.items.len = a_len;
            try b.ensureTotalCapacity(allocator, b_len);
            b.items.len = b_len;

            const scratch_len = 4 * (a_len + b_len) + 2;
            try scratch.ensureTotalCapacity(allocator, scratch_len);
            scratch.items.len = scratch_len;

            random.bytes(a.items);
            random.bytes(b.items);

            const ses_len = PrimitiveSliceDiffer(u8).calculateSesLength(a.items, b.items, scratch.items);
            const reverse_ses_len = PrimitiveSliceDiffer(u8).calculateReverseSesLength(a.items, b.items, scratch.items);
            try std.testing.expectEqual(ses_len, reverse_ses_len);

            edits.items.len = 0;
            try PrimitiveSliceDiffer(u8).diff(allocator, &edits, a.items, b.items, scratch.items);

            var actual_ses_len: u32 = 0;

            diffed_b.items.len = 0;

            for (edits.items) |edit| {
                switch (edit.kind) {
                    .equal => {
                        try diffed_b.appendSlice(allocator, a.items[edit.range.start..edit.range.end]);
                    },
                    .insert => {
                        actual_ses_len += edit.range.end - edit.range.start;
                        try diffed_b.appendSlice(allocator, b.items[edit.range.start..edit.range.end]);
                    },
                    .delete => {
                        actual_ses_len += edit.range.end - edit.range.start;
                    },
                }
            }

            try std.testing.expectEqual(ses_len, actual_ses_len);
            try std.testing.expectEqualSlices(u8, b.items, diffed_b.items);
        }
    }
}
