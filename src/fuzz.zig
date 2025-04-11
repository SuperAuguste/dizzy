const std = @import("std");
const dizzy = @import("dizzy.zig");
const assert = std.debug.assert;

var edits: std.ArrayListUnmanaged(dizzy.Edit) = .{};
var diagonal_lines_best_xs: std.ArrayListUnmanaged(u32) = .{};
var reconstructed_after: std.ArrayListUnmanaged(u8) = .{};

export fn LLVMFuzzerTestOneInput(data_ptr: [*]const u8, data_len: usize) c_int {
    errdefer |err| std.debug.panic("{any}", .{err});

    const data = data_ptr[0..data_len];

    if (data.len < @sizeOf(Header)) {
        return -1;
    }

    const header: Header = @bitCast(data[0..@sizeOf(Header)].*);
    assert(header.magic == Header.valid_magic);

    const before = data[@sizeOf(Header)..][0..header.before_len];
    const after = data[@sizeOf(Header) + header.before_len ..][0..header.after_len];

    const allocator = std.heap.smp_allocator;

    edits.clearRetainingCapacity();
    diagonal_lines_best_xs.clearRetainingCapacity();
    reconstructed_after.clearRetainingCapacity();

    try diagonal_lines_best_xs.resize(allocator, 4 * (before.len + after.len) + 2);
    try dizzy.PrimitiveSliceDiffer(u8).diff(
        allocator,
        &edits,
        before,
        after,
        diagonal_lines_best_xs.items,
    );

    for (edits.items) |edit| {
        switch (edit.kind) {
            .equal => {
                try reconstructed_after.appendSlice(
                    allocator,
                    before[edit.range.start..edit.range.end],
                );
            },
            .insert => {
                try reconstructed_after.appendSlice(
                    allocator,
                    after[edit.range.start..edit.range.end],
                );
            },
            .delete => {},
        }
    }

    assert(std.mem.eql(u8, after, reconstructed_after.items));

    return 0;
}

export fn LLVMFuzzerCustomMutator(data_ptr: [*]u8, data_len: usize, capacity: usize, seed: c_uint) usize {
    assert(data_len <= capacity);

    var data = std.ArrayListUnmanaged(u8){ .items = data_ptr[0..data_len], .capacity = capacity };

    if (data.items.len < @sizeOf(Header)) {
        // Initial case
        data.clearRetainingCapacity();
        data.appendSliceAssumeCapacity(std.mem.asBytes(&Header{
            .before_len = 0,
            .after_len = 0,
        }));
    } else blk: {
        const header: *Header = @ptrCast(data.items[0..@sizeOf(Header)]);
        assert(header.magic == Header.valid_magic);

        var prng = std.Random.DefaultPrng.init(seed);
        const random = prng.random();

        const is_space_limited = @sizeOf(Header) + header.before_len + header.after_len >= data.capacity;

        const before_or_after = random.enumValue(enum { before, after });

        switch (before_or_after) {
            .before => if (header.before_len == 0) {
                if (!is_space_limited) {
                    data.insertAssumeCapacity(@sizeOf(Header), random.int(u8));
                    header.before_len += 1;
                }
                break :blk;
            },
            .after => if (header.after_len == 0) {
                if (!is_space_limited) {
                    data.insertAssumeCapacity(@sizeOf(Header) + header.before_len, random.int(u8));
                    header.after_len += 1;
                }
                break :blk;
            },
        }

        const index = switch (before_or_after) {
            .before => @sizeOf(Header) + random.uintLessThan(u32, header.before_len),
            .after => @sizeOf(Header) + header.before_len + random.uintLessThan(u32, header.after_len),
        };

        switch (Mutation.pick(random, if (is_space_limited) .space_limited else .any)) {
            .insert_byte => {
                const byte = random.int(u8);
                data.insertAssumeCapacity(index, byte);

                switch (before_or_after) {
                    .before => header.before_len += 1,
                    .after => header.after_len += 1,
                }
            },
            .mutate_byte => {
                const byte = random.int(u8);
                data.items[index] = byte;
            },
            .remove_byte => {
                _ = data.orderedRemove(index);

                switch (before_or_after) {
                    .before => header.before_len -= 1,
                    .after => header.after_len -= 1,
                }
            },
        }
    }

    assert(data.items.ptr == data_ptr);
    assert(data.items.len <= capacity);
    assert(data.capacity == capacity);
    return data.items.len;
}

export fn LLVMFuzzerCustomCrossOver(
    a_ptr: [*]const u8,
    a_len: usize,
    b_ptr: [*]const u8,
    b_len: usize,
    out_ptr: [*]u8,
    out_len: usize,
    seed: c_uint,
) usize {
    const a = a_ptr[0..a_len];
    const b = b_ptr[0..b_len];

    if (a.len < @sizeOf(Header) or b.len < @sizeOf(Header)) {
        return 0;
    }

    var prng = std.Random.DefaultPrng.init(seed);
    const random = prng.random();

    const a_header: *const Header = @ptrCast(a[0..@sizeOf(Header)]);
    const b_header: *const Header = @ptrCast(b[0..@sizeOf(Header)]);

    var out = std.ArrayListUnmanaged(u8).initBuffer(out_ptr[0..out_len]);

    const crossover_kind = random.enumValue(enum { a_before_b_after, b_before_a_after });

    switch (crossover_kind) {
        .a_before_b_after => {
            out.appendSliceAssumeCapacity(std.mem.asBytes(&Header{
                .before_len = a_header.before_len,
                .after_len = b_header.after_len,
            }));
            out.appendSliceAssumeCapacity(a_header.beforeConstSlice());
            out.appendSliceAssumeCapacity(b_header.afterConstSlice());
        },
        .b_before_a_after => {
            out.appendSliceAssumeCapacity(std.mem.asBytes(&Header{
                .before_len = b_header.before_len,
                .after_len = a_header.after_len,
            }));
            out.appendSliceAssumeCapacity(b_header.beforeConstSlice());
            out.appendSliceAssumeCapacity(a_header.afterConstSlice());
        },
    }

    assert(out.items.ptr == out_ptr);
    assert(out.items.len <= out_len);
    assert(out.capacity == out_len);
    return out.items.len;
}

/// pinned
const Header = extern struct {
    const valid_magic = 0x49d12b7b;

    magic: u32 align(1) = valid_magic,
    before_len: u32 align(1),
    after_len: u32 align(1),

    fn beforeConstSlice(header: *const Header) []const u8 {
        const buffer: [*]const u8 = @ptrCast(header);
        return buffer[@sizeOf(Header)..][0..header.before_len];
    }

    fn afterConstSlice(header: *const Header) []const u8 {
        const buffer: [*]const u8 = @ptrCast(header);
        return buffer[@sizeOf(Header) + header.before_len ..][0..header.after_len];
    }
};

const Mutation = enum {
    insert_byte,
    mutate_byte,
    remove_byte,

    pub fn pick(random: std.Random, constraint: enum { any, space_limited }) Mutation {
        return @enumFromInt(random.weightedIndex(f32, &.{
            if (constraint == .space_limited) 0 else 5,
            1,
            1,
        }));
    }
};
