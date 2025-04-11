const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const dizzy = b.addModule("dizzy", .{
        .root_source_file = b.path("src/dizzy.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe_unit_tests = b.addTest(.{
        .root_module = dizzy,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // Fuzzing only works well after Zig commit 3830fc041b2ccfc989546ed1d689cde6a38dc647
    if (b.option(bool, "enable-fuzzing", "Fuzzing") orelse false) blk: {
        const libfuzzer_kit = b.lazyDependency("libfuzzer_kit", .{}) orelse break :blk;

        const fuzz_dizzy_obj = b.addObject(.{
            .name = "fuzz-dizzy",
            .root_module = b.createModule(.{
                .target = target,
                .optimize = optimize,
                .root_source_file = b.path("src/fuzz.zig"),
            }),
        });

        const fuzz_dizzy_instrumented_obj = b.addObject(.{
            .name = "fuzz-dizzy-instrumented",
            .root_module = b.createModule(.{
                .target = target,
                .optimize = optimize,
            }),
        });
        fuzz_dizzy_instrumented_obj.addCSourceFile(.{
            .file = fuzz_dizzy_obj.getEmittedLlvmIr(),
            .flags = &.{"-fsanitize=fuzzer-no-link"},
        });

        const fuzz_dizzy = b.addExecutable(.{
            .name = "fuzz-dizzy",
            .root_module = b.createModule(.{
                .target = target,
                .optimize = optimize,
            }),
        });
        fuzz_dizzy.addObject(fuzz_dizzy_instrumented_obj);
        fuzz_dizzy.linkLibrary(libfuzzer_kit.artifact("fuzzer"));
        fuzz_dizzy.addCSourceFile(.{
            .file = libfuzzer_kit.path("src/FuzzerMain.cpp"),
            .language = .cpp,
        });

        const run_fuzz_dizzy = b.addRunArtifact(fuzz_dizzy);
        if (b.args) |args| run_fuzz_dizzy.addArgs(args);
        const fuzz_dizzy_step = b.step("fuzz", "Fuzz dizzy");
        fuzz_dizzy_step.dependOn(&run_fuzz_dizzy.step);
    }
}
