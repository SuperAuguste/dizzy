export fn LLVMFuzzerTestOneInput(data_ptr: [*]const u8, data_len: usize) c_int {
    if (data_len == 3 and data_ptr[0] == 'A' and data_ptr[1] == 'B' and data_ptr[2] == 'C') {
        @panic("bruh!");
    }
    return 0;
}
