/* These functions return -1 if there wasn't enough space in output.
 * LZDecompress can also return -1 if the input data was malformed,
 * Returns the number of bytes written to output on success. */

int uxn_lz_compress(void *output, int output_size, const void *input, int input_size);
int uxn_lz_decompress(void *output, int output_size, const void *input, int input_size);

typedef struct uxn_lz_decompress_t {
	unsigned char *next_in, *next_out;
	int avail_in, avail_out;

	int dict_len, dict_read_pos, dict_write_pos, copy_num, state;
	unsigned char dict[256];
} uxn_lz_decompress_t;

int uxn_lz_decompress_stream(uxn_lz_decompress_t *a);
