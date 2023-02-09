#include "uxn_lz.h"

enum { MinMatchLength = 4 };

int
uxn_lz_compress(void *output, int output_size, const void *input, int input_size)
{
	int dict_len, match_len, i, string_len, match_ctl;
	unsigned char *out = output, *combine = 0;
	const unsigned char *dict, *dict_best = 0, *in = input, *start = in, *end = in + input_size;
	while (in != end)
	{
		dict_len = (int)(in - start); /* Get available dictionary size (history of original output) */
		if (dict_len > 256) dict_len = 256; /* Limit history lookback to 256 bytes */
		dict = in - dict_len; /* Start of dictionary */
		string_len = (int)(end - in); /* Size of the string to search for */
		if (string_len > 0x3FFF + MinMatchLength) string_len = 0x3FFF + MinMatchLength;
		/* ^ Limit string length to what we can fit in 14 bits, plus the minimum match length */
		match_len = 0; /* This will hold the length of our best match */
		for (; dict_len; dict += 1, dict_len -= 1) /* Iterate through the dictionary */
		{
			for (i = 0;; i++) /* Find common prefix length with the string */
			{
				if (i == string_len) { match_len = i; dict_best = dict; goto done_search; }
				/* ^ If we reach the end of the string, this is the best possible match. End. */
				if (in[i] != dict[i % dict_len]) break; /* Dictionary repeats if we hit the end */
			}
			if (i > match_len) { match_len = i; dict_best = dict; }
		}
done_search:
		if (match_len >= MinMatchLength) /* Long enough? Use dictionary match */
		{
			if ((output_size -= 2) < 0) goto overflow;
			match_ctl = match_len - MinMatchLength; /* More numeric range: treat 0 as 4, 1 as 5, etc. */
			if (match_ctl > 0x3F) /* Match is long enough to use 2 bytes for the size */
			{
				if ((output_size -= 1) < 0) goto overflow;
				*out++ = match_ctl >> 8 | 0x40 | 0x80; /* High byte of the size, with both flags set */
				*out++ = match_ctl; /* Low byte of the size */
			}
			else /* Use 1 byte for the size */
			{
				*out++ = match_ctl | 0x80; /* Set the "dictionary" flag */
			}
			*out++ = in - dict_best - 1; /* Write offset into history. (0 is -1, 1 is -2, ...) */
			in += match_len; /* Advance input by size of the match */
			combine = 0; /* Disable combining previous literal, if any */
			continue;
		}
		if (combine) /* Combine with previous literal */
		{
			if ((output_size -= 1) < 0) goto overflow;
			if (++*combine == 127) combine = 0; /* If the literal hits its size limit, terminate it. */
		}
		else /* Start a new literal */
		{
			if ((output_size -= 2) < 0) goto overflow;
			combine = out++; /* Store this address, and later use it to increment the literal size. */
			*combine = 0; /* The 0 here means literal of length 1. */
		}
		*out++ = *in++; /* Write 1 literal byte from the input to the output. */
	}
	return (int)(out - (unsigned char *)output);
	overflow: return -1;
}

int
uxn_lz_expand(void *output, int output_size, const void *input, int input_size)
{
	int num, offset, written = 0;
	unsigned char *out = output;
	const unsigned char *from, *in = input;
	while (input_size)
	{
		num = *in++;
		if (num > 127) /* Dictionary */
		{
			if ((input_size -= 1) < 0) goto malformed;
			num &= 0x7F;
			if (num & 0x40)
			{
				if ((input_size -= 1) < 0) goto malformed;
				num = *in++ | num << 8 & 0x3FFF;
			}
			num += MinMatchLength;
			offset = *in++ + 1;
			if (offset > written) goto malformed;
			from = out + written - offset;
		}
		else /* Literal */
		{
			input_size -= ++num;
			if (input_size < 0) goto malformed;
			from = in, in += num;
		}
		if (written + num > output_size) goto overflow;
		while (num--) out[written++] = *from++;
	}
	return written;
	overflow: malformed: return -1;
}

int
uxn_lz_expand_stream(struct uxn_lz_expand_t *a)
{
	/* Copy struct to stack variables for compiler optimizations */
	unsigned char *next_in = a->next_in, *next_out = a->next_out;
	int avail_in = a->avail_in, avail_out = a->avail_out;
	int dict_len = a->dict_len, copy_num = a->copy_num;
	unsigned char dict_read_pos = a->dict_read_pos, dict_write_pos = a->dict_write_pos, *dict = a->dict;
	int result = 0;
	switch (a->state)
	{
case 0:
	for (; avail_in;)
	{
		copy_num = *next_in++;
		avail_in--;
		if (copy_num > 127) /* Dictionary */
		{
			copy_num &= 0x7F;
			if (copy_num & 0x40)
			{
case 1:
				if (!avail_in) { a->state = 1; goto need_more; }
				avail_in--;
				copy_num = *next_in++ | copy_num << 8 & 0x3FFF;
			}
			copy_num += MinMatchLength;
case 2:
			if (!avail_in) { a->state = 2; goto need_more; }
			avail_in--;
			dict_read_pos = *next_in++ + 1;
			if (dict_read_pos > dict_len) { a->state = 5; result = -1; goto flush; } /* Malformed */
			dict_read_pos = dict_write_pos - dict_read_pos;
			if ((dict_len += copy_num) > 256) dict_len = 256;
case 3:
			do {
				if (!avail_out) { a->state = 3; goto need_more; }
				*next_out++ = dict[dict_write_pos++] = dict[dict_read_pos++];
				avail_out--;
			} while (--copy_num);
		}
		else /* Literal */
		{
			copy_num++;
			if ((dict_len += copy_num) > 256) dict_len = 256;
case 4:
			do {
				if (!avail_in || !avail_out) { a->state = 4; goto need_more; }
				*next_out++ = dict[dict_write_pos++] = *next_in++;
				avail_in--, avail_out--;
			} while (--copy_num);
		}
	}
	a->state = 0;
case 5:;
	}
need_more: flush:
	/* Flush stack variables back to struct */
	a->next_in = next_in, a->next_out = next_out;
	a->avail_in = avail_in, a->avail_out = avail_out;
	a->dict_len = dict_len, a->copy_num = copy_num;
	a->dict_read_pos = dict_read_pos, a->dict_write_pos = dict_write_pos;
	return result;
}

unsigned int uxn_checksum(unsigned int seed, void *bytes, unsigned int bytes_size)
{
	unsigned int x = seed >> 16, y = seed, c;
	unsigned char *in = bytes, *end = in + bytes_size;
	for (; in != end; in++) {
		c = *in << 8 | *in;
		x = x * 0x2443 + c;
		y = y * 0x118d + c;
	}
	return x << 16 | (y & 0xFFFF);
}
