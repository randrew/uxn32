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
uxn_lz_decompress(void *output, int output_size, const void *input, int input_size)
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
uxn_lz_decompress_stream(struct uxn_lz_decompress_t *a)
{
	switch (a->state)
	{
case 0:
	while (a->avail_in)
	{
		a->copy_num = *a->next_in++;
		a->avail_in--;
		if (a->copy_num > 127) /* Dictionary */
		{
			a->copy_num &= 0x7F;
			if (a->copy_num & 0x40)
			{
case 1:
				if (!a->avail_in) { a->state = 1; goto need_more_input; }
				a->avail_in--;
				a->copy_num = *a->next_in++ | a->copy_num << 8 & 0x3FFF;
			}
			a->copy_num += MinMatchLength;
case 2:
			if (!a->avail_in) { a->state = 2; goto need_more_input; }
			a->avail_in--;
			a->dict_read_pos = *a->next_in++ + 1;
			if (a->dict_read_pos > a->dict_len) return -1; /* Malformed */
			a->dict_read_pos = (a->dict_write_pos + 256 - a->dict_read_pos) % 256;
			a->state = 3;
case 3:
			do {
				if (!a->avail_out) goto need_more_output;
				*a->next_out++ = a->dict[a->dict_write_pos++] = a->dict[a->dict_read_pos++];
				a->avail_out--;
				if (a->dict_len < 256) a->dict_len++;
			} while (--a->copy_num);
		}
		else /* Literal */
		{
			a->copy_num++;
			a->state = 4;
case 4:
			do {
				if (!a->avail_in) goto need_more_input;
				if (!a->avail_out) goto need_more_output;
				*a->next_out++ = a->dict[a->dict_write_pos++] = *a->next_in++;
				a->avail_in--, a->avail_out--;
				if (a->dict_len < 256) a->dict_len++;
			} while (--a->copy_num);
		}
	}
	}
need_more_input: need_more_output:
	return 0;
}
