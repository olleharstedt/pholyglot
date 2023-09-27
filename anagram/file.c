/**
 * From php-src/ext/standard/file.c etc
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "php_streams.h"

char* file_get_contents(char* filename)
{
	size_t filename_len;
	bool use_include_path = 0;
	php_stream *stream;
	zend_long offset = 0;
	zend_long maxlen;
	bool maxlen_is_null = 1;
	zval *zcontext = NULL;
	php_stream_context *context = NULL;
	zend_string *contents;

	/* Parse arguments */
    /*
	ZEND_PARSE_PARAMETERS_START(1, 5)
		Z_PARAM_PATH(filename, filename_len)
		Z_PARAM_OPTIONAL
		Z_PARAM_BOOL(use_include_path)
		Z_PARAM_RESOURCE_OR_NULL(zcontext)
		Z_PARAM_LONG(offset)
		Z_PARAM_LONG_OR_NULL(maxlen, maxlen_is_null)
	ZEND_PARSE_PARAMETERS_END();
    */

	if (maxlen_is_null) {
		maxlen = (ssize_t) PHP_STREAM_COPY_ALL;
	} else if (maxlen < 0) {
		zend_argument_value_error(5, "must be greater than or equal to 0");
		RETURN_THROWS();
	}

	context = php_stream_context_from_zval(zcontext, 0);

	stream = php_stream_open_wrapper_ex(filename, "rb",
				(use_include_path ? USE_PATH : 0) | REPORT_ERRORS,
				NULL, context);
	if (!stream) {
		RETURN_FALSE;
	}

	/* disabling the read buffer allows doing the whole transfer
	   in just one read() system call */
	if (php_stream_is(stream, PHP_STREAM_IS_STDIO)) {
		php_stream_set_option(stream, PHP_STREAM_OPTION_READ_BUFFER, PHP_STREAM_BUFFER_NONE, NULL);
	}

	if (offset != 0 && php_stream_seek(stream, offset, ((offset > 0) ? SEEK_SET : SEEK_END)) < 0) {
		php_error_docref(NULL, E_WARNING, "Failed to seek to position " ZEND_LONG_FMT " in the stream", offset);
		php_stream_close(stream);
		RETURN_FALSE;
	}

	if ((contents = php_stream_copy_to_mem(stream, maxlen, 0)) != NULL) {
		RETVAL_STR(contents);
	} else {
		RETVAL_EMPTY_STRING();
	}

	php_stream_close(stream);
}
