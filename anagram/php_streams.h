struct _php_stream  {
	const php_stream_ops *ops;
	void *abstract;			/* convenience pointer for abstraction */

	php_stream_filter_chain readfilters, writefilters;

	php_stream_wrapper *wrapper; /* which wrapper was used to open the stream */
	void *wrapperthis;		/* convenience pointer for an instance of a wrapper */
	zval wrapperdata;		/* fgetwrapperdata retrieves this */

	uint8_t is_persistent:1;
	uint8_t in_free:2;			/* to prevent recursion during free */
	uint8_t eof:1;
	uint8_t __exposed:1;	/* non-zero if exposed as a zval somewhere */

	/* so we know how to clean it up correctly.  This should be set to
	 * PHP_STREAM_FCLOSE_XXX as appropriate */
	uint8_t fclose_stdiocast:2;

	/* flag to mark whether the stream has buffered data */
	uint8_t has_buffered_data:1;

	char mode[16];			/* "rwb" etc. ala stdio */

	uint32_t flags;	/* PHP_STREAM_FLAG_XXX */

	zend_resource *res;		/* used for auto-cleanup */
	FILE *stdiocast;    /* cache this, otherwise we might leak! */
	char *orig_path;

	zend_resource *ctx;

	/* buffer */
	zend_off_t position; /* of underlying stream */
	unsigned char *readbuf;
	size_t readbuflen;
	zend_off_t readpos;
	zend_off_t writepos;

	/* how much data to read when filling buffer */
	size_t chunk_size;

#if ZEND_DEBUG
	const char *open_filename;
	uint32_t open_lineno;
#endif

	struct _php_stream *enclosing_stream; /* this is a private stream owned by enclosing_stream */
}; /* php_stream */
