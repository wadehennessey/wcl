! (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

	.seg	"text"
	.proc	4
	.global	_flush_cache
_flush_cache:
	flush	%o0
	retl				
	nop

