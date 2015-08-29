! (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.
! %i0 = argc
! %i1 = func (no tag)
! %i2 = args
! %i3 = num_args

	.proc	16
	.global	_apply_function_1
_apply_function_1:
	save	 %sp,((-128 * 4) - 96),%sp
	ld	[%i2 + 0],%o1
	ld	[%i2 + 4],%o2
	ld	[%i2 + 8],%o3
	ld	[%i2 + 12],%o4
	ld	[%i2 + 16],%o5
	cmp	%i3,5
	ble	docall
	sll	%i3,2,%i3
	mov	5*4,%l0
	mov	92,%l1
copy_args:
	ld	[%i2 + %l0] ,%l2
	st	%l2,[%sp + %l1]
	add	%l0,4,%l0
	cmp	%l0,%i3
	bl	copy_args
	add	%l1,4,%l1
docall:
	jmpl	%i1,%o7
	mov	%i0,%o0	
	ret
	restore	%g0,%o0,%o0

	.global	_save_all_registers
_save_all_registers:
	ta	3
	retl				
	nop
