! SPARC integer arithmetic routines to catch fixnum->bignum overflow cases 
!
! HEY! these add and subtract sequences look too long for the
! fixnum cases, but I keep getting screwed by the "back-to-back delayed
! controlled transfer" semantics of SPARC. Maybe look at these later...
	.seg	"text"
	.proc	4
	.global	_integer_add
_integer_add:
	mov	%o0,%o2
	addcc 	%o0,%o1,%o0
	bvs	L1			
	nop			
	retl				! Yikes! Target inst is always executed
	nop
L1:	save	%sp,-96,%sp
	mov	%i2,%o0
	call	_addition_overflow_handler,2
	mov	%i1,%o1
	ret
	restore	%g0,%o0,%o0

	.seg	"text"
	.proc	4
	.global	_integer_subtract
_integer_subtract:
	mov	%o0,%o2
	subcc 	%o0,%o1,%o0
	bvs	L2
	nop
	retl
	nop
L2:	save	%sp,-96,%sp
	mov	%i2,%o0
	call	_subtraction_overflow_handler,2
	mov	%i1,%o1
	ret
	restore	%g0,%o0,%o0

.proc	4
	.global	_integer_multiply
_integer_multiply:	
	save	%sp,-96,%sp
	sra	%i0,1,%o0
	call	.mul,2
	mov	%i1,%o1
	be	L3
	nop
	sra	%i0,1,%o0
	call	_multiply_overflow_handler,2
	mov	%i1,%o1
L3:	ret
	restore	%g0,%o0,%o0

	.global	_copy_regs_to_stack
_copy_regs_to_stack:
	ta	3			! flush register windows
	jmp	%o7+8			! return
	add	%sp, 0, %o0		! return stack pointer


