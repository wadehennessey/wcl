		.align 4
# This is a pretty dumb version of apply - it could be made better.
# Args are passed as follows:	
# arg1 - %rdi = argc
# arg2 - %rsi = func
# arg3 - %rdx = argv
# arg4 - %rcx = numargs
# arg5 - %r8
# arg6 - %r9
# arg7 - (%rsp)
# arg8 - 8(%rsp)	
# etc. on stack	
#
# %r10, %rax, and %r11 are tmp regs available for use		
	.globl apply_function_1
	.type	 apply_function_1,@function
apply_function_1:
	pushq	%rbp
	movq	%rsp, %rbp	# new frame
	movq	%rcx, %rax	# numargs in %rax
	movq	%rsi, %r10	# func in %r10
	movq	%rdx, %r11	# argv in %r11
	cmpq	$0,   %rax	# just pass argc as is in %rdi
	je	.L2
	movq	(%r11), %rsi
	cmpq	$1, %rax
	je	.L2
	movq	8(%r11), %rdx
	cmpq	$2, %rax
	je	.L2
	movq	16(%r11), %rcx
	cmpq	$3, %rax
	je	.L2
	movq	24(%r11), %r8
	cmpq	$4, %rax
	je	.L2
	movq	32(%r11), %r9
	cmpq	$5, %rax			
	je	.L2
	addq	$32, %r11
	subq	$5, %rax
	salq	$3, %rax
	addq	%rax, %r11	# point to last arg
.L1:	
	pushq	(%r11)		# push args on from last to first not in reg
	subq	$8, %rax
	cmpq	$0, %rax
	je	 .L2
	subq	$8, %r11
	jmp	.L1
.L2:
	movl	$0, %eax	# C varargs use this for SSE float arg count
	call	*%r10
	leave
	ret
