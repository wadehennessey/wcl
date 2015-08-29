.text
	.align	8
.globl integer_add
integer_add:
	movq	%rdi, %rax
	addq	%rsi,%rax
	jo	.L1
	ret
.L1:	jmp	 addition_overflow_handler@PLT

	.align	8
.globl integer_subtract
	.type	 integer_subtract,@function
integer_subtract:
	movq	%rdi, %rax
	subq	%rsi,%rax
	jo	.L2
	ret
.L2:	jmp	subtraction_overflow_handler@PLT
	
	.align	8
.globl integer_multiply
integer_multiply:
	movq	%rdi, %rax
	sarq	$1, %rax
	imulq	%rsi,%rax
	jo	.L3
	ret
.L3:	jmp	multiply_overflow_handler@PLT
	
	.align	8
.globl	copy_regs_to_stack
copy_regs_to_stack:
	pushq	%rbp
	movq	%rsp,%rbp
	movq	%rbx, (%rdi)
	movq	%r12, 8(%rdi)
	movq	%r13, 16(%rdi)
	movq	%r14, 24(%rdi)
	movq	%r15, 32(%rdi)
	movq	%r15, 40(%rdi)	
	leave
	ret
	
