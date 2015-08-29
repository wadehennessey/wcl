.text
	.align 4
.globl integer_add
	.type	 integer_add,@function
integer_add:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%edx
	addl 12(%ebp),%edx
	leave
	jo   .L1
	movl %edx,%eax
	ret
	.align 4
.L1:	jmp  addition_overflow_handler

.Lfe1:
	.size	 integer_add,.Lfe1-integer_add
	.align 4
.globl integer_subtract
	.type	 integer_subtract,@function
integer_subtract:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%edx
	subl 12(%ebp),%edx
	leave
	jo   .L2
	movl %edx,%eax
	ret
	.align 4
.L2:
	jmp   subtraction_overflow_handler
	
.Lfe2:
	.size	 integer_subtract,.Lfe2-integer_subtract
	.align 4
.globl integer_multiply
	.type	 integer_multiply,@function
integer_multiply:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%edx
	sarl $1, %edx
	imull 12(%ebp),%edx
	leave
	jo   .L3
	movl %edx,%eax
	ret
	.align 4
.L3:
	jmp    multiply_overflow_handler
	
.Lfe3:
	.size	 integer_multiply,.Lfe3-integer_multiply

	.align	4
.globl	copy_regs_to_stack
	.type	 copy_regs_to_stack,@function
copy_regs_to_stack:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp), %eax
	movl %ecx, (%eax)
	movl %edx, 4(%eax)
	movl %ebx, 8(%eax)
	movl %ebp, 12(%eax)
	movl %esi, 16(%eax)
	leave
	ret
	