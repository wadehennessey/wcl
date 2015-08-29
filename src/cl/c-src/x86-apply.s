		.align 4
# This is a pretty dumb version of apply - it could be made better.
# Stack after frame setup looks like:	
# numargs 20(%ebp)
# argv    16(%ebp)
# func	  12(%ebp)
# argc	   8(%ebp)
#
	.globl apply_function_1
	.type	 apply_function_1,@function
apply_function_1:	
	pushl %ebp
	movl %esp,%ebp		# new frame
	movl 20(%ebp), %edx	# numargs
	cmpl $0, %edx
	je  .L2
	movl 16(%ebp), %eax	# pointer to start of arg vector
	decl %edx
	sall $2, %edx
	addl %edx, %eax		# point to last argument
	
.L1:	
	pushl (%eax)
	cmpl $0, %edx
	je   .L2
	subl $4, %edx		# this is really stupid...improve one day
	subl $4, %eax
	jmp  .L1
.L2:
	pushl 8(%ebp)
	movl 12(%ebp), %eax
	call *%eax
	leave
	ret
