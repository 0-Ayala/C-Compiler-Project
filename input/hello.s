
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	.section	.rodata
lab005:
	.string	"Hello world"
	.text
	movl	$lab005, %edi
	call	puts
	popq	%rbp
	ret

	.section .rodata
.output:
	.string "%d\n"

