
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	$1
	popq	%rax
	movq	%rax, (accumulator)
	pushq	$1
	popq	%rax
	movq	%rax, (factor)
	pushq	$10
	popq	%rax
	movq	%rax, (number)
	jmp	lab004
lab003:
	pushq	(accumulator)
	pushq	(factor)
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (accumulator)
	pushq	(factor)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (factor)
	pushq	(accumulator)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab004:
	pushq	(factor)
	pushq	(number)
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab003
	popq	%rbp
	ret

	.section .rodata
.output:
	.string "%d\n"

	.globl	accumulator
	.data
	.align	8
	.size	accumulator, 8
accumulator:
	.quad	0

	.globl	factor
	.data
	.align	8
	.size	factor, 8
factor:
	.quad	0

	.globl	number
	.data
	.align	8
	.size	number, 8
number:
	.quad	0

