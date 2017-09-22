
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	$0
	popq	%rax
	movq	%rax, (number)
	jmp	lab002
lab001:
	pushq	(number)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	pushq	(number)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (number)
lab002:
	pushq	(number)
	pushq	$101
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab001
	popq	%rbp
	ret

	.section .rodata
.output:
	.string "%d\n"

	.globl	number
	.data
	.align	8
	.size	number, 8
number:
	.quad	0

