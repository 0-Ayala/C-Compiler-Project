
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	$0
	popq	%rax
	movq	%rax, (count)
	pushq	$100
	popq	%rax
	movq	%rax, (number)
	jmp	lab007
lab006:
	pushq	(count)
	pushq	$15
	popq	%rbx
	popq	%rax
	cltd
	idivq	%rbx
	movq	%rdx,	%rax
	pushq	%rax
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sete	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab008
	jmp	lab009
lab008:
	jmp	lab010
lab009:
	pushq	(count)
	pushq	$3
	popq	%rbx
	popq	%rax
	cltd
	idivq	%rbx
	movq	%rdx,	%rax
	pushq	%rax
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sete	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab011
	jmp	lab012
lab011:
	jmp	lab013
lab012:
	pushq	(count)
	pushq	$5
	popq	%rbx
	popq	%rax
	cltd
	idivq	%rbx
	movq	%rdx,	%rax
	pushq	%rax
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sete	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab014
	jmp	lab015
lab014:
	jmp	lab016
lab015:
	pushq	(count)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab016:
lab013:
lab010:
	pushq	(count)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (count)
lab007:
	pushq	(count)
	pushq	(number)
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab006
	popq	%rbp
	ret

	.section .rodata
.output:
	.string "%d\n"

	.globl	count
	.data
	.align	8
	.size	count, 8
count:
	.quad	0

	.globl	number
	.data
	.align	8
	.size	number, 8
number:
	.quad	0

