	.text
	.globl	Main
	.type	Main, @function
Main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	$0
	popq	%rax
	movq	%rax, (count)
	pushq	$100
	popq	%rax
	movq	%rax, (number)
	jmp	lab006
lab005:
	pushq	(count)
	pushq	$15
	popq	%rbx
	popq	%rax
	cltd
	idivq	%rbx
	movq	%rdx,	%rax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab007
	jmp	lab008
lab007:
	pushq	(count)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (count)
	jmp	lab009
lab008:
	pushq	(count)
	pushq	$3
	popq	%rbx
	popq	%rax
	cltd
	idivq	%rbx
	movq	%rdx,	%rax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab010
	jmp	lab011
lab010:
	pushq	(count)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (count)
	jmp	lab012
lab011:
	pushq	(count)
	pushq	$5
	popq	%rbx
	popq	%rax
	cltd
	idivq	%rbx
	movq	%rdx,	%rax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab013
	jmp	lab014
lab013:
	pushq	(count)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (count)
	jmp	lab015
lab014:
	.section	.rodata
LC0:
	.string	"count"
	.text
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab015:
lab012:
lab009:
lab006:
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
	jne	lab005
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