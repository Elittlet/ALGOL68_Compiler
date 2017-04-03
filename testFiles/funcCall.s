	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// dummy := (printX (x, y));
	pushq	(y)
	pushq	(x)
	call	printX
	addq	$16, %rsp
	pushq	%rax
	popq	%rax
	movq	%rax, (dummy)
	popq	%rbp
	ret

	.text
	.globl	printX
	.type	printX, @function
printX:
	pushq	%rbp
	movq	%rsp, %rbp
// print (x);
	pushq	16(%rbp)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	popq	%rbp
	ret

	.text
	.globl	print_string
	.type	print_string, @function
print_string:
.LFB0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	jmp	.L2
.L3:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movsbl	%al, %eax
	movl	%eax, %edi
	call	putchar
	addq	$8, -8(%rbp)
.L2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L3
	leave
	ret

	.section .rodata
.output:
	.string "%d\n"

	.globl	dummy
	.data
	.align	8
	.size	dummy, 8
dummy:
	.quad	0

	.globl	x
	.data
	.align	8
	.size	x, 8
x:
	.quad	0

	.globl	y
	.data
	.align	8
	.size	y, 8
y:
	.quad	0

