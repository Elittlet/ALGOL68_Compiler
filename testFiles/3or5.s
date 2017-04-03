	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// for (x := 0 to 100)
	pushq	$0
	popq	%rax
	movq	%rax, (x)
	jmp	lab002
lab001:
// ((x % 3) < 1)
	pushq	(x)
	pushq	$3
	popq	%rbx
	popq	%rax
	cqto
	idivq	%rbx
	movq	%rdx, %rax
	pushq	%rax
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab003
	jmp	lab004
lab003:
// y := 0;
	pushq	$0
	popq	%rax
	movq	%rax, (y)
	jmp	lab005
lab004:
// print (x);
	pushq	(x)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab005:
// ((x % 5) < 1)
	pushq	(x)
	pushq	$5
	popq	%rbx
	popq	%rax
	cqto
	idivq	%rbx
	movq	%rdx, %rax
	pushq	%rax
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab006
	jmp	lab007
lab006:
// g := 0;
	pushq	$0
	popq	%rax
	movq	%rax, (g)
	jmp	lab008
lab007:
// print (x);
	pushq	(x)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab008:
	movq	(x), %rax
	addq	$1, %rax
	movq	%rax, (x)
lab002:
	pushq	$100
	popq	%rbx
	movq	(x), %rax
	cmpq	%rbx, %rax
	jle	lab001
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

	.globl	g
	.data
	.align	8
	.size	g, 8
g:
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

