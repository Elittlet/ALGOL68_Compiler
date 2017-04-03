	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// printstring ((read (hello, world)));
	pushq	(hello)
	pushq	(world)
	popq	%rbx
	popq	%rax
	movq	(%rax,%rbx,8), %rax
	pushq	%rax
	popq	%rdi
	call	print_string
// return (0);
	pushq	$0
	popq	%rax
	popq	%rbp
	ret
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

	.globl	hello
	.data
	.align	8
	.size	hello, 8
hello:
	.quad	0

	.globl	world
	.data
	.align	8
	.size	world, 8
world:
	.quad	0

