	.text
	.file	"complyed"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI0_0:
	.quad	4620242857719391846     # double 7.5999999999999996
.LCPI0_1:
	.quad	4614388178203810201     # double 3.1999999999999997
.LCPI0_2:
	.quad	4621267426634618634     # double 9.0199999999999996
.LCPI0_3:
	.quad	4613262278296967578     # double 2.7000000000000002
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	leaq	.Lfmt.1(%rip), %r14
	movsd	.LCPI0_0(%rip), %xmm0   # xmm0 = mem[0],zero
	movb	$1, %al
	movq	%r14, %rdi
	callq	printf@PLT
	leaq	.Lfmt.2(%rip), %rbx
	leaq	.Lstr(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	printf@PLT
	movsd	.LCPI0_1(%rip), %xmm0   # xmm0 = mem[0],zero
	movb	$1, %al
	movq	%r14, %rdi
	callq	printf@PLT
	leaq	.Lstr.3(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	printf@PLT
	movsd	.LCPI0_2(%rip), %xmm0   # xmm0 = mem[0],zero
	movb	$1, %al
	movq	%r14, %rdi
	callq	printf@PLT
	leaq	.Lstr.4(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	printf@PLT
	movsd	.LCPI0_3(%rip), %xmm0   # xmm0 = mem[0],zero
	movb	$1, %al
	movq	%r14, %rdi
	callq	printf@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"%f\n"
	.size	.Lfmt.1, 4

	.type	.Lfmt.2,@object         # @fmt.2
.Lfmt.2:
	.asciz	"%s\n"
	.size	.Lfmt.2, 4

	.type	.Lstr,@object           # @str
.Lstr:
	.asciz	" "
	.size	.Lstr, 2

	.type	.Lstr.3,@object         # @str.3
.Lstr.3:
	.asciz	" "
	.size	.Lstr.3, 2

	.type	.Lstr.4,@object         # @str.4
.Lstr.4:
	.asciz	" "
	.size	.Lstr.4, 2


	.section	".note.GNU-stack","",@progbits
