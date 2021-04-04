	.text
	.file	"complyed"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	x@GOTPCREL(%rip), %rbx
	movl	(%rbx), %esi
	leaq	.Lfmt(%rip), %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	movl	$1, (%rbx)
	xorl	%eax, %eax
	popq	%rbx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	x,@object               # @x
	.bss
	.globl	x
	.p2align	2
x:
	.long	0                       # 0x0
	.size	x, 4

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


	.section	".note.GNU-stack","",@progbits
