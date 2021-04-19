	.text
	.file	"complyed"
	.globl	add_3_to                # -- Begin function add_3_to
	.p2align	4, 0x90
	.type	add_3_to,@function
add_3_to:                               # @add_3_to
	.cfi_startproc
# %bb.0:                                # %entry
                                        # kill: def %esi killed %esi def %rsi
                                        # kill: def %edi killed %edi def %rdi
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	leal	(%rdi,%rsi), %eax
	addl	-12(%rsp), %eax
	retq
.Lfunc_end0:
	.size	add_3_to, .Lfunc_end0-add_3_to
	.cfi_endproc
                                        # -- End function
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$39, %edi
	movl	$3, %esi
	callq	add_3_to@PLT
	movl	%eax, %ecx
	movq	a@GOTPCREL(%rip), %rax
	movl	%ecx, (%rax)
	leaq	.Lfmt.3(%rip), %rdi
	xorl	%eax, %eax
	movl	%ecx, %esi
	callq	printf@PLT
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.type	a,@object               # @a
	.bss
	.globl	a
	.p2align	2
a:
	.long	0                       # 0x0
	.size	a, 4

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"%g\n"
	.size	.Lfmt.1, 4

	.type	.Lfmt.2,@object         # @fmt.2
.Lfmt.2:
	.asciz	"%s\n"
	.size	.Lfmt.2, 4

	.type	.Lfmt.3,@object         # @fmt.3
.Lfmt.3:
	.asciz	"%d\n"
	.size	.Lfmt.3, 4

	.type	.Lfmt.4,@object         # @fmt.4
.Lfmt.4:
	.asciz	"%f\n"
	.size	.Lfmt.4, 4

	.type	.Lfmt.5,@object         # @fmt.5
.Lfmt.5:
	.asciz	"%s\n"
	.size	.Lfmt.5, 4


	.section	".note.GNU-stack","",@progbits
