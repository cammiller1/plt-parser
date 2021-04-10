	.text
	.file	"complyed"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI0_0:
	.quad	4616820122002590269     # double 4.5599999999999996
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	x.3@GOTPCREL(%rip), %rax
	movabsq	$4616820122002590269, %rcx # imm = 0x40123D70A3D70A3D
	movq	%rcx, (%rax)
	leaq	.Lfmt(%rip), %rdi
	movsd	.LCPI0_0(%rip), %xmm0   # xmm0 = mem[0],zero
	movb	$1, %al
	callq	printf@PLT
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	x,@object               # @x
	.bss
	.globl	x
	.p2align	3
x:
	.quad	0                       # double 0
	.size	x, 8

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

	.type	x.3,@object             # @x.3
	.bss
	.globl	x.3
	.p2align	3
x.3:
	.quad	0                       # double 0
	.size	x.3, 8


	.section	".note.GNU-stack","",@progbits
