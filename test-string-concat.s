	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	L_str(%rip), %rdi
	movq	%rdi, _a(%rip)
	leaq	L_str.3(%rip), %rsi
	movq	%rsi, _b(%rip)
	callq	_string_concat
	movq	%rax, _c(%rip)
	leaq	L_fmt.2(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_c                              ## @c
.zerofill __DATA,__common,_c,8,3
	.globl	_b                              ## @b
.zerofill __DATA,__common,_b,8,3
	.globl	_a                              ## @a
.zerofill __DATA,__common,_a,8,3
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%d\n"

L_fmt.1:                                ## @fmt.1
	.asciz	"%g\n"

L_fmt.2:                                ## @fmt.2
	.asciz	"%s\n"

L_str:                                  ## @str
	.asciz	"hello "

L_str.3:                                ## @str.3
	.asciz	" world!"

.subsections_via_symbols
