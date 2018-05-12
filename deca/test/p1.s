.text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	addq $-8, %rsp
	movl $1, %eax
	negl %eax
	movq %rax, -8(%rbp)
	xorl %eax, %eax
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rax
	cmpl $0, %eax
	jne __label__00003
	movl $1, %eax
	jmp __label__00004
__label__00003:
	xorl %eax, %eax
__label__00004:
	cmpl $0, %eax
	je __label__00008
	movq -8(%rbp), %rax
	pushq %rax
	movl $1, %eax
	negl %eax
	popq %r9
	cmpl %eax, %r9d
	je __label__00007
	movl $0, %eax
__label__00007:
__label__00008:
	cmpl $0, %eax
	je __label__00010
	movq -8(%rbp), %rax
	pushq %rax
	movl $3, %eax
	popq %r9
	addl %r9d, %eax
	movq %rax, -8(%rbp)
__label__00010:
	movq -8(%rbp), %rax
	call __builtin_print_int
__exit_main:
	movq %rbp, %rsp
	popq %rbp
	movq $0, %rax
	ret
#Primitives Assembleur
__builtin_print_int:
	movq $__str_d, %rdi
	movq %rax, %rsi
	xor %rax, %rax
	call printf
	ret
__builtin_print_boolean:
	cmpl $0, %eax
	je __print_boolean_false
	movq $__str_true, %rdi
	jmp __print_boolean_next
__print_boolean_false:
	movq $__str_false, %rdi
__print_boolean_next:
	call puts
	ret
__builtin_print_String:
	movq $__str_null, %rdi
	cmpq $0, %rax
	je __builtin_print_String_next
	movq 8(%rax), %rdi
__builtin_print_String_next:
	call puts
	ret
__builtin_null_error:
	movq $__null_error_msg, %rdi
	call puts
	movq $1, %rdi
	call exit
__builtin_div0_error:
	movq $__div0_error_msg, %rdi
	call puts
	movq $1, %rdi
	call exit
.data
#Définitions des constantes
__str_true:
	.string "true"
__str_false:
	.string "false"
__str_null:
	.string "null"
__str_d:
	.string "%d"
__null_error_msg:
	.string "null pointer exception\n"
__div0_error_msg:
	.string "division by 0 exception\n"
