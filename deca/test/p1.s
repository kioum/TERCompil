.text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	addq $0, %rsp
	movl $2, %eax
	cmpl $0, %eax
	jne __label__00001
	call __builtin_div0_error
__label__00001:
	movl $2, %eax
	pushq %rax
	movl $4, %eax
	popq %r9
	movl $0, %edx
	movl %r9d, %ebx
	idivl %ebx
	movl %edx, %eax
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	pushq %rax
	movl $0, %eax
	popq %r9
	cmpl %eax, %r9d
	je __label__00002
	movl $0, %eax
__label__00002:
	cmpl $0, %eax
	je __label__00004
	movq $24, %rdi
	call malloc
	movq $1234, 0(%rax)
	movq $__str__label__00003, 8(%rax)
	movq $5, 16(%rax)
	call __builtin_print_String
	jmp __label__00005
__label__00004:
	movl $0, %eax
	movq %rax, -8(%rbp)
__label__00005:
	movq -8(%rbp), %rax
	call __builtin_print_int
	xorl %eax, %eax
	movq %rax, -16(%rbp)
	movl $0, %eax
	movq %rax, -8(%rbp)
__label__00006:
	movq -8(%rbp), %rax
	pushq %rax
	movl $5, %eax
	popq %r9
	cmpl %eax, %r9d
	jl __label__00012
	xorl %eax, %eax
__label__00012:
	cmpl $0, %eax
	je __label__00007
	movq -8(%rbp), %rax
	pushq %rax
	movl $4, %eax
	popq %r9
	cmpl %eax, %r9d
	jg __label__00008
	movl $0, %eax
__label__00008:
	cmpl $0, %eax
	je __label__00011
	movq -16(%rbp), %rax
	cmpl $0, %eax
	jne __label__00009
	movl $1, %eax
	jmp __label__00010
__label__00009:
	xorl %eax, %eax
__label__00010:
	movq %rax, -16(%rbp)
__label__00011:
	movq -8(%rbp), %rax
	addl $1, %eax
	movq %rax, -8(%rbp)
	subl $1, %eax
	jmp __label__00006
__label__00007:
	movl $1, %eax
	movq %rax, -24(%rbp)
	movq %rax, -32(%rbp)
	movq -16(%rbp), %rax
	call __builtin_print_boolean
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
__str__label__00003:
	.string "i = 0"
#Définitions des constantes
__str_true:
	.string "true"
__str_false:
	.string "false"
__str_null:
	.string "null"
__str_d:
	.string "%d\n"
__null_error_msg:
	.string "null pointer exception\n"
__div0_error_msg:
	.string "division by 0 exception\n"
