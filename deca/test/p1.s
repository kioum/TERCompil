.text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	addq $0, %rsp
	movl $1, %eax
	movq %rax, -8(%rbp)
	movl $1, %eax
	movq %rax, -16(%rbp)
	movl $1, %eax
	movq %rax, -24(%rbp)
	movl $1, %eax
	movq %rax, -32(%rbp)
	movq -16(%rbp), %rax
	addl $1, %eax
	movq %rax, -16(%rbp)
	call __builtin_print_int
	movq $24, %rdi
	call malloc
	movq $1234, 0(%rax)
	movq $__str__label__00001, 8(%rax)
	movq $1, 16(%rax)
	call __builtin_print_String
	movq -16(%rbp), %rax
	call __builtin_print_int
	movq $24, %rdi
	call malloc
	movq $1234, 0(%rax)
	movq $__str__label__00002, 8(%rax)
	movq $2, 16(%rax)
	call __builtin_print_String
	movq -24(%rbp), %rax
	subl $1, %eax
	movq %rax, -24(%rbp)
	addl $1, %eax
	call __builtin_print_int
	movq $24, %rdi
	call malloc
	movq $1234, 0(%rax)
	movq $__str__label__00001, 8(%rax)
	movq $1, 16(%rax)
	call __builtin_print_String
	movq -24(%rbp), %rax
	call __builtin_print_int
	movq $24, %rdi
	call malloc
	movq $1234, 0(%rax)
	movq $__str__label__00002, 8(%rax)
	movq $2, 16(%rax)
	call __builtin_print_String
	movq -32(%rbp), %rax
	subl $1, %eax
	movq %rax, -32(%rbp)
	call __builtin_print_int
	movq $24, %rdi
	call malloc
	movq $1234, 0(%rax)
	movq $__str__label__00001, 8(%rax)
	movq $1, 16(%rax)
	call __builtin_print_String
	movq -32(%rbp), %rax
	call __builtin_print_int
	movq $24, %rdi
	call malloc
	movq $1234, 0(%rax)
	movq $__str__label__00002, 8(%rax)
	movq $2, 16(%rax)
	call __builtin_print_String
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
__str__label__00002:
	.string " \n"
__str__label__00001:
	.string " "
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
