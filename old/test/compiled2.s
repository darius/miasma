        .org  1048576
        .global _start
_start:

        mov $.all-done-354, %ebp
        push $1
        push $.literal-355
        push $14
        call write
        movl %ecx, %eax
        jmpl %ebp
.all-done-354:
        ret 
.literal-355:
        asciiz "Hello, world!\n"
write:
        push %ebp
        movl %esp, %ebp
        push %eax
        push %ebx
        push %edx
        mov $4, %eax
        movl (%ebp), %ebx
        movl (%ebp), %ecx
        movl (%ebp), %edx
        int $128
        movl %eax, %ecx
        pop %edx
        pop %ebx
        pop %eax
        pop %ebp
        ret $12
read:
        push %ebp
        movl %esp, %ebp
        push %eax
        push %ebx
        push %edx
        mov $3, %eax
        movl (%ebp), %ebx
        movl (%ebp), %ecx
        movl (%ebp), %edx
        int $128
        movl %eax, %ecx
        pop %edx
        pop %ebx
        pop %eax
        pop %ebp
        ret $12
sbrk:
        push %eax
        push %ebx
        push %edx
        mov $45, %eax
        movl (%ebp), %ebx
        movl (%ebp), %ecx
        movl (%ebp), %edx
        int $128
        movl %eax, %ecx
        pop %edx
        pop %ebx
        pop %eax
        ret $16
