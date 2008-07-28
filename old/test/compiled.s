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
        .ascii "Hello, world!\n\x0"
write:
        push %eax
        push %ebx
        push %edx
        mov $4, %eax
        movl (%esp), %ebx
        movl (%esp), %ecx
        movl (%esp), %edx
        int $128
        movl %eax, %ecx
        pop %edx
        pop %ebx
        pop %eax
        ret $16
