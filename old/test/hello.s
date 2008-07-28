        .org  0
        .global _start
_start:

        mov $4, %eax
        mov $1, %ebx
        mov $data, %ecx
        mov $14, %edx
        int $128
        ret 
data:
        .ascii "Hello, world!\n"
