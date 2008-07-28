        .org  0
        .global _start
_start:

        mov $10, %esi
again:
        mov $4, %eax
        mov $1, %ebx
        mov $data, %ecx
        mov $14, %edx
        int $128
        dec %esi
        ja again
        ret 
data:
        .ascii "Hello, world!\n"
