        .org  0
        .global _start
_start:

        .ascii "Sample"
        asciiz "Sample"
        data 42
        aaa 
        aas 
        daa 
        das 
        aad 
        aam 
        adc $-128, %al
        adc $127, %al
        adc $-2147483648, %eax
        adc $2147483647, %eax
        adcb $-128, (2147483648)
        adcb $127, (2147483648)
        adcl $-2147483648, (2147483648)
        adcl $2147483647, (2147483648)
        adcl $-128, (2147483648)
        adcl $127, (2147483648)
        adcb %al, (2147483648)
        adcb %ah, (2147483648)
        adcb %bl, (2147483648)
        adcb %bh, (2147483648)
        adcb %cl, (2147483648)
        adcb %ch, (2147483648)
        adcb %dl, (2147483648)
        adcb %dh, (2147483648)
        adcl %eax, (2147483648)
        adcl %ebx, (2147483648)
        adcl %ecx, (2147483648)
        adcl %edx, (2147483648)
        adcl %esi, (2147483648)
        adcl %edi, (2147483648)
        adcl %ebp, (2147483648)
        adcl %esp, (2147483648)
        adcb (2147483648), %al
        adcb (2147483648), %ah
        adcb (2147483648), %bl
        adcb (2147483648), %bh
        adcb (2147483648), %cl
        adcb (2147483648), %ch
        adcb (2147483648), %dl
        adcb (2147483648), %dh
        adcl (2147483648), %eax
        adcl (2147483648), %ebx
        adcl (2147483648), %ecx
        adcl (2147483648), %edx
        adcl (2147483648), %esi
        adcl (2147483648), %edi
        adcl (2147483648), %ebp
        adcl (2147483648), %esp
        add $-128, %al
        add $127, %al
        add $-2147483648, %eax
        add $2147483647, %eax
        addb $-128, (2147483648)
        addb $127, (2147483648)
        addl $-2147483648, (2147483648)
        addl $2147483647, (2147483648)
        addl $-128, (2147483648)
        addl $127, (2147483648)
        addb %al, (2147483648)
        addb %ah, (2147483648)
        addb %bl, (2147483648)
        addb %bh, (2147483648)
        addb %cl, (2147483648)
        addb %ch, (2147483648)
        addb %dl, (2147483648)
        addb %dh, (2147483648)
        addl %eax, (2147483648)
        addl %ebx, (2147483648)
        addl %ecx, (2147483648)
        addl %edx, (2147483648)
        addl %esi, (2147483648)
        addl %edi, (2147483648)
        addl %ebp, (2147483648)
        addl %esp, (2147483648)
        addb (2147483648), %al
        addb (2147483648), %ah
        addb (2147483648), %bl
        addb (2147483648), %bh
        addb (2147483648), %cl
        addb (2147483648), %ch
        addb (2147483648), %dl
        addb (2147483648), %dh
        addl (2147483648), %eax
        addl (2147483648), %ebx
        addl (2147483648), %ecx
        addl (2147483648), %edx
        addl (2147483648), %esi
        addl (2147483648), %edi
        addl (2147483648), %ebp
        addl (2147483648), %esp
        sbb $-128, %al
        sbb $127, %al
        sbb $-2147483648, %eax
        sbb $2147483647, %eax
        sbbb $-128, (2147483648)
        sbbb $127, (2147483648)
        sbbl $-2147483648, (2147483648)
        sbbl $2147483647, (2147483648)
        sbbl $-128, (2147483648)
        sbbl $127, (2147483648)
        sbbb %al, (2147483648)
        sbbb %ah, (2147483648)
        sbbb %bl, (2147483648)
        sbbb %bh, (2147483648)
        sbbb %cl, (2147483648)
        sbbb %ch, (2147483648)
        sbbb %dl, (2147483648)
        sbbb %dh, (2147483648)
        sbbl %eax, (2147483648)
        sbbl %ebx, (2147483648)
        sbbl %ecx, (2147483648)
        sbbl %edx, (2147483648)
        sbbl %esi, (2147483648)
        sbbl %edi, (2147483648)
        sbbl %ebp, (2147483648)
        sbbl %esp, (2147483648)
        sbbb (2147483648), %al
        sbbb (2147483648), %ah
        sbbb (2147483648), %bl
        sbbb (2147483648), %bh
        sbbb (2147483648), %cl
        sbbb (2147483648), %ch
        sbbb (2147483648), %dl
        sbbb (2147483648), %dh
        sbbl (2147483648), %eax
        sbbl (2147483648), %ebx
        sbbl (2147483648), %ecx
        sbbl (2147483648), %edx
        sbbl (2147483648), %esi
        sbbl (2147483648), %edi
        sbbl (2147483648), %ebp
        sbbl (2147483648), %esp
        sub $-128, %al
        sub $127, %al
        sub $-2147483648, %eax
        sub $2147483647, %eax
        subb $-128, (2147483648)
        subb $127, (2147483648)
        subl $-2147483648, (2147483648)
        subl $2147483647, (2147483648)
        subl $-128, (2147483648)
        subl $127, (2147483648)
        subb %al, (2147483648)
        subb %ah, (2147483648)
        subb %bl, (2147483648)
        subb %bh, (2147483648)
        subb %cl, (2147483648)
        subb %ch, (2147483648)
        subb %dl, (2147483648)
        subb %dh, (2147483648)
        subl %eax, (2147483648)
        subl %ebx, (2147483648)
        subl %ecx, (2147483648)
        subl %edx, (2147483648)
        subl %esi, (2147483648)
        subl %edi, (2147483648)
        subl %ebp, (2147483648)
        subl %esp, (2147483648)
        subb (2147483648), %al
        subb (2147483648), %ah
        subb (2147483648), %bl
        subb (2147483648), %bh
        subb (2147483648), %cl
        subb (2147483648), %ch
        subb (2147483648), %dl
        subb (2147483648), %dh
        subl (2147483648), %eax
        subl (2147483648), %ebx
        subl (2147483648), %ecx
        subl (2147483648), %edx
        subl (2147483648), %esi
        subl (2147483648), %edi
        subl (2147483648), %ebp
        subl (2147483648), %esp
        and $-128, %al
        and $127, %al
        and $-2147483648, %eax
        and $2147483647, %eax
        andb $-128, (2147483648)
        andb $127, (2147483648)
        andl $-2147483648, (2147483648)
        andl $2147483647, (2147483648)
        andl $-128, (2147483648)
        andl $127, (2147483648)
        and $128, %al
        and $255, %al
        and $2147483648, %eax
        and $4294967295, %eax
        andb $128, (2147483648)
        andb $255, (2147483648)
        andl $2147483648, (2147483648)
        andl $4294967295, (2147483648)
        andl $128, (2147483648)
        andl $255, (2147483648)
        andb %al, (2147483648)
        andb %ah, (2147483648)
        andb %bl, (2147483648)
        andb %bh, (2147483648)
        andb %cl, (2147483648)
        andb %ch, (2147483648)
        andb %dl, (2147483648)
        andb %dh, (2147483648)
        andl %eax, (2147483648)
        andl %ebx, (2147483648)
        andl %ecx, (2147483648)
        andl %edx, (2147483648)
        andl %esi, (2147483648)
        andl %edi, (2147483648)
        andl %ebp, (2147483648)
        andl %esp, (2147483648)
        andb (2147483648), %al
        andb (2147483648), %ah
        andb (2147483648), %bl
        andb (2147483648), %bh
        andb (2147483648), %cl
        andb (2147483648), %ch
        andb (2147483648), %dl
        andb (2147483648), %dh
        andl (2147483648), %eax
        andl (2147483648), %ebx
        andl (2147483648), %ecx
        andl (2147483648), %edx
        andl (2147483648), %esi
        andl (2147483648), %edi
        andl (2147483648), %ebp
        andl (2147483648), %esp
        or $-128, %al
        or $127, %al
        or $-2147483648, %eax
        or $2147483647, %eax
        orb $-128, (2147483648)
        orb $127, (2147483648)
        orl $-2147483648, (2147483648)
        orl $2147483647, (2147483648)
        orl $-128, (2147483648)
        orl $127, (2147483648)
        or $128, %al
        or $255, %al
        or $2147483648, %eax
        or $4294967295, %eax
        orb $128, (2147483648)
        orb $255, (2147483648)
        orl $2147483648, (2147483648)
        orl $4294967295, (2147483648)
        orl $128, (2147483648)
        orl $255, (2147483648)
        orb %al, (2147483648)
        orb %ah, (2147483648)
        orb %bl, (2147483648)
        orb %bh, (2147483648)
        orb %cl, (2147483648)
        orb %ch, (2147483648)
        orb %dl, (2147483648)
        orb %dh, (2147483648)
        orl %eax, (2147483648)
        orl %ebx, (2147483648)
        orl %ecx, (2147483648)
        orl %edx, (2147483648)
        orl %esi, (2147483648)
        orl %edi, (2147483648)
        orl %ebp, (2147483648)
        orl %esp, (2147483648)
        orb (2147483648), %al
        orb (2147483648), %ah
        orb (2147483648), %bl
        orb (2147483648), %bh
        orb (2147483648), %cl
        orb (2147483648), %ch
        orb (2147483648), %dl
        orb (2147483648), %dh
        orl (2147483648), %eax
        orl (2147483648), %ebx
        orl (2147483648), %ecx
        orl (2147483648), %edx
        orl (2147483648), %esi
        orl (2147483648), %edi
        orl (2147483648), %ebp
        orl (2147483648), %esp
        xor $-128, %al
        xor $127, %al
        xor $-2147483648, %eax
        xor $2147483647, %eax
        xorb $-128, (2147483648)
        xorb $127, (2147483648)
        xorl $-2147483648, (2147483648)
        xorl $2147483647, (2147483648)
        xorl $-128, (2147483648)
        xorl $127, (2147483648)
        xorb %al, (2147483648)
        xorb %ah, (2147483648)
        xorb %bl, (2147483648)
        xorb %bh, (2147483648)
        xorb %cl, (2147483648)
        xorb %ch, (2147483648)
        xorb %dl, (2147483648)
        xorb %dh, (2147483648)
        xorl %eax, (2147483648)
        xorl %ebx, (2147483648)
        xorl %ecx, (2147483648)
        xorl %edx, (2147483648)
        xorl %esi, (2147483648)
        xorl %edi, (2147483648)
        xorl %ebp, (2147483648)
        xorl %esp, (2147483648)
        xorb (2147483648), %al
        xorb (2147483648), %ah
        xorb (2147483648), %bl
        xorb (2147483648), %bh
        xorb (2147483648), %cl
        xorb (2147483648), %ch
        xorb (2147483648), %dl
        xorb (2147483648), %dh
        xorl (2147483648), %eax
        xorl (2147483648), %ebx
        xorl (2147483648), %ecx
        xorl (2147483648), %edx
        xorl (2147483648), %esi
        xorl (2147483648), %edi
        xorl (2147483648), %ebp
        xorl (2147483648), %esp
        test $-128, %al
        test $127, %al
        test $-2147483648, %eax
        test $2147483647, %eax
        testb $-128, (2147483648)
        testb $127, (2147483648)
        testl $-2147483648, (2147483648)
        testl $2147483647, (2147483648)
        testb %al, (2147483648)
        testb %ah, (2147483648)
        testb %bl, (2147483648)
        testb %bh, (2147483648)
        testb %cl, (2147483648)
        testb %ch, (2147483648)
        testb %dl, (2147483648)
        testb %dh, (2147483648)
        testl %eax, (2147483648)
        testl %ebx, (2147483648)
        testl %ecx, (2147483648)
        testl %edx, (2147483648)
        testl %esi, (2147483648)
        testl %edi, (2147483648)
        testl %ebp, (2147483648)
        testl %esp, (2147483648)
        arplw %ax, (2147483648)
        arplw %bx, (2147483648)
        arplw %cx, (2147483648)
        arplw %dx, (2147483648)
        arplw %si, (2147483648)
        arplw %di, (2147483648)
        arplw %bp, (2147483648)
        arplw %sp, (2147483648)
        boundl (2147483648), %eax
        boundl (2147483648), %ebx
        boundl (2147483648), %ecx
        boundl (2147483648), %edx
        boundl (2147483648), %esi
        boundl (2147483648), %edi
        boundl (2147483648), %ebp
        boundl (2147483648), %esp
        bsfl (2147483648), %eax
        bsfl (2147483648), %ebx
        bsfl (2147483648), %ecx
        bsfl (2147483648), %edx
        bsfl (2147483648), %esi
        bsfl (2147483648), %edi
        bsfl (2147483648), %ebp
        bsfl (2147483648), %esp
        bsrl (2147483648), %eax
        bsrl (2147483648), %ebx
        bsrl (2147483648), %ecx
        bsrl (2147483648), %edx
        bsrl (2147483648), %esi
        bsrl (2147483648), %edi
        bsrl (2147483648), %ebp
        bsrl (2147483648), %esp
        btl %eax, (2147483648)
        btl %ebx, (2147483648)
        btl %ecx, (2147483648)
        btl %edx, (2147483648)
        btl %esi, (2147483648)
        btl %edi, (2147483648)
        btl %ebp, (2147483648)
        btl %esp, (2147483648)
        btl $128, (2147483648)
        btl $255, (2147483648)
        btcl %eax, (2147483648)
        btcl %ebx, (2147483648)
        btcl %ecx, (2147483648)
        btcl %edx, (2147483648)
        btcl %esi, (2147483648)
        btcl %edi, (2147483648)
        btcl %ebp, (2147483648)
        btcl %esp, (2147483648)
        btcl $128, (2147483648)
        btcl $255, (2147483648)
        btrl %eax, (2147483648)
        btrl %ebx, (2147483648)
        btrl %ecx, (2147483648)
        btrl %edx, (2147483648)
        btrl %esi, (2147483648)
        btrl %edi, (2147483648)
        btrl %ebp, (2147483648)
        btrl %esp, (2147483648)
        btrl $128, (2147483648)
        btrl $255, (2147483648)
        btsl %eax, (2147483648)
        btsl %ebx, (2147483648)
        btsl %ecx, (2147483648)
        btsl %edx, (2147483648)
        btsl %esi, (2147483648)
        btsl %edi, (2147483648)
        btsl %ebp, (2147483648)
        btsl %esp, (2147483648)
        btsl $128, (2147483648)
        btsl $255, (2147483648)
        calll (2147483648)
        callfl (2147483648)
        cwde 
        cdq 
        clc 
        cld 
        cli 
        stc 
        std 
        sti 
        cmc 
        clts 
        cmp $-2147483648, %eax
        cmp $2147483647, %eax
        cmp $-128, %al
        cmp $127, %al
        cmp $2147483648, %eax
        cmp $4294967295, %eax
        cmp $128, %al
        cmp $255, %al
        cmpb $-128, (2147483648)
        cmpb $127, (2147483648)
        cmpl $-2147483648, (2147483648)
        cmpl $2147483647, (2147483648)
        cmpl $-128, (2147483648)
        cmpl $127, (2147483648)
        cmpb $128, (2147483648)
        cmpb $255, (2147483648)
        cmpl $2147483648, (2147483648)
        cmpl $4294967295, (2147483648)
        cmpl $128, (2147483648)
        cmpl $255, (2147483648)
        cmpb %al, (2147483648)
        cmpb %ah, (2147483648)
        cmpb %bl, (2147483648)
        cmpb %bh, (2147483648)
        cmpb %cl, (2147483648)
        cmpb %ch, (2147483648)
        cmpb %dl, (2147483648)
        cmpb %dh, (2147483648)
        cmpl %eax, (2147483648)
        cmpl %ebx, (2147483648)
        cmpl %ecx, (2147483648)
        cmpl %edx, (2147483648)
        cmpl %esi, (2147483648)
        cmpl %edi, (2147483648)
        cmpl %ebp, (2147483648)
        cmpl %esp, (2147483648)
        cmpb (2147483648), %al
        cmpb (2147483648), %ah
        cmpb (2147483648), %bl
        cmpb (2147483648), %bh
        cmpb (2147483648), %cl
        cmpb (2147483648), %ch
        cmpb (2147483648), %dl
        cmpb (2147483648), %dh
        cmpl (2147483648), %eax
        cmpl (2147483648), %ebx
        cmpl (2147483648), %ecx
        cmpl (2147483648), %edx
        cmpl (2147483648), %esi
        cmpl (2147483648), %edi
        cmpl (2147483648), %ebp
        cmpl (2147483648), %esp
        cmpsb 
        cmpsl 
        cmpxchgb %al, (2147483648)
        cmpxchgb %ah, (2147483648)
        cmpxchgb %bl, (2147483648)
        cmpxchgb %bh, (2147483648)
        cmpxchgb %cl, (2147483648)
        cmpxchgb %ch, (2147483648)
        cmpxchgb %dl, (2147483648)
        cmpxchgb %dh, (2147483648)
        cmpxchgl %eax, (2147483648)
        cmpxchgl %ebx, (2147483648)
        cmpxchgl %ecx, (2147483648)
        cmpxchgl %edx, (2147483648)
        cmpxchgl %esi, (2147483648)
        cmpxchgl %edi, (2147483648)
        cmpxchgl %ebp, (2147483648)
        cmpxchgl %esp, (2147483648)
        cmpxchg8bl (2147483648)
        cpuid 
        dec %ebx
        dec %ecx
        dec %edx
        dec %esi
        dec %edi
        dec %ebp
        dec %esp
        decb (2147483648)
        decl (2147483648)
        divb (2147483648)
        divl (2147483648)
        enter $32768, $128
        enter $32768, $255
        enter $65535, $128
        enter $65535, $255
        hlt 
        idivb (2147483648)
        idivl (2147483648)
        imulb (2147483648)
        imull (2147483648)
        imull (2147483648), %eax
        imull (2147483648), %ebx
        imull (2147483648), %ecx
        imull (2147483648), %edx
        imull (2147483648), %esi
        imull (2147483648), %edi
        imull (2147483648), %ebp
        imull (2147483648), %esp
        imull $-2147483648, (2147483648), %eax
        imull $2147483647, (2147483648), %eax
        imull $-2147483648, (2147483648), %ebx
        imull $2147483647, (2147483648), %ebx
        imull $-2147483648, (2147483648), %ecx
        imull $2147483647, (2147483648), %ecx
        imull $-2147483648, (2147483648), %edx
        imull $2147483647, (2147483648), %edx
        imull $-2147483648, (2147483648), %esi
        imull $2147483647, (2147483648), %esi
        imull $-2147483648, (2147483648), %edi
        imull $2147483647, (2147483648), %edi
        imull $-2147483648, (2147483648), %ebp
        imull $2147483647, (2147483648), %ebp
        imull $-2147483648, (2147483648), %esp
        imull $2147483647, (2147483648), %esp
        imull $-128, (2147483648), %eax
        imull $127, (2147483648), %eax
        imull $-128, (2147483648), %ebx
        imull $127, (2147483648), %ebx
        imull $-128, (2147483648), %ecx
        imull $127, (2147483648), %ecx
        imull $-128, (2147483648), %edx
        imull $127, (2147483648), %edx
        imull $-128, (2147483648), %esi
        imull $127, (2147483648), %esi
        imull $-128, (2147483648), %edi
        imull $127, (2147483648), %edi
        imull $-128, (2147483648), %ebp
        imull $127, (2147483648), %ebp
        imull $-128, (2147483648), %esp
        imull $127, (2147483648), %esp
        in $128, %al
        in $255, %al
        in $128, %eax
        in $255, %eax
        in %dx, %al
        in %dx, %eax
        out %al, $128
        out %al, $255
        out %eax, $128
        out %eax, $255
        inc %ebx
        inc %ecx
        inc %edx
        inc %esi
        inc %edi
        inc %ebp
        inc %esp
        incb (2147483648)
        incl (2147483648)
        insb 
        insl 
        outsb 
        outsl 
        int $128
        int $255
        int $3 
        into 
        iret 
        jmpl (2147483648)
        jmpfl (2147483648)
        lahf 
        larl (2147483648), %eax
        larl (2147483648), %ebx
        larl (2147483648), %ecx
        larl (2147483648), %edx
        larl (2147483648), %esi
        larl (2147483648), %edi
        larl (2147483648), %ebp
        larl (2147483648), %esp
        ldsl (2147483648), %eax
        ldsl (2147483648), %ebx
        ldsl (2147483648), %ecx
        ldsl (2147483648), %edx
        ldsl (2147483648), %esi
        ldsl (2147483648), %edi
        ldsl (2147483648), %ebp
        ldsl (2147483648), %esp
        lesl (2147483648), %eax
        lesl (2147483648), %ebx
        lesl (2147483648), %ecx
        lesl (2147483648), %edx
        lesl (2147483648), %esi
        lesl (2147483648), %edi
        lesl (2147483648), %ebp
        lesl (2147483648), %esp
        lssl (2147483648), %eax
        lssl (2147483648), %ebx
        lssl (2147483648), %ecx
        lssl (2147483648), %edx
        lssl (2147483648), %esi
        lssl (2147483648), %edi
        lssl (2147483648), %ebp
        lssl (2147483648), %esp
        lfsl (2147483648), %eax
        lfsl (2147483648), %ebx
        lfsl (2147483648), %ecx
        lfsl (2147483648), %edx
        lfsl (2147483648), %esi
        lfsl (2147483648), %edi
        lfsl (2147483648), %ebp
        lfsl (2147483648), %esp
        lgsl (2147483648), %eax
        lgsl (2147483648), %ebx
        lgsl (2147483648), %ecx
        lgsl (2147483648), %edx
        lgsl (2147483648), %esi
        lgsl (2147483648), %edi
        lgsl (2147483648), %ebp
        lgsl (2147483648), %esp
        leal (2147483648), %eax
        leal (2147483648), %ebx
        leal (2147483648), %ecx
        leal (2147483648), %edx
        leal (2147483648), %esi
        leal (2147483648), %edi
        leal (2147483648), %ebp
        leal (2147483648), %esp
        leave 
        lgdtl (2147483648)
        lidtl (2147483648)
        sgdtl (2147483648)
        sidtl (2147483648)
        lldtw (2147483648)
        sldtl (2147483648)
        lmsww (2147483648)
        smswl (2147483648)
        lodsb 
        lodsl 
        lsll (2147483648), %eax
        lsll (2147483648), %ebx
        lsll (2147483648), %ecx
        lsll (2147483648), %edx
        lsll (2147483648), %esi
        lsll (2147483648), %edi
        lsll (2147483648), %ebp
        lsll (2147483648), %esp
        ltrw (2147483648)
        strw (2147483648)
        movb %al, (2147483648)
        movb %ah, (2147483648)
        movb %bl, (2147483648)
        movb %bh, (2147483648)
        movb %cl, (2147483648)
        movb %ch, (2147483648)
        movb %dl, (2147483648)
        movb %dh, (2147483648)
        movl %eax, (2147483648)
        movl %ebx, (2147483648)
        movl %ecx, (2147483648)
        movl %edx, (2147483648)
        movl %esi, (2147483648)
        movl %edi, (2147483648)
        movl %ebp, (2147483648)
        movl %esp, (2147483648)
        movb (2147483648), %al
        movb (2147483648), %ah
        movb (2147483648), %bl
        movb (2147483648), %bh
        movb (2147483648), %cl
        movb (2147483648), %ch
        movb (2147483648), %dl
        movb (2147483648), %dh
        movl (2147483648), %eax
        movl (2147483648), %ebx
        movl (2147483648), %ecx
        movl (2147483648), %edx
        movl (2147483648), %esi
        movl (2147483648), %edi
        movl (2147483648), %ebp
        movl (2147483648), %esp
        mov $-128, %ah
        mov $127, %ah
        mov $-128, %bl
        mov $127, %bl
        mov $-128, %bh
        mov $127, %bh
        mov $-128, %cl
        mov $127, %cl
        mov $-128, %ch
        mov $127, %ch
        mov $-128, %dl
        mov $127, %dl
        mov $-128, %dh
        mov $127, %dh
        mov $-2147483648, %ebx
        mov $2147483647, %ebx
        mov $-2147483648, %ecx
        mov $2147483647, %ecx
        mov $-2147483648, %edx
        mov $2147483647, %edx
        mov $-2147483648, %esi
        mov $2147483647, %esi
        mov $-2147483648, %edi
        mov $2147483647, %edi
        mov $-2147483648, %ebp
        mov $2147483647, %ebp
        mov $-2147483648, %esp
        mov $2147483647, %esp
        movb $-128, (2147483648)
        movb $127, (2147483648)
        movl $-2147483648, (2147483648)
        movl $2147483647, (2147483648)
        mov $128, %ah
        mov $255, %ah
        mov $128, %bl
        mov $255, %bl
        mov $128, %bh
        mov $255, %bh
        mov $128, %cl
        mov $255, %cl
        mov $128, %ch
        mov $255, %ch
        mov $128, %dl
        mov $255, %dl
        mov $128, %dh
        mov $255, %dh
        mov $2147483648, %ebx
        mov $4294967295, %ebx
        mov $2147483648, %ecx
        mov $4294967295, %ecx
        mov $2147483648, %edx
        mov $4294967295, %edx
        mov $2147483648, %esi
        mov $4294967295, %esi
        mov $2147483648, %edi
        mov $4294967295, %edi
        mov $2147483648, %ebp
        mov $4294967295, %ebp
        mov $2147483648, %esp
        mov $4294967295, %esp
        movb $128, (2147483648)
        movb $255, (2147483648)
        movl $2147483648, (2147483648)
        movl $4294967295, (2147483648)
        movsb 
        movsl 
        movsxl (2147483648), %eax
        movsxl (2147483648), %ebx
        movsxl (2147483648), %ecx
        movsxl (2147483648), %edx
        movsxl (2147483648), %esi
        movsxl (2147483648), %edi
        movsxl (2147483648), %ebp
        movsxl (2147483648), %esp
        movsxl (2147483648), %eax
        movsxl (2147483648), %ebx
        movsxl (2147483648), %ecx
        movsxl (2147483648), %edx
        movsxl (2147483648), %esi
        movsxl (2147483648), %edi
        movsxl (2147483648), %ebp
        movsxl (2147483648), %esp
        movzxl (2147483648), %eax
        movzxl (2147483648), %ebx
        movzxl (2147483648), %ecx
        movzxl (2147483648), %edx
        movzxl (2147483648), %esi
        movzxl (2147483648), %edi
        movzxl (2147483648), %ebp
        movzxl (2147483648), %esp
        movzxl (2147483648), %eax
        movzxl (2147483648), %ebx
        movzxl (2147483648), %ecx
        movzxl (2147483648), %edx
        movzxl (2147483648), %esi
        movzxl (2147483648), %edi
        movzxl (2147483648), %ebp
        movzxl (2147483648), %esp
        mulb (2147483648)
        mull (2147483648)
        negb (2147483648)
        negl (2147483648)
        nop 
        notb (2147483648)
        notl (2147483648)
        pop %ebx
        pop %ecx
        pop %edx
        pop %esi
        pop %edi
        pop %ebp
        pop %esp
        popl (2147483648)
        pop %ds
        pop %es
        pop %ss
        pop %fs
        pop %gs
        popal 
        popfl 
        push %ebx
        push %ecx
        push %edx
        push %esi
        push %edi
        push %ebp
        push %esp
        push $-128
        push $127
        push $-2147483648
        push $2147483647
        push %cs
        push %ds
        push %es
        push %ss
        push %fs
        push %gs
        pushl (2147483648)
        pushal 
        pushfl 
        rclb (2147483648)
        rcll (2147483648)
        rclb %cl, (2147483648)
        rcll %cl, (2147483648)
        rclb $128, (2147483648)
        rclb $255, (2147483648)
        rcll $128, (2147483648)
        rcll $255, (2147483648)
        rcrb (2147483648)
        rcrl (2147483648)
        rcrb %cl, (2147483648)
        rcrl %cl, (2147483648)
        rcrb $128, (2147483648)
        rcrb $255, (2147483648)
        rcrl $128, (2147483648)
        rcrl $255, (2147483648)
        rolb (2147483648)
        roll (2147483648)
        rolb %cl, (2147483648)
        roll %cl, (2147483648)
        rolb $128, (2147483648)
        rolb $255, (2147483648)
        roll $128, (2147483648)
        roll $255, (2147483648)
        rorb (2147483648)
        rorl (2147483648)
        rorb %cl, (2147483648)
        rorl %cl, (2147483648)
        rorb $128, (2147483648)
        rorb $255, (2147483648)
        rorl $128, (2147483648)
        rorl $255, (2147483648)
        salb (2147483648)
        sall (2147483648)
        salb %cl, (2147483648)
        sall %cl, (2147483648)
        salb $128, (2147483648)
        salb $255, (2147483648)
        sall $128, (2147483648)
        sall $255, (2147483648)
        shlb (2147483648)
        shll (2147483648)
        shlb %cl, (2147483648)
        shll %cl, (2147483648)
        shlb $128, (2147483648)
        shlb $255, (2147483648)
        shll $128, (2147483648)
        shll $255, (2147483648)
        sarb (2147483648)
        sarl (2147483648)
        sarb %cl, (2147483648)
        sarl %cl, (2147483648)
        sarb $128, (2147483648)
        sarb $255, (2147483648)
        sarl $128, (2147483648)
        sarl $255, (2147483648)
        shrb (2147483648)
        shrl (2147483648)
        shrb %cl, (2147483648)
        shrl %cl, (2147483648)
        shrb $128, (2147483648)
        shrb $255, (2147483648)
        shrl $128, (2147483648)
        shrl $255, (2147483648)
        ret $-32768
        ret $32767
        ret 
        lret $-32768
        lret $32767
        lret 
        rsm 
        sahf 
        scasb 
        scasl 
        setob (2147483648)
        setnob (2147483648)
        setcb (2147483648)
        setbb (2147483648)
        setnaeb (2147483648)
        setaeb (2147483648)
        setnbb (2147483648)
        setncb (2147483648)
        seteb (2147483648)
        setzb (2147483648)
        setneb (2147483648)
        setnzb (2147483648)
        setbeb (2147483648)
        setnab (2147483648)
        setab (2147483648)
        setnbeb (2147483648)
        setsb (2147483648)
        setnsb (2147483648)
        setpb (2147483648)
        setpeb (2147483648)
        setpob (2147483648)
        setnpb (2147483648)
        setlb (2147483648)
        setngeb (2147483648)
        setgeb (2147483648)
        setnlb (2147483648)
        setleb (2147483648)
        setngb (2147483648)
        setgb (2147483648)
        setnleb (2147483648)
        shldl $128, %eax, (2147483648)
        shldl $255, %eax, (2147483648)
        shldl $128, %ebx, (2147483648)
        shldl $255, %ebx, (2147483648)
        shldl $128, %ecx, (2147483648)
        shldl $255, %ecx, (2147483648)
        shldl $128, %edx, (2147483648)
        shldl $255, %edx, (2147483648)
        shldl $128, %esi, (2147483648)
        shldl $255, %esi, (2147483648)
        shldl $128, %edi, (2147483648)
        shldl $255, %edi, (2147483648)
        shldl $128, %ebp, (2147483648)
        shldl $255, %ebp, (2147483648)
        shldl $128, %esp, (2147483648)
        shldl $255, %esp, (2147483648)
        shldl %cl, %eax, (2147483648)
        shldl %cl, %ebx, (2147483648)
        shldl %cl, %ecx, (2147483648)
        shldl %cl, %edx, (2147483648)
        shldl %cl, %esi, (2147483648)
        shldl %cl, %edi, (2147483648)
        shldl %cl, %ebp, (2147483648)
        shldl %cl, %esp, (2147483648)
        shrdl $128, %eax, (2147483648)
        shrdl $255, %eax, (2147483648)
        shrdl $128, %ebx, (2147483648)
        shrdl $255, %ebx, (2147483648)
        shrdl $128, %ecx, (2147483648)
        shrdl $255, %ecx, (2147483648)
        shrdl $128, %edx, (2147483648)
        shrdl $255, %edx, (2147483648)
        shrdl $128, %esi, (2147483648)
        shrdl $255, %esi, (2147483648)
        shrdl $128, %edi, (2147483648)
        shrdl $255, %edi, (2147483648)
        shrdl $128, %ebp, (2147483648)
        shrdl $255, %ebp, (2147483648)
        shrdl $128, %esp, (2147483648)
        shrdl $255, %esp, (2147483648)
        shrdl %cl, %eax, (2147483648)
        shrdl %cl, %ebx, (2147483648)
        shrdl %cl, %ecx, (2147483648)
        shrdl %cl, %edx, (2147483648)
        shrdl %cl, %esi, (2147483648)
        shrdl %cl, %edi, (2147483648)
        shrdl %cl, %ebp, (2147483648)
        shrdl %cl, %esp, (2147483648)
        stosb 
        stosl 
        verrw (2147483648)
        verww (2147483648)
        xchg %eax, %ebx
        xchg %eax, %ecx
        xchg %eax, %edx
        xchg %eax, %esi
        xchg %eax, %edi
        xchg %eax, %ebp
        xchg %eax, %esp
        xchg %ebx, %eax
        xchg %ecx, %eax
        xchg %edx, %eax
        xchg %esi, %eax
        xchg %edi, %eax
        xchg %ebp, %eax
        xchg %esp, %eax
        xchgb (2147483648), %al
        xchgb (2147483648), %ah
        xchgb (2147483648), %bl
        xchgb (2147483648), %bh
        xchgb (2147483648), %cl
        xchgb (2147483648), %ch
        xchgb (2147483648), %dl
        xchgb (2147483648), %dh
        xchgb %al, (2147483648)
        xchgb %ah, (2147483648)
        xchgb %bl, (2147483648)
        xchgb %bh, (2147483648)
        xchgb %cl, (2147483648)
        xchgb %ch, (2147483648)
        xchgb %dl, (2147483648)
        xchgb %dh, (2147483648)
        xchgl (2147483648), %eax
        xchgl (2147483648), %ebx
        xchgl (2147483648), %ecx
        xchgl (2147483648), %edx
        xchgl (2147483648), %esi
        xchgl (2147483648), %edi
        xchgl (2147483648), %ebp
        xchgl (2147483648), %esp
        xchgl %eax, (2147483648)
        xchgl %ebx, (2147483648)
        xchgl %ecx, (2147483648)
        xchgl %edx, (2147483648)
        xchgl %esi, (2147483648)
        xchgl %edi, (2147483648)
        xchgl %ebp, (2147483648)
        xchgl %esp, (2147483648)
        xlatb 
